# modules/description_v2.R
# =============================================================================
# Description Module
# =============================================================================

# This module also currently loads and plots the the classification rasters
# ideally would like to separate the EAU/classification upload to a separate module,
# I've got a separate module, but can get the EAU to plot on the description map...... 

# TO DO:
# 1. separate out raster and roi parts
# 2. separate out the description classification part so that it loads an ROI quickly?
# 3. roi area draw/selection
# 6. height of cards


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
description_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Description",
    layout_column_wrap(
      width = 1/2,
      height = 600,
      
      # Ecosystem / ROI map ---------------------------------------------------
       card(
        full_screen = TRUE,
        card_header("Ecosystem location"),
        card_body(leafletOutput(ns("roi_map"), height = 500),class = "p-0")),
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        # Rainfall ------------------------------------------------------------
        card(
          full_screen = TRUE,
          card_header("Rainfall", tooltip(bs_icon("info-circle"),
                                          "Mean monthly rainfall (2010–2024 average)"
          )),
          card_body(plotlyOutput(ns("rain_graph")),  min_height = 150)
        ),
        
        # Temperature ---------------------------------------------------------
        card(
          full_screen = TRUE,
          card_header("Temperature", tooltip(bs_icon("info-circle"),
                                             "Mean monthly temperature (2010–2024 average)"
          )),
          card_body(plotlyOutput(ns("temp_graph")),  min_height = 150)
        ),
        
        # Elevation -----------------------------------------------------------
        card(
          full_screen = TRUE,
          card_header("Elevation", tooltip(bs_icon("info-circle"),
                                           "Elevation distribution within the ROI"
          )),
          card_body(plotlyOutput(ns("elev_graph")),  min_height = 150)
        )
      )),
      
    layout_column_wrap(
      width = 1/2,
      height = 300,
    
      # Country overlap -------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("ROI Country Overlap", tooltip(bs_icon("info-circle"),
                                                   "Proportion of the ROI overlapping each country"
        )),
        card_body(DT::dataTableOutput(ns("roi_country_table")))
      ),
      # Typology -------------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header(tags$a(
          href = "https://global-ecosystems.org/",
          "Global Ecosystem Typology",
          target = "_blank"
        )),
        card_body(shinycssloaders::withSpinner(DT::dataTableOutput(ns("get_table")), type=5))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
description_module_server <- function(id, data_module, tifs_input, year_inputs,
                                      process_btn, process_roi_btn, roi_input, countries) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Reactive values container
    rv <- reactiveValues(
      roi = NULL,
      roi_wgs = NULL,
      countries = NULL,
      rainfall = NULL,
      temperature = NULL,
      elevation = NULL,
      typology = NULL
    )
    
    # -------------------------------------------------------------------------
    # Leaflet base map
    output$roi_map <- renderLeaflet({
      leaflet() %>%
        addMapPane("rasterPane", zIndex = 410) %>%  # create persistent raster pane
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    # -------------------------------------------------------------------------
    # Load ROI shapefile
    load_roi <- function(roi_input) {
      req(roi_input)
      # Create a new temporary folder each time
      temp_dir <- tempfile(pattern = "roi_")
      dir.create(temp_dir)
      roi_files <- roi_input$datapath
      roi_names <- roi_input$name
      
      file.copy(from = roi_files,
                to = file.path(temp_dir, roi_names),
                overwrite = TRUE)
      
      shp <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      validate(need(length(shp) == 1, "No valid shapefile found"))
      
      terra::vect(shp)
    }
    
    # -------------------------------------------------------------------------
    # Update leaflet with ROI
    update_roi_map <- function(roi) {
      roi_wgs <- terra::project(roi, "EPSG:4326")
      centroid <- terra::centroids(roi_wgs)
      coords <- terra::crds(centroid)
      
      leafletProxy(ns("roi_map")) %>%
        # removeShape("ROI") %>%   # safer than clearShapes()
        clearGroup("ROI") %>%
        # clearGroup(group_name) %>%
        addPolygons(
          data = sf::st_as_sf(roi_wgs),
          fillColor = "grey80", fillOpacity = 0.2,
          color = "darkblue", weight = 2,
          group = "ROI",
          highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
        ) %>%
        setView(lng = coords[1], lat = coords[2], zoom = 6)
      
      # ✅ Re-add latest raster if exists
      if (!is.null(data_module$rasters())) {
        latest <- data_module$rasters()[[length(data_module$rasters())]]
        
        # extract name of the latest raster (should be the year, e.g. "2024")
        latest_year <- names(latest)
        if (is.null(latest_year) || latest_year == "") latest_year <- "Latest"
        
        # group_name <- paste0(latest_year, " classification")
        # 
        # leafletProxy(ns("roi_map")) %>%
        #   # clearGroup(group_name) %>%  # optional: prevent stacking rasters
        #   # clearGroup(paste0(names(r_list), " classification")) %>%  # remove any existing classification rasters
        #   addRasterImage(latest, opacity = 0.7, colors = "#000000", group = group_name)
        
        display_label <- paste0(latest_year, " classification")
        
        leafletProxy(ns("roi_map")) %>%
          clearGroup(display_label) %>%  # remove any existing classification rasters
          addRasterImage(latest, opacity = 0.7, colors = "#000000", group = display_label, layerId = display_label,
                         options = leaflet::pathOptions(pane = "rasterPane")) %>%
        # leafletProxy(ns("roi_map")) %>%
          addLayersControl(
            baseGroups = c("OSM", "ESRI Satellite"),
            overlayGroups = c(display_label, "ROI", "Elevation"), # , "Rainfall (10y avg.)"
            options = layersControlOptions(collapsed = FALSE))
        
        
        
      }
    }
    
    # -------------------------------------------------------------------------
    # Compute overlap with countries
    compute_country_overlap <- function(roi, countries) {
      validate(need(!is.null(countries), "Countries data not available"))
      validate(need(!is.null(roi), "ROI not loaded."))
      
      countries_proj <- terra::project(countries, "EPSG:4326")
      roi_proj <- terra::project(roi, "EPSG:4326")
      
      roi_int <- tryCatch({
        terra::intersect(countries_proj, roi_proj)
      }, error = function(e) {
        message("Error during intersect: ", e$message)
        NULL
      })
      
      if (is.null(roi_int) || nrow(roi_int) == 0) return(data.frame())
      
      roi_int$area_km2 <- terra::expanse(roi_int, unit = "km")
      total_area <- terra::expanse(roi_proj, unit = "km")
      
      as.data.frame(roi_int) %>%
        dplyr::group_by(admin) %>%
        dplyr::summarise(area_km2 = sum(area_km2), .groups = "drop") %>%
        dplyr::mutate(
          perc_roi = 100 * area_km2 / total_area,
          area_km2 = format(round(area_km2, 0), big.mark = ","),
          perc_roi = format(round(perc_roi, 1))
        ) %>%
        dplyr::arrange(desc(perc_roi)) %>%
        dplyr::rename(Country = admin, `Area (km²)` = area_km2, `% of ROI` = perc_roi)
    }
    
    # -------------------------------------------------------------------------
    # Compute rainfall
    compute_rainfall <- function(roi) {

      # Load and crop TerraClim rainfall
      roi_rast <- getTerraClim(roi, varname = "ppt",
                               startDate = "2010-01-01",
                               endDate = "2024-12-31") |>
        {\(x) crop(x$ppt, roi) |> mask(roi)}()

      # Extract dates and months
      dates <- as.Date(gsub("ppt_|_total", "", names(roi_rast)))
      months <- lubridate::month(dates, label = TRUE)

      # Compute mean, min, max per layer
      layer_stats <- terra::global(roi_rast, c("mean", "range"), na.rm = TRUE) |> as.data.frame()
      layer_stats$date <- dates
      layer_stats$month <- months
      layer_stats$month_num <- as.numeric(months)  # numeric for loess smoothing

      # Aggregate by month
      monthly_avg <- layer_stats %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          mean = mean(mean, na.rm = TRUE),
          min = min(min, na.rm = TRUE),
          max = max(max, na.rm = TRUE)
        ) %>%
        dplyr::arrange(match(month, month.abb))  # ensure Jan-Dec order


      # Smooth min, mean, max with loess
      smooth_df <- monthly_avg %>%
        mutate(
          min_smooth  = predict(loess(min  ~ as.numeric(month), span = 0.5)),
          max_smooth  = predict(loess(max  ~ as.numeric(month), span = 0.5)),
          mean_smooth = predict(loess(mean ~ as.numeric(month), span = 0.5))
        ) %>% mutate_if(is.numeric, round, 1) %>%
        mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))# <0 to 0s

      return(smooth_df)
    }

    # -------------------------------------------------------------------------
    # Compute temperature
    compute_temperature <- function(roi) {
      t_10 <- list.files("data/temperature/wc2.1_10m_tavg", full.names = TRUE) |>
        rast(win = terra::ext(roi)) |> crop(roi) |> mask(roi)
      stats <- terra::global(t_10, c("mean", "range"), na.rm = TRUE)
      stats$month <- lubridate::month(1:12, label = TRUE)
      
      # Aggregate by month across all years
      monthly_avg <- stats %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          mean = mean(mean, na.rm = TRUE),
          min = min(min, na.rm = TRUE),
          max = max(max, na.rm = TRUE)
        ) %>%
        dplyr::arrange(match(month, month.abb))  # ensure Jan-Dec order
      
      # Smooth min, mean, max with loess - this makes it much prettier!
      smooth_df <- monthly_avg %>%
        mutate(
          min_smooth  = predict(loess(min  ~ as.numeric(month), span = 0.5)),
          max_smooth  = predict(loess(max  ~ as.numeric(month), span = 0.5)),
          mean_smooth = predict(loess(mean ~ as.numeric(month), span = 0.5))
        ) %>% mutate_if(is.numeric, round, 1)
      
      return(smooth_df)
    }
    
    # -------------------------------------------------------------------------
    # Compute elevation
    compute_elevation <- function(roi) {
      elev_roi <- rast(get_elev_raster(locations = st_as_sf(roi), z = 6)) |> crop(roi) |> mask(roi)
      return(elev_roi)
    }
    
    # -------------------------------------------------------------------------
    # Compute Global Ecosystem Typology
    compute_typology <- function(roi, efg_lookup) {
      validate(need(!is.null(roi), "ROI not loaded."))
      
      # Load typology rasters ----------------------------------------------------
      get_data <- rast(list.files("data/get_geotiff",
                                  pattern = "\\.tif$", full.names = TRUE)[c(1:26, 28:70, 72:109)],
                       win = ext(roi))
      # if (length(get_data) == 0) return(data.frame())
      validate(need(!is.null(get_data), "Could not load typology rasters."))
      
      # Crop + mask to ROI
      r_roi <- terra::crop(get_data, roi) |> terra::mask(roi)
      
      # count number of cells for each layer/value
      cellCount <- freq(r_roi, usenames=T) %>% arrange(desc(count), desc(value))

      # grand total of all counts (all layers, both values)
      grand_total <- sum(cellCount$count, na.rm = TRUE)
      
      # Summarise proportion per EFG --------------------------------------------
      typ_tbl <- cellCount |>
        dplyr::group_by(layer) |>
        dplyr::summarise(
          count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
          count_total = sum(count, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          proportion_global = count_major / grand_total,
          proportion_within_layer = dplyr::if_else(count_total > 0,
                                                   count_major / count_total, NA_real_),
          EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract EFG_CODE = text up to the second dot
        ) |>
        dplyr::arrange(desc(proportion_global)) |>
        dplyr::filter(proportion_global >= 0.05) |>
        dplyr::left_join(
          efg_lookup %>%
            dplyr::select(
              EFG_CODE = EFG_ID,
              `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
              Realm = REALM,
              Biome = BIOME
            ),
          by = "EFG_CODE"
        ) |>
        dplyr::mutate(`% of ROI` = scales::percent(proportion_global, accuracy = 0.1)) |>
        dplyr::select(EFG_CODE, `Ecosystem Functional Group`, Realm, Biome, `% of ROI`)
      
      typ_tbl
    }
    
    # -------------------------------------------------------------------------
    # Process ROI on button click
    observeEvent(process_roi_btn(), {
      
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0,
                                      max = 6)
      progress$set(message = "Processing ecosystem maps...", value = 0)
      
      # show modal popup
      showModal(modalDialog(
        title = "Calculating EAU description data",
        tagList(
          div(style = "text-align:center;",
              tags$h4("Please wait... "),
              tags$p("This will take less than 30 seconds ", tags$em("(but the wait will be worth it!)")),
              withSpinner(uiOutput("dummy_spinner"), type = 5)  # CSS spinner
          )
        ),
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Update progress
      progress$inc(1/6, detail = "loading ROI")
      
      roi <- load_roi(roi_input())
      rv$roi <- roi
      update_roi_map(roi)
      # Update progress
      progress$inc(2/6, detail = "countries")
      rv$countries <- compute_country_overlap(roi, countries)
      # Update progress
      progress$inc(3/6, detail = "rainfall")
      # rv$rainfall <- compute_rainfall(roi)
      # Update progress
      progress$inc(4/6,detail = "temperature")
      rv$temperature <- compute_temperature(roi)
      # Update progress
      progress$inc(5/6, detail = "elevation")
      rv$elevation <- compute_elevation(roi)
      # Update progress
      progress$inc(6/6, detail = "typology")
  #    rv$typology <- compute_typology(roi, efg_lookup)
      
      # hide modal once done
      removeModal()
      progress$close()
      
    })
    
    
    # -------------------------------------------------------------------------
    # Outputs - plots and graphs
    # -------------------------------------------------------------------------
    
    # country table ----------------------------------------------------------
    output$roi_country_table <- DT::renderDataTable({
      req(rv$countries)
      rv$countries
    }, options = list(pageLength = 5, dom = "t"))
    
    # rainfall graph ---------------------------------------------------------
    output$rain_graph <- renderPlotly({
      req(rv$rainfall)

      plot_ly(rv$rainfall, x = ~month) %>%
        add_ribbons(
          ymin = ~min_smooth,
          ymax = ~max_smooth,
          line = list(color = 'transparent'),
          fillcolor = 'lightblue',
          opacity = 0.4,
          name = "95% range"
        ) %>%
        add_lines(
          y = ~mean_smooth,
          line = list(color = 'blue', width = 2),
          name = "Mean"
        ) %>%
        layout(
          # title = "Monthly Rainfall (average 2010-2024)",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Rainfall (mm)")
        )
    })
    
    # output temperature graph------------------------------------------------
    output$temp_graph <- renderPlotly({
      req(rv$temperature)
      
      p2 <- plot_ly(rv$temperature, x = ~month) %>%
        add_ribbons(
          ymin = ~min_smooth,
          ymax = ~max_smooth,
          line = list(color = 'transparent'),
          fillcolor = 'firebrick',
          opacity = 0.1,
          name = "95% range"
        ) %>%
        add_lines(
          y = ~mean_smooth,
          line = list(color = 'red', width = 2),
          name = "Mean"
        ) %>%
        layout(
          # title = "Monthly Rainfall (average since 2010)",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Rainfall (mm)")
        )
    })
    
    # elevation histogram ----------------------------------------------------
    output$elev_graph <- renderPlotly({
      req(rv$elevation)
      
      elev_values <- values(rv$elevation, na.rm = TRUE)  # get numeric vector without NAs
      elev_values <- as.numeric(elev_values)             # ensure it's a vector, not matrix
      
      plot_ly(x = elev_values, type = "histogram") %>%
        layout(xaxis = list(title = "Elevation (m)"), yaxis = list(title = "Frequency"), bargap = 0.1)
    })
    
    # typology table ---------------------------------------------------------
    output$get_table <- DT::renderDataTable({
      req(rv$typology)
      rv$typology
    }, options = list(
      paging = FALSE,        # no pagination
      searching = FALSE,     # no search box
      info = FALSE,          # no "Showing X of Y entries"
      dom = 't'              # only show table
    ), rownames = FALSE)

    #map layers --------------------------------------------------------------
    observe({
      req(rv$elevation)  # ensure raster exists
      
      leafletProxy(ns("roi_map")) %>%
        clearImages() %>%   # optional: clear previous raster layers
        addRasterImage(
          rv$elevation,  # note the parentheses to read reactive
          colors = colorNumeric(
            palette = terrain.colors(100),
            domain = values(rv$elevation),
            na.color = "transparent"
          ),
          opacity = 0.7,
          group = "Elevation"
        ) %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          overlayGroups = c("ROI", "Rainfall (10y avg.)", "Elevation"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    
    
    
    
    #-------------------------------------------------------------------------
    # raster processing part
    #-------------------------------------------------------------------------
    # ideally this should be separate
    
    #--------------------------------------------------------------------------
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      req(tifs_input())                    # see app.R server for tifs_inputs
      files <- tifs_input()$datapath
      years <- year_inputs()               # see app.R server for year_inputs
      req(length(years) == length(files))  # sanity check
      
      # show modal popup
      showModal(modalDialog(
        title = "Processing classification maps",
        tagList(
          div(style = "text-align:center;",
              tags$h4("Please wait... "),
              tags$p("This will take approximately 5 seconds per classification map"),
              tags$p(tags$b("Criteria A"), "results will be visable on the ",tags$b("'Criteria A'")," tab on completion"),
              withSpinner(uiOutput("dummy_spinner"), type = 5)  # CSS spinner
          )
        ),
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Call the data module function (see data.R) to process rasters
      data_module$process_rasters(files, years)
      
      # hide modal once done
      removeModal()
    })

    # Render leaflet map ------------------------------------------------------
    observe({
      r_list <- data_module$rasters()
      req(r_list)

      latest <- r_list[[length(r_list)]]

      # extract name of the latest raster (should be the year, e.g. "2024")
      latest_year <- names(latest)
      if (is.null(latest_year) || latest_year == "") latest_year <- "Latest"
      
      # Display label for users
      display_label <- paste0(latest_year, " classification")
      
      # overlay groups
      overlay_groups <- display_label  # always include the raster
      
      # Only add ROI/Elevation if the ROI map exists
      if (!is.null( rv$roi) && length( rv$roi) > 0) {
        overlay_groups <- c(overlay_groups, "ROI", "Elevation")
      }
      
      leafletProxy(ns("roi_map")) %>%
        clearGroup("classification") %>%  # remove any existing classification rasters
        clearGroup(display_label) %>%  # remove any existing classification rasters
        addRasterImage(latest, opacity = 0.7, colors = "#000000",
                       group = display_label,
                       layerId = display_label,
                       options = leaflet::pathOptions(pane = "rasterPane")) %>%
         addLayersControl(
           baseGroups = c("OSM", "ESRI Satellite"),
           overlayGroups = overlay_groups,
           options = layersControlOptions(collapsed = FALSE)) %>%
        setView(lng = crds(centroids(vect(ext(latest))))[1],
                lat = crds(centroids(vect(ext(latest))))[2],
                zoom= 7)
      
    })
    
    
    # ##
    # # -------------------------------------------------------------------------
    # # Compute Global Ecosystem Typology
    # #   - Based on the most recent uploaded EAU raster instead of ROI polygon
    # # -------------------------------------------------------------------------
    # compute_typology <- function(eau_rasters, efg_lookup) {
    #   validate(need(!is.null(eau_rasters) && length(eau_rasters) > 0,
    #                 "No EAU rasters available."))
    # 
    #   # Use the most recent EAU raster (assuming names are years or sorted chronologically)
    #   latest_raster <- eau_rasters[[length(eau_rasters)]]
    #   validate(need(!is.null(latest_raster), "Latest EAU raster not found."))
    # 
    #   # Load typology rasters ----------------------------------------------------
    #   typology_files <- list.files(
    #     "data/get_geotiff",
    #     pattern = "\\.tif$",
    #     full.names = TRUE
    #   )[c(1:26, 28:70, 72:109)]
    # 
    #   get_data <- rast(typology_files, win = ext(latest_raster))
    #   validate(need(!is.null(get_data), "Could not load typology rasters."))
    # 
    #   # Crop + mask to the extent of the latest EAU raster
    #   r_masked <- terra::crop(get_data, latest_raster) |>
    #     terra::mask(latest_raster)
    # 
    #   # Count number of cells for each layer/value
    #   cellCount <- freq(r_masked, usenames = TRUE) |>
    #     dplyr::arrange(desc(count), desc(value))
    # 
    #   # Grand total of all counts
    #   grand_total <- sum(cellCount$count, na.rm = TRUE)
    # 
    #   # Summarise proportion per EFG --------------------------------------------
    #   typ_tbl <- cellCount |>
    #     dplyr::group_by(layer) |>
    #     dplyr::summarise(
    #       count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
    #       count_total = sum(count, na.rm = TRUE),
    #       .groups = "drop"
    #     ) |>
    #     dplyr::mutate(
    #       proportion_global = count_major / grand_total,
    #       proportion_within_layer = dplyr::if_else(count_total > 0,
    #                                                count_major / count_total, NA_real_),
    #       EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract code before 2nd dot
    #     ) |>
    #     dplyr::arrange(desc(proportion_global)) |>
    #     dplyr::filter(proportion_global >= 0.05) |>
    #     dplyr::left_join(
    #       efg_lookup |>
    #         dplyr::select(
    #           EFG_CODE = EFG_ID,
    #           `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
    #           Realm = REALM,
    #           Biome = BIOME
    #         ),
    #       by = "EFG_CODE"
    #     ) |>
    #     dplyr::mutate(`% of Area` = scales::percent(proportion_global, accuracy = 0.1)) |>
    #     dplyr::select(EFG_CODE, `Ecosystem Functional Group`, Realm, Biome, `% of Area`)
    # 
    #   typ_tbl
    # }
    # 
    # observeEvent(input$compute_typology, {
    #   req(data_module$rasters())
    #   typology_tbl <- compute_typology(data_module$rasters(), efg_lookup)
    #   output$typology_table <- renderTable(typology_tbl)
    #   # typology table ---------------------------------------------------------
    #   output$get_table <- DT::renderDataTable({
    #     typology_tbl
    #   }, options = list(
    #     paging = FALSE,        # no pagination
    #     searching = FALSE,     # no search box
    #     info = FALSE,          # no "Showing X of Y entries"
    #     dom = 't'              # only show table
    #   ), rownames = FALSE)
    # 
    # })
    
    # return the data/function ------------------------------------------------
    return(list(
      roi = reactive({ rv$roi }),
      roi_wgs = reactive({ rv$roi_wgs }),
      countries = reactive({rv$countries}),
      rainfall = reactive({rv$rainfall})
    ))
    
  })
}

