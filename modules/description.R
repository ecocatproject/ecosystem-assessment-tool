# modules/description.R


# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------
description_module_ui <- function(id) {
  ns <- NS(id)

nav_panel(
  "Description",
  layout_column_wrap(
    width = 1/2,
    height = 600,
    
    # Ecosystem / ROI map -----------------------------------------------------
    card(
      full_screen = TRUE,
      card_header("Ecosystem location"),
      card_body(leafletOutput(ns("roi_map"), height = 500),class = "p-0")
    ),
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      
      # Rainfall --------------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("Rainfall",
                    tooltip(
                      bs_icon("info-circle"),
                      "Mean annual rainfall time series over the ROI (10-year average)"
                    )),
        card_body(
          plotlyOutput(ns("rain_graph")),  min_height = 150,
        ),
      ), 
      
      # Temperature -----------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("Temperature",
                    tooltip(
                      bs_icon("info-circle"),
                      "Mean annual temperature time series over the ROI (10-year average)"
                    )),
        card_body(
          plotlyOutput(ns("temp_graph")),  min_height = 150,
        ),
      ),
      
      # Elevation -------------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("Elevation",
                    tooltip(
                      bs_icon("info-circle"),
                      "Elevation distribution within the ROI"
                    )), #style = "height: 25px; line-height: 25px;"),
        card_body(
          plotlyOutput(ns("elev_graph")),  min_height = 150,
        ),
      )
    )
  ),
  layout_column_wrap(
    width = 1/2,
    height = 300,
    
    # Countries  --------------------------------------------------------------
    card(
      full_screen = TRUE,
      card_header("ROI country overlap",
                  tooltip(
                    bs_icon("info-circle"),
                    "Proportion of the ROI overlapping each country"
                  )),
      # h4("ROI Country Overlap"),
      tableOutput(ns("roi_country_table"))
    ),
    
    # Global ecosystem typology  ----------------------------------------------
    card(
      full_screen = TRUE,
      card_header(tags$a(href = "https://global-ecosystems.org/", "Global Ecosystem Typology",
                         tooltip(
                           bs_icon("info-circle"),
                           "Mean annual rainfall time series over the ROI (10-year average)"
                         ))),
      tableOutput(ns("get_table")),
    )
  )
)
}


# -----------------------------------------------------------------------------
# server 
# -----------------------------------------------------------------------------
# mod_description_server <- function(id, roi_vec, countries, process_roi_btn, roi_countries, typology_table) {
#   moduleServer(id, function(input, output, session) {
#     observeEvent(process_roi_btn(), {
#       ...
#     })
#   })
# }



description_module_server <- function(id, data_module, tifs_input, year_inputs, process_btn, process_roi_btn, roi_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # reactive variables to store data ----------------------------------------
    roi_countries <- reactiveVal(data.frame())  # store country stats table
    chirps_summary <- reactiveVal()
    elevation_summary <- reactiveVal()
    rain_data <- reactiveVal()
    temp_data <- reactiveVal()
    typology_table <- reactiveVal(list())  
    roi_vec <- reactiveVal()
    roi_wgs <- reactiveVal()
    roi <- reactiveVal()
  
  
  # initialize map and tables -------------------------------------------------
    # Initialize map
    output$roi_map <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
        addLayersControl(
                  baseGroups = c("OSM", "ESRI Satellite"),
                  options = layersControlOptions(collapsed = FALSE)
                )
    })

    
    # load in files for an ROI ------------------------------------------------
    observeEvent(process_roi_btn(), {

      req(roi_input())                    # see app.R server for tifs_inputs
      roi_files <- roi_input()$datapath
      
        roi_names <- roi_input()$name
        
        # Safety check
        if (length(roi_files) == 0 || length(roi_names) == 0) {
          showNotification("No files uploaded.", type = "error")
          return(NULL)
        }
        
        temp_dir <- tempdir()
        
        for (i in seq_along(roi_files)) {
          file.copy(from = roi_files[i],
                    to = file.path(temp_dir, roi_names[i]),
                    overwrite = TRUE)
        }
        
        shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
        
        if (length(shp_files) != 1) {
          showNotification("No valid .shp file found in upload.", type = "error")
          return(NULL)
        }
        
        shp_file <- shp_files[1]
        roi_vec <- terra::vect(shp_file)
        
        # Show country table
        output$roi_country_table <- renderTable({
          roi_countries()
        }, sanitize.text.function = identity)   # allow HTML superscripts
        
        
        # add data to reactives ----------------------------------------------
        roi(roi_vec)
        roi_vec(roi_vec)
        roi_wgs(terra::project(roi_vec, "EPSG:4326"))
        
        # Render leaflet map --------------------------------------------------
        centroid <- terra::centroids(roi_wgs())
        coords <- terra::crds(centroid)
        
        leafletProxy(ns("roi_map")) %>%
          clearShapes() %>%
          addPolygons(data = sf::st_as_sf(roi_wgs()), fillColor = "grey80", fillOpacity = 0.2,
                      color = "darkblue", weight = 2,group = "ROI",
                      highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)) %>%
          addLayersControl(
            baseGroups = c("OSM", "ESRI Satellite"),
            overlayGroups = c("ROI"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 6)   
      
   
    #--------------------------------------------------------------------------
    # Calculate overlap with countries
    # observeEvent(process_roi_btn(), {
    #   req(roi_vec())
      
      # ensure same CRS
      countries_proj <- terra::project(countries, "EPSG:4326")
      roi_proj <- terra::project(roi_vec(), "EPSG:4326")
      
      roi_int <- tryCatch({
        terra::intersect(countries_proj, roi_proj)
      }, error = function(e) {
        message("Error during intersect: ", e$message)
        NULL
      })
      
      if (!is.null(roi_int) && nrow(roi_int) > 0) {
        roi_int$area_km2 <- terra::expanse(roi_int, unit = "km")
        total_area <- terra::expanse(roi_proj, unit = "km")
        
        tbl <- roi_int |>
          as.data.frame() |>
          dplyr::group_by(admin) |>
          dplyr::summarise(area_km2 = sum(area_km2), .groups = "drop") |>
          dplyr::mutate(
            perc_roi = 100 * area_km2 / total_area,
            area_km2 = format(round(area_km2, 0), big.mark = ",", trim = TRUE),
            perc_roi = format(round(perc_roi, 1), trim = TRUE)
          ) |>
          dplyr::arrange(desc(perc_roi)) |>
          dplyr::rename(
            Country = admin,
            `Area (km<sup>2</sup>)` = area_km2,
            `% of ROI` = perc_roi
          )
      
        roi_countries(tbl)
      } else {
        message("No valid intersection between ROI and countries.")
        roi_countries(data.frame())
      }
    # })
    
    # Show country table ------------------------------------------------------
    output$roi_country_table <- renderTable({
      req(roi_countries())
      roi_countries()
    }, sanitize.text.function = identity)
    
    
    #--------------------------------------------------------------------------
    # Global ecosystem typology
      # Global ecosystem typology
      req(roi_vec())
      # so 27 and 71 have different extents..... remove these for now to get things working....
      get_data <- rast(list.files("data/get_geotiff",
                                  pattern = "\\.tif$", full.names = TRUE)[c(1:26, 28:70, 72:109)],
                       win = ext(roi_vec()))
      
      # Clip to ROI
      r_roi <- crop(get_data, roi_vec()) |> mask(roi_vec())
      
      # count number of cells for each layer/value
      cellCount <- freq(r_roi, usenames=T)
      cellCount |>
        arrange(desc(count), desc(value))
      
      
      # grand total of all counts (all layers, both values)
      grand_total <- sum(cellCount$count, na.rm = TRUE)
      
      efg_lookup2 <- efg_lookup %>%
        dplyr::select(EFG_CODE = EFG_ID,
                      EFG_NAME = ECOSYSTEM_FUNCTIONAL_GROUP)
      
      get_tbl <- cellCount |>
        group_by(layer) |>
        summarise(
          count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
          count_total = sum(count, na.rm = TRUE)) |>
        mutate(
          proportion_global = count_major / grand_total,
          proportion_within_layer = ifelse(count_total > 0,
                                           count_major / count_total, NA_real_),
          EFG_CODE = str_extract(layer, "^[^.]+\\.[0-9]+")  # Extract EFG_CODE = text up to the second dot
        ) |>
        arrange(desc(proportion_global)) |>
        filter(proportion_global >= 0.05) |>
        dplyr::select(EFG_CODE, proportion_global) |>
        left_join(efg_lookup2, by = "EFG_CODE") |>
        # pretty column names
        rename(
          EFG_CODE = EFG_CODE,
          `% of ROI` = proportion_global
        )
      
      # Example: merge results_df (with EFG_CODE) to add name/realm/biome
      merged <- merge(
        get_tbl,
        efg_lookup[, c("EFG_ID", "ECOSYSTEM_FUNCTIONAL_GROUP", "REALM", "BIOME")],
        by.x = "EFG_CODE",   # column in your results_df
        by.y = "EFG_ID",     # column in lookup
        all.x = TRUE
      )
      
      merged <- merged %>%
        select(EFG_CODE, `% of ROI`, ECOSYSTEM_FUNCTIONAL_GROUP, REALM, BIOME) %>%
        rename(`Ecosysystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP)
      
      
      typology_table(get_tbl) # assign to a reactive
      typology_table(merged) # assign to a reactive
      
      # create table
    output$get_table <- renderTable({
      typology_table()
    }, sanitize.text.function = identity)   # allow HTML superscripts
    
    
    
    #--------------------------------------------------------------------------
    # Rainfall
    # Calculate rainfall using terraClim in climateR package - currently the fastest if need to do on the fly
      climate_raw <- getTerraClim(
        roi(),
        varname = "ppt",
        startDate = "2010-01-01",
        endDate = "2024-12-31"
      )
      
      roi_rast <- climate_raw$ppt |>
        terra::crop(roi_vec()) |>
        terra::mask(roi_vec())
      
      # Extract dates from layer names  ---------------------------------------
      dates <- as.Date(gsub("ppt_|_total", "", names(roi_rast)))
      #months <- month(dates, label = TRUE)  # abbreviated month names
      months <- lubridate::month(dates, label = TRUE)  # abbreviated month names
      
      # Compute median, min, max for each layer  ------------------------------
      layer_stats <- global(x=roi_rast,   c("mean", "range"), na.rm = TRUE)
      layer_stats <- as.data.frame(layer_stats)
      layer_stats$date <- dates
      layer_stats$month <- months
      
      # Aggregate by month across all years  ----------------------------------
      monthly_avg <- layer_stats %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          mean = mean(mean, na.rm = TRUE),
          min = min(min, na.rm = TRUE),
          max = max(max, na.rm = TRUE)
        ) %>%
        dplyr::arrange(match(month, month.abb))  # ensure Jan-Dec order
      
      rain_data(monthly_avg) # assign to a reactive
      
      print(monthly_avg)
      
      # Smooth min, mean, max with loess --------------------------------------
      smooth_df <- monthly_avg %>%
        mutate(
          min_smooth  = predict(loess(min  ~ as.numeric(month), span = 0.5)),
          max_smooth  = predict(loess(max  ~ as.numeric(month), span = 0.5)),
          mean_smooth = predict(loess(mean ~ as.numeric(month), span = 0.5))
        ) %>% mutate_if(is.numeric, round, 1) %>%
        mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))# <0 to 0s
      print(smooth_df)
      
      # graph -----------------------------------------------------------------
      output$rain_graph <- renderPlotly({
        p2 <- plot_ly(smooth_df, x = ~month) %>%
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
            title = "Monthly Rainfall (average since 2010)",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Rainfall (mm)")
          )
      })
    
    
    
    
    #--------------------------------------------------------------------------
    # Temperature
      #  get temperature data - currently from local file, some potential options for remote downloads, but slower/trickier
      
      # read with terra
      t_10 <- rast(list.files("data/temperature/wc2.1_10m_tavg", full.names=T),
                   win=ext(roi_vec())) %>%
       terra::crop(roi_vec()) %>%
       terra::mask(roi_vec())
      
      # Compute median, min, max for each layer
      temp_layer_stats <- global(x=t_10,   c("mean", "range"), na.rm = TRUE)
      # layer_stats <- as.data.frame(layer_stats)
      temp_layer_stats$month <- month.abb
      temp_layer_stats$month <- lubridate::month(1:12, label = TRUE)
      
      # Aggregate by month across all years
      temp_monthly_avg <- temp_layer_stats %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          mean = mean(mean, na.rm = TRUE),
          min = min(min, na.rm = TRUE),
          max = max(max, na.rm = TRUE)
        ) %>%
        dplyr::arrange(match(month, month.abb))  # ensure Jan-Dec order
      
      # rain data
      temp_data(temp_monthly_avg) # assign to a reactive
      
      # Smooth min, mean, max with loess - this makes it much prettier!
      smooth_temp <- temp_monthly_avg %>%
        mutate(
          min_smooth  = predict(loess(min  ~ as.numeric(month), span = 0.5)),
          max_smooth  = predict(loess(max  ~ as.numeric(month), span = 0.5)),
          mean_smooth = predict(loess(mean ~ as.numeric(month), span = 0.5))
        ) %>% mutate_if(is.numeric, round, 1)
      
      
      # output graph
      output$temp_graph <- renderPlotly({
        p2 <- plot_ly(smooth_temp, x = ~month) %>%
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
            title = "Monthly Rainfall (average since 2010)",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Rainfall (mm)")
          )
      })
    
    
    
    
    
    #--------------------------------------------------------------------------
    # Elevation
    #  get elevation data
    elev_roi <- rast(get_elev_raster(locations = st_as_sf(roi_vec()), z=6)) # z might need to be dynamic
    elev_roi <- terra::mask(terra::crop(elev_roi, roi_vec()), roi_vec())
    elev_pal <- colorNumeric(
      # palette = "Greys",
      palette = wiki.colors(100),
      domain = values(elev_roi),
      na.color = "transparent"
    )
    
    elevation_summary(values(elev_roi)) # add to reactive
    
    # -------------------------------------------------------
    # Add layers to leaflet map
    # currentl rainfall, elevation
    # lots of options for basemaps - https://leaflet-extras.github.io/leaflet-providers/preview/
    leafletProxy(ns("roi_map")) %>%
      addRasterImage(elev_roi,
                     colors = elev_pal, opacity = 0.7, group = "Elevation") %>%
      # clearShapes() %>%
      # addTiles(group = "OSM") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
      # addPolygons(data = sf::st_as_sf(roi_wgs),
      #             fillColor = "grey80", fillOpacity = 0.2,
      #             color = "darkblue", weight = 2, group="ROI",
      #             highlightOptions = highlightOptions(bringToFront = TRUE, color="white", weight=3)) %>%
      # addRasterImage(chirps_roi,
      #                colors = pal, opacity = 0.7, group = "Rainfall (10y avg.)") %>%
      # addRasterImage(elev_roi,
      #                colors = elev_pal, opacity = 0.7, group = "Elevation") # %>%
      # setView(lng = coords[1], lat = coords[2], zoom = 6) #%>%  # zoom to centroid
      # addLegend(pal = pal,values = values(chirps_roi),group = "CHIRPS Avg (10y)") %>%
      addLayersControl(baseGroups = c("OSM", "ESRI Satellite"),
                       overlayGroups = c("ROI", "Rainfall (10y avg.)", "Elevation"),
                       options = layersControlOptions(collapsed = F)) #%>%
      # addDrawToolbar(position = "topright",
      #                editOptions =
      #                  editToolbarOptions(edit = FALSE))
    
    
    # -------------------------------------------------------
    # plotly histogram
    output$elev_graph <- renderPlotly({
      elev_p <- plot_ly(x = ~values(elev_roi, na.rm=T)[,1], type = "histogram") %>%
        layout(xaxis = list(title = "Elevation (m)"),
               yaxis = list(title = "Frequency"),
               bargap = 0.1)
    })
    
    
    
    }) # end of observer roi_process_button
    
    
    #--------------------------------------------------------------------------
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      req(tifs_input())                    # see app.R server for tifs_inputs
      files <- tifs_input()$datapath    
      years <- year_inputs()               # see app.R server for year_inputs
      req(length(years) == length(files))  # sanity check
      
      # Call the data module function (see data.R) to process rasters
      data_module$process_rasters(files, years)
    })
    
    # Render leaflet map ------------------------------------------------------
    observe({
      r_list <- data_module$rasters()
      req(r_list)
      
      latest <- r_list[[length(r_list)]]
      
      leafletProxy(ns("roi_map")) %>%
        addRasterImage(latest, opacity = 0.7, colors = "#000000", group = "Latest EAU") %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          overlayGroups = c("Latest EAU", "ROI", "Rainfall (10y avg.)", "Elevation"),
          options = layersControlOptions(collapsed = FALSE)) %>%
            setView(lng = crds(centroids(vect(ext(latest))))[1],
                    lat = crds(centroids(vect(ext(latest))))[2],
                    zoom= 7)
    })
    
    
    
    })
}



