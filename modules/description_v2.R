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
                                           "Elevation distribution within the ecosystem"
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
        card_header("Ecosystem Country Overlap", tooltip(bs_icon("info-circle"),
                                                   "Proportion of the ecosystem within each country"
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
        ), tooltip(bs_icon("info-circle"),
                   "Global Ecosystem Typology (Levels 1-3) within the ecosystem")
        ),
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
            # overlayGroups = c(display_label, "ROI", "Elevation"), # , "Rainfall (10y avg.)"
            overlayGroups = c(display_label), # , ""ROI", "Elevation, Rainfall (10y avg.)"
            options = layersControlOptions(collapsed = FALSE))
        
        
        
      }
    }
    
    # -------------------------------------------------------------------------
    #                        Description variables
    # -------------------------------------------------------------------------
    
    # -------------------------------------------------------------------------
    # Compute overlap with countries
    source("helpers/country_overlap.R") #compute_countries
    
    # -------------------------------------------------------------------------
    # Compute temperature
    source("helpers/temperature.R") # compute_temperature
    
    # -------------------------------------------------------------------------
    # Compute elevation
    source("helpers/typology.R") # compute_elevation
    
    # -------------------------------------------------------------------------
    # Compute Global Ecosystem Typology
    source("helpers/elevation.R") # compute_typology
    
    # -------------------------------------------------------------------------
    # Compute rainfall
    source("helpers/precipitation.R") # compute_precipitation
   

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
          yaxis = list(title = "Temperature (°C)")
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
              # tags$p("This will take approximately 15 seconds per classification map"),
              tags$p(tags$b("Criteria A"), "results will be visible on the ",tags$b("'Criteria A'")," tab on completion"),
              withSpinner(uiOutput("dummy_spinner"), type = 5)  # CSS spinner
          )
        ),
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Call the data module function (see data.R) to process rasters
      data_module$process_rasters(files, years)
      
      
      # Description data ------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0,
                                      max = 2)
      progress$set(message = "extacting data...", value = 0)
      
      latest <- data_module$rasters_original()[[length(files)]]
      ##latest <- r_list[[length(r_list)]]

      # 1. country overlap
      progress$inc(2/6, detail = "countries") # update progress
      rv$countries <- compute_countries(latest, countries)

      # 2. temperature
      progress$inc(3/6,detail = "monthly temperature") # update progress
      rv$temperature <- compute_temperature(latest)

      # 3. elevation
      progress$inc(4/6, detail = "elevation")
      rv$elevation <- compute_elevation(latest)

      # 4. global ecosystem typology (get)
      progress$inc(5/6, detail = "global ecosystem typology")
      rv$typology <- compute_typology(latest, efg_lookup)

      # 5. precipitation
      progress$inc(6/6, detail = "monthly rainfall") # update progress
      rv$rainfall <- compute_precipitation(latest)
      
      # hide modal once done
      removeModal()
      progress$close()
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
      
      # # Only add ROI/Elevation if the ROI map exists
      # if (!is.null( rv$roi) && length( rv$roi) > 0) {
      #   overlay_groups <- c(overlay_groups, "ROI", "Elevation")
      # }
      
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
    
    # return the data/function ------------------------------------------------
    return(list(
      roi = reactive({ rv$roi }),
      roi_wgs = reactive({ rv$roi_wgs }),
      countries = reactive({rv$countries}),
      rainfall = reactive({rv$rainfall}),
      typology = reactive({rv$typology}),
      temperature = reactive({rv$temperature}),
      elevation = reactive({rv$elevation})
    ))
    
  })
}


