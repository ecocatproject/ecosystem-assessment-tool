# modules/description_eu.R

# =============================================================================
# Description Module
# =============================================================================

# This module processes data to present the following descriptive variables:
# temperature
# rainfall
# country overlap
# global ecosystem typology overlap
# elevation

# =============================================================================



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
        card_header("Ecosystem Distribution", tooltip(bs_icon("info-circle"),
                                                         "Distribution of uploaded ecosystem from the most recent mapped year"
        )),
        card_body(leafletOutput(ns("eu_map"), height = 500),class = "p-0")),
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        # Rainfall ------------------------------------------------------------
        card(
          full_screen = TRUE,
          card_header(tagList( "Rainfall", uiOutput(ns("r_value"), inline = TRUE)),
                    # tooltip(bs_icon("info-circle"),"Mean monthly rainfall (2010–2024 average)")),
                      tooltip(bs_icon("info-circle"),"Mean monthly rainfall across mapped pixels (1970-2000 average) extracted from WorldClim v2.1")),
        
          card_body(plotlyOutput(ns("rain_graph")),  min_height = 150)
        ),
        
        # Temperature ---------------------------------------------------------
        card(
          full_screen = TRUE,
          card_header(
            tagList( "Temperature", uiOutput(ns("t_value"), inline = TRUE)),
            # tooltip(bs_icon("info-circle"), "Mean monthly temperature (2010–2024 average
            tooltip(bs_icon("info-circle"), "Mean monthly temperture across mapped pixels (1970-2000 average) extracted from WorldClim v2.1")),
          card_body(plotlyOutput(ns("temp_graph")),  min_height = 150)
        ),
        
        # Elevation -----------------------------------------------------------
        card(
          full_screen = TRUE,
          card_header(tagList("Elevation", uiOutput(ns("e_value"), inline = TRUE)),
                       tooltip(bs_icon("info-circle"), "Elevation distribution within the ecosystem extracted from Amazon Web Services Terrain Tiles")),
          card_body(plotlyOutput(ns("elev_graph")),  min_height = 150)
        )
      )),
    
    layout_column_wrap(
      width = 1/2,
      height = 300,
      
      # Country overlap -------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("Ecosystem Country Overlap", 
                    tooltip(bs_icon("info-circle"), "Proportion of the ecosystem within each country"
        )),
        card_body(DT::dataTableOutput(ns("roi_country_table")))
      ),
      
      # Typology -------------------------------------------------------------
      card(
        full_screen = TRUE,
        card_header(tags$a(href = "https://global-ecosystems.org/", "Global Ecosystem Typology", target = "_blank"),
                    tooltip(bs_icon("info-circle"), "Global Ecosystem Typology (Levels 1-3) within the ecosystem (> 1% of major or minor occurrence)"
        )),
        # card_body(shinycssloaders::withSpinner(DT::dataTableOutput(ns("get_table")), type=5))
        card_body(DT::dataTableOutput(ns("get_table")))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
description_module_server <- function(id, upload_module, countries,
                                      selected_layer, process_btn,
                                      tifs_input, 
                                      year_inputs
                                      ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Reactive values container
    rv <- reactiveValues(
      countries = NULL,
      rainfall = NULL,
      temperature = NULL,
      elevation = NULL,
      typology = NULL
    )
    
    # -------------------------------------------------------------------------
    # Leaflet base map
    output$eu_map <- renderLeaflet({
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
    #                     Outputs - plots and graphs
    # -------------------------------------------------------------------------
   
    # country table ----------------------------------------------------------
    output$roi_country_table <- DT::renderDataTable({
      req(rv$countries)
      rv$countries
    }, options = list(pageLength = 5, dom = "t"))
    
    # output rainfall value ------------------------------------------------
    output$r_value <- renderUI({
      if (is.null(rv$rainfall)) return(NULL)
      
      span(
        paste0(" – (Annual average: ", round(sum(rv$rainfall$mean, na.rm=T),0), "mm)"),
        class = "text-muted ms-1"
      )
    })
    
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
    
    # output temperature value ------------------------------------------------
    output$t_value <- renderUI({
      if (is.null(rv$temperature)) return(NULL)
      
      span(
        paste0(" – (Annual average: ", sprintf("%.1f °C) ", mean(rv$temperature$mean, na.rm=T))),
        class = "text-muted ms-1"
      )
    })
    
    # output temperature graph ------------------------------------------------
    output$temp_graph <- renderPlotly({
      req(rv$temperature)
      
      plot_ly(rv$temperature, x = ~month) %>%
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
          xaxis = list(title = "Month"),
          yaxis = list(title = "Temperature (°C)")
        )
    })
    
    # output elevation value ------------------------------------------------
    output$e_value <- renderUI({
      if (is.null(rv$elevation)) return(NULL)
      e_vals <- global(rv$elevation, quantile, probs=c(0.05, .95), na.rm=T)
      span(
        paste0(" – (95% range: ", e_vals[1], "m - ", e_vals[2], "m)"),
        class = "text-muted ms-1"
      )
    })
    
    # elevation histogram ----------------------------------------------------
    output$elev_graph <- renderPlotly({
      req(rv$elevation)
      
      elev_values <- values(rv$elevation, na.rm = TRUE)  # get numeric vector without NAs
      elev_values <- as.numeric(elev_values)             # ensure it's a vector, not matrix
      
      plot_ly(x = elev_values, type = "histogram", marker=list(color="#669900")) %>%
        layout(xaxis = list(title = "Elevation (m)"),
               yaxis = list(title = "Number of mapped pixels"),
               bargap = 0.1)
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
    
    
    
    
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      req(upload_module$rasters_original())                    # see app.R server for tifs_inputs
      #files <- tifs_input()$datapath
      years <- year_inputs()               # see app.R server for year_inputs
     # req(length(years) == length(files))  # sanity check
      
    
      # Description data ------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0,
                                      max = 2)
      progress$set(message = "extacting data...", value = 0)
      
      latest <- upload_module$rasters_original()[[length(years)]]

      # 1. country overlap
      progress$inc(2/6, detail = "countries") # update progress
      rv$countries <- compute_countries(latest, countries)

      # 2. temperature
      progress$inc(3/6,detail = "monthly temperature") # update progress
      #rv$temperature <- compute_temperature(latest)

      # 3. elevation
      progress$inc(4/6, detail = "elevation")
      #rv$elevation <- compute_elevation(latest)

      # 4. global ecosystem typology (get)
      progress$inc(5/6, detail = "global ecosystem typology")
      #rv$typology <- compute_typology(latest, efg_lookup)

      # 5. precipitation
      progress$inc(6/6, detail = "monthly rainfall") # update progress
      #rv$rainfall <- compute_precipitation(latest)
    
      # hide modal once done
      # removeModal()
      progress$close()
    })
    
    
    # Render leaflet map ------------------------------------------------------
    observe({
      r_list <- upload_module$rasters()
      req(r_list)
      
     # latest <- r_list[[length(r_list)]]
      
      # extract name of the latest raster (should be the year, e.g. "2024")
      latest_year <- names(r_list[[length(r_list)]])
      if (is.null(latest_year) || latest_year == "") latest_year <- "Latest"
      
      # Display label for users
      display_label <- paste0(latest_year, " classification")
      
      # overlay groups
      overlay_groups <- display_label  # always include the raster
      
      # replot 
      leafletProxy(ns("eu_map")) %>%
        # clearGroup("classification") %>%  # remove any existing classification rasters
        # clearGroup(display_label) %>%  # remove any existing classification rasters
        clearImages() %>%
        addRasterImage(r_list[[length(r_list)]],
                       opacity = 0.7, colors = "#000000",
                       group = display_label,
                       layerId = display_label,
                       options = leaflet::pathOptions(pane = "rasterPane")) %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          overlayGroups = overlay_groups,
          options = layersControlOptions(collapsed = FALSE)) %>%
        setView(lng = crds(centroids(vect(ext(r_list[[length(r_list)]]))))[1],
                lat = crds(centroids(vect(ext(r_list[[length(r_list)]]))))[2],
                zoom= 7)
      
      removeModal()
   })

    # return the data/function ------------------------------------------------
    return(list(
      countries = reactive({rv$countries}),
      rainfall = reactive({rv$rainfall}),
      typology = reactive({rv$typology}),
      temperature = reactive({rv$temperature}),
      elevation = reactive({rv$elevation})
    ))
    
  })
}

