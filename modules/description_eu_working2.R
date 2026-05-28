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



# ==============================================================================
# UI
# ==============================================================================

description_module_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Description",
    layout_column_wrap(
      width = 1/2,
      height = 600,

      # ----------------------------------------------------------------------
      # Ecosystem / ROI map
      card(
        full_screen = TRUE,

              card_header(
        tags$div(
          style = "
      display:flex;
      justify-content:space-between;
      align-items:center;
      margin-bottom:4px;
    ",
          
          # Left side
          tags$strong("Ecosystem Distribution",  tooltip(
            bs_icon("info-circle"),
            "Distribution of uploaded ecosystem from the most recent mapped year"
          )),
          
          # Right side
          tags$div(
            style = "
        display:flex;
        align-items:center;
        gap:12px;
        font-size:12px;
      ",
            
           
            
            actionLink(
              ns("show_draw_help"),
              tagList(bs_icon("pencil"), " How to subset"),
              style = "
          padding:0;
          text-decoration:none;
        "
            )
          )
        )
      ),
        
        
        
        card_body(leafletOutput(ns("eu_map"), height = 800),class = "p-0")),
      layout_column_wrap(
        width = 1,
        heights_equal = "row",

        # ----------------------------------------------------------------------
        # Rainfall 
        card(
          full_screen = TRUE,
          card_header(tagList( tags$strong("Rainfall"), uiOutput(ns("r_value"), inline = TRUE)),
                      tooltip(bs_icon("info-circle"),"Mean monthly rainfall across mapped pixels (1970-2000 average) extracted from WorldClim v2.1")),

          card_body(plotlyOutput(ns("rain_graph")),  min_height = 150)
        ),

        # ----------------------------------------------------------------------
        # Temperature
        card(
          full_screen = TRUE,
          card_header(
            tagList( tags$strong("Temperature"), uiOutput(ns("t_value"), inline = TRUE)),
            tooltip(bs_icon("info-circle"), "Mean monthly temperature across mapped pixels (1970-2000 average) extracted from WorldClim v2.1")),
          card_body(plotlyOutput(ns("temp_graph")),  min_height = 150)
        ),

        # ----------------------------------------------------------------------
        # Elevation
        card(
          full_screen = TRUE,
          card_header(tagList(tags$strong("Elevation"), uiOutput(ns("e_value"), inline = TRUE)),
                       tooltip(bs_icon("info-circle"), "Elevation distribution within the ecosystem extracted from Amazon Web Services Terrain Tiles")),
          card_body(plotlyOutput(ns("elev_graph")),  min_height = 150)
        ),
        
        # ----------------------------------------------------------------------
        # Country overlap
        card(
          full_screen = TRUE,
          card_header(tags$strong("Ecosystem Country Overlap"),
                      tooltip(bs_icon("info-circle"), "Proportion of the ecosystem within each country"
                      )),
          # card_body(DT::dataTableOutput(ns("roi_country_table")))
          card_body(uiOutput(ns("roi_country_table")))
        ),
        
        # ----------------------------------------------------------------------
        # Typology
        card(
          full_screen = TRUE,
          card_header(tags$strong(tags$a(href = "https://global-ecosystems.org/", "Global Ecosystem Typology", target = "_blank"), " overlap"),
                      tooltip(bs_icon("info-circle"), 
                              "Shows overlap between the classified map and the IUCN Global Ecosystem Typology (v. 2.1, Levels 1–3).
                            Only ecosystem functional groups with overlap exceeding 1% of major or minor occurrence are included. Results are indicative due to the coarse resolution and ongoing development of the global typology. 
                            The assessor should determine the most appropriate ecosystem typology."
                      )),
          card_body(uiOutput(ns("get_table")))
        )
      ))#,

    # layout_column_wrap(
    #   width = 1/2,
    #   height = 300,

   
    # )
  )
}
# ==============================================================================
# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
# ==============================================================================
description_module_server <- function(id, upload_module, countries,
                                      selected_layer, process_btn,
                                      tifs_input,
                                      year_inputs
                                      ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
      observeEvent(input$show_draw_help, {
      showModal(draw_instructions_modal)
    })
    
    # =========================================================================
    # set up reactive variables
    # =========================================================================
    rv <- reactiveValues(
      countries = NULL,
      rainfall = NULL,
      temperature = NULL,
      elevation = NULL,
      typology = NULL
    )
      
      # Cache descriptive data for full extent
      desc_cache <- reactiveValues(
        countries   = NULL,
        temperature = NULL,
        elevation   = NULL,
        typology    = NULL,
        rainfall    = NULL
      )
      
      desc_vars <- c("countries", "temperature", "elevation", "typology", "rainfall")

    # =========================================================================
    # base map (leaflet)
    # =========================================================================
    output$eu_map <- renderLeaflet({
      leaflet() %>%
        addMapPane("rasterPane", zIndex = 410) %>%  # create persistent raster pane
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          options = layersControlOptions(collapsed = FALSE)) %>%
            addDrawToolbar(
              targetGroup   = "drawn",
              polylineOptions = FALSE,
              circleOptions   = FALSE,
              rectangleOptions = drawRectangleOptions(repeatMode = FALSE),
              polygonOptions   = drawPolygonOptions(repeatMode = FALSE),
              markerOptions    = FALSE,
              circleMarkerOptions = FALSE,
              editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
            )
    })

    # =========================================================================
    # Functions for descriptive variables
    # =========================================================================
    # Compute overlap with countries
    source("helpers/country_overlap.R") #compute_countries
    
    # Compute temperature
    source("helpers/temperature.R") # compute_temperature
    
    # Compute elevation
    source("helpers/typology.R") # compute_elevation
    
    # Compute Global Ecosystem Typology
    source("helpers/elevation.R") # compute_typology
    
    # Compute rainfall
    source("helpers/precipitation.R") # compute_precipitation


    # =========================================================================
    # Outputs - graphs and tables
    # =========================================================================
    # blank plot when varibles calcualtion not selected
    biotic_blank <- plot_ly(type = "scatter") %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(
            text = "Upload data to view results",
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "grey")
          )
        ),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
    
    # -------------------------------------------------------------------------
    # output rainfall value
    output$r_value <- renderUI({
      if (is.null(rv$rainfall)) return(NULL)
      span(
        paste0(" – (Annual average: ", round(sum(rv$rainfall$mean, na.rm=T),0), "mm)"),
        class = "text-muted ms-1"
      )
    })

    # -------------------------------------------------------------------------
    # rainfall graph 
    output$rain_graph <- renderPlotly({
     # req(rv$rainfall)
      print(str(rv$rainfall))
      p <- tryCatch(rv$rainfall, error = function(e) NULL)
      if (is.null(p)) {
        biotic_blank
      } else {
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
      }
    })
    
    # -------------------------------------------------------------------------
    # output temperature value 
    output$t_value <- renderUI({
      if (is.null(rv$temperature)) return(NULL)

      span(
        paste0(" – (Annual average: ", sprintf("%.1f °C) ", mean(rv$temperature$mean, na.rm=T))),
        class = "text-muted ms-1"
      )
    })
    
    # -------------------------------------------------------------------------
    # output temperature graph 
    output$temp_graph <- renderPlotly({
      # req(rv$temperature)
      p <- tryCatch(rv$temperature, error = function(e) NULL)
      if (is.null(p)) {
        biotic_blank
      } else {
      plot_ly(rv$temperature, x = ~month ) %>%
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
      }
    })
    
    # -------------------------------------------------------------------------
    # output elevation value 
    output$e_value <- renderUI({
      if (is.null(rv$elevation)) return(NULL)
      e_vals <- global(rv$elevation, quantile, probs=c(0.05, .95), na.rm=T)
      span(
        paste0(" – (95% range: ", e_vals[1], "m - ", e_vals[2], "m)"),
        class = "text-muted ms-1"
      )
    })
    
    # -------------------------------------------------------------------------
    # elevation histogram 
    output$elev_graph <- renderPlotly({
      # req(rv$elevation)
      p <- tryCatch(rv$elevation, error = function(e) NULL)
      if (is.null(p)) {
        biotic_blank
      } else {
      
      elev_values <- values(rv$elevation, na.rm = TRUE)  # get numeric vector without NAs
      elev_values <- as.numeric(elev_values)             # ensure it's a vector, not matrix

      plot_ly(x = elev_values, type = "histogram", histnorm = "percent",marker=list(color="#669900")) %>%
        layout(xaxis = list(title = "Elevation (m)"),
               yaxis = list(title = "Percentage of mapped pixels"),
               bargap = 0.01)
      }
    })

    # -------------------------------------------------------------------------
    # typology table 
    output$get_dt <- DT::renderDataTable({
        req(rv$typology)
        rv$typology
      },
      options = list(
      paging = FALSE,        # no pagination
      searching = FALSE,     # no search box
      info = FALSE,          # no "Showing X of Y entries"
      dom = 't'              # only show table
    ), rownames = FALSE)

    output$get_blank <- renderPlotly({ biotic_blank })
    
    # renderUI just decides which one to show
    output$get_table <- renderUI({
      dat <- tryCatch(rv$countries, error = function(e) NULL)
      if (is.null(dat)) {
        plotlyOutput(ns("get_blank"))
      } else {
        dataTableOutput(ns("get_dt"))
      }
    })
    
    # -------------------------------------------------------------------------
    # country table
    # Define the two outputs independently
    output$roi_country_dt <- DT::renderDataTable({
      req(rv$countries)
      rv$countries
    }, options = list(pageLength = 5, dom = "t"))
    
    output$roi_country_blank <- renderPlotly({ biotic_blank })
    
    # renderUI just decides which one to show
    output$roi_country_table <- renderUI({
      dat <- tryCatch(rv$countries, error = function(e) NULL)
      if (is.null(dat)) {
        plotlyOutput(ns("roi_country_blank"))
      } else {
        dataTableOutput(ns("roi_country_dt"))
      }
    })
    
    ## section for removing modal after descriptive vars compelte
    desc_tasks_remaining <- reactiveVal(0)
    
    # Call this at the START of each descriptive observer
    begin_desc_task <- function() desc_tasks_remaining(desc_tasks_remaining() + 1)
    
    # Call this at the END of each descriptive observer  
    # end_desc_task <- function() {
    #   desc_tasks_remaining(max(0, desc_tasks_remaining() - 1))
    #   if (desc_tasks_remaining() == 0) removeModal()
    # }
    end_desc_task <- function() {
      desc_tasks_remaining(max(0, desc_tasks_remaining() - 1))
      if (desc_tasks_remaining() == 0) {
        removeModal()
        # Only show on a full-extent run, not on refine reprocess
        if (!isTRUE(upload_module$is_subsetted())) {
          showModal(draw_instructions_modal)
        }
      }
    }
    
    
    # Capture drawn polygon — fires on each new draw
    observeEvent(input$eu_map_draw_new_feature, {
      feat   <- input$eu_map_draw_new_feature
      coords <- feat$geometry$coordinates[[1]]
      mat    <- do.call(rbind, lapply(coords, function(p) c(p[[1]], p[[2]])))
      poly   <- terra::vect(mat, type = "polygon", crs = "EPSG:4326")
      drawn_poly(poly)
    })
    
    # Clear drawn_poly if the user deletes the shape
    observeEvent(input$eu_map_draw_deleted_features, {
      drawn_poly(NULL)
    })
    
    observeEvent(drawn_poly(), {
      if (is.null(drawn_poly())) {
        leafletProxy(ns("eu_map")) %>% clearGroup("drawn")
      }
    }, ignoreNULL = FALSE)
    
    # ========================================================================
    # Processing 
    # ========================================================================
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      req(isTRUE(upload_module$run_description()))   # guard — only runs if checked
      
      begin_desc_task()
      
      # reset all reactive values to NULL first
      rv$countries    <- NULL
      rv$rainfall     <- NULL
      rv$temperature  <- NULL
      rv$elevation    <- NULL
      rv$typology     <- NULL

      req(upload_module$run_description())
      req(upload_module$rasters_original()) # see app.R server for tifs_inputs
      years <- year_inputs()                # see app.R server for year_inputs

      # Add drawn_poly store near other reactiveVals in the server
      drawn_poly <- reactiveVal(NULL)
      

      # Description data ------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0,
                                      max = 2)
      progress$set(message = "extracting data...", value = 0)

      latest <- upload_module$rasters_original()#[[length(years)]]

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

      # # hide modal once done
      progress$close()
      
      end_desc_task()   # closes modal only when ALL tasks finish
      # hide modal once done
     
    })

    # ========================================================================
    # Render the leaflet map
    # ========================================================================
 
    # 0.  Reset when source data changes (new upload, new file selection)
    observeEvent(upload_module$tifs(), {
      for (nm in desc_vars) desc_cache[[nm]] <- NULL
      rv$countries   <- NULL
      rv$temperature <- NULL
      rv$elevation   <- NULL
      rv$typology    <- NULL
      rv$rainfall    <- NULL
    }, ignoreNULL = FALSE)
    
    # Reset when the description checkbox is toggled off
    observeEvent(input$run_description, {
      if (!isTRUE(input$run_description)) {
        rv$countries   <- NULL
        rv$temperature <- NULL
        rv$elevation   <- NULL
        rv$typology    <- NULL
        rv$rainfall    <- NULL
      }
    }, ignoreNULL = FALSE)
    
    # Reset when the subset is TRUE
    observeEvent(input$run_description, {
      if (!isTRUE(upload_module$is_subsetted())) {
        rv$countries   <- NULL
        rv$temperature <- NULL
        rv$elevation   <- NULL
        rv$typology    <- NULL
        rv$rainfall    <- NULL
      }
    }, ignoreNULL = FALSE)

    
    # 1. Reset drawn_poly and map when rasters are cleared (new upload)
    observeEvent(upload_module$rasters(), {
      
      if (is.null(upload_module$rasters())) {
        # New data incoming — clear draw state and show blank base map
        drawn_poly(NULL)
        output$eu_map <- renderLeaflet({
          leaflet() |>
            addTiles(group = "OSM") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
            addLayersControl(
              baseGroups = c("OSM", "ESRI Satellite"),
              options = layersControlOptions(collapsed = FALSE)) %>%
            leaflet.extras::addDrawToolbar(
              targetGroup         = "drawn",
              polylineOptions     = FALSE, circleOptions = FALSE,
              markerOptions       = FALSE, circleMarkerOptions = FALSE,
              rectangleOptions    = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
              polygonOptions      = leaflet.extras::drawPolygonOptions(repeatMode  = TRUE),
              editOptions         = leaflet.extras::editToolbarOptions()
            )
        })
        return()
      }
      
      # 2. Use isolate() for years so year selection doesn't re-render the map
      output$eu_map <- renderLeaflet({
        make_fig_distribution_leaflet(
          r_map  = upload_module$rasters(),
          r_loss = upload_module$rasters_change(),
          years  = isolate(upload_module$years())   # ← isolate prevents year-input re-renders
        )
      })
    }, ignoreNULL = FALSE) # this resets teh observer desciption    
    

    observeEvent(upload_module$is_subsetted(), {
      if (isTRUE(upload_module$is_subsetted())) {
        # Subset just applied — cache current full-extent values before they're overwritten
        # Only cache if not already cached (first subset after this upload)
        for (nm in desc_vars) {
          if (is.null(desc_cache[[nm]]) && !is.null(rv[[nm]]))
            desc_cache[[nm]] <- rv[[nm]]
        }
      } else {
        # Reverted to full extent — restore from cache if available, else let
        # existing reset logic trigger a recalculation
        for (nm in desc_vars) {
          if (!is.null(desc_cache[[nm]]))
            rv[[nm]] <- desc_cache[[nm]]
        }
      }
    }, ignoreNULL = FALSE)
    
    
    # ── Drawn polygon from map ──────────────────────────────────────────────────
    drawn_poly <- reactiveVal(NULL)
    
    # Replaces both the new_feature and deleted_features observers
    observeEvent(input$eu_map_draw_all_features, {
      features <- input$eu_map_draw_all_features$features
      
      # No features left (all deleted) — clear
      if (is.null(features) || length(features) == 0) {
        drawn_poly(NULL)
        return()
      }
      
      # Convert every drawn shape to a SpatVector and combine
      polys <- lapply(features, function(feat) {
        coords <- feat$geometry$coordinates[[1]]
        mat    <- do.call(rbind, lapply(coords, function(p) c(p[[1]], p[[2]])))
        terra::vect(mat, type = "polygon", crs = "EPSG:4326")
      })
      
      drawn_poly(do.call(rbind, polys))   # single multi-polygon SpatVector
      

    }, ignoreNULL = FALSE)
    


   # =========================================================================
   # Return reactive outputs
   # =========================================================================
    return(list(
      countries = reactive({rv$countries}),
      rainfall = reactive({rv$rainfall}),
      typology = reactive({rv$typology}),
      temperature = reactive({rv$temperature}),
      elevation = reactive({rv$elevation}),
      drawn_poly = drawn_poly
    ))

  })
}
