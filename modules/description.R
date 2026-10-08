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
        # card_header(tags$strong("Ecosystem Distribution"), tooltip(bs_icon("info-circle"),
        #                                                  "Distribution of uploaded ecosystem from the most recent mapped year"
        # )),
        
        #   card_header(
        #   tags$div(
        #     style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
        #     tags$strong("Ecosystem Distribution"), tooltip(bs_icon("info-circle"),
        #                                                    "Distribution of uploaded ecosystem from the most recent mapped year"
        #     ),
        #     actionLink(ns("show_draw_help"), tagList(bs_icon("info-circle"), " How to draw"),
        #                style = "font-size:12px;")
        #   
        # )),
        
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
                tagList(bs_icon("pencil"), " How to refine"),
                style = "
          padding:0;
          text-decoration:none;
        "
              )
            )
          )
        ),
        
        
        
        card_body(leafletOutput(ns("eu_map"), height = 500),class = "p-0")),
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
        )
      )),
    
    layout_column_wrap(
      width = 1/2,
      height = 300,
      
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
    )
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
                                      year_inputs,
                                      session_tmp = tempdir()
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
    
          # # Cache descriptive data for full extent
          # desc_cache <- reactiveValues(
          #   elevation   = NULL)
          # 
          # desc_vars <- c("elevation")
          # 
    
    # File-based cache for the full-extent elevation raster.
    # On spatial subset, the cached raster is masked rather than re-downloaded.
    elev_cache <- file.path(session_tmp, "elev_full.tif")
    
    # # Clear the elevation cache whenever new data is uploaded
    # observeEvent(tifs_input(), {
    #   if (file.exists(elev_cache)) unlink(elev_cache)
    # }, ignoreNULL = FALSE)
    
    # =========================================================================
    # In-memory cache for all other descriptive results (mirrors cached_full in upload module).
    # Stores the *final* output objects (data frames / SpatRaster) at full extent, so revert
    # can restore instantly without recomputing.
    desc_cache <- reactiveValues(
      countries   = NULL,
      rainfall    = NULL,
      temperature = NULL,
      typology    = NULL,
      elevation   = NULL
    )
    
    # =========================================================================
    # Clear all descriptive state whenever new data is uploaded
    observeEvent(tifs_input(), {
      # Live values shown in graphs/tables
      rv$countries   <- NULL
      rv$rainfall    <- NULL
      rv$temperature <- NULL
      rv$typology    <- NULL
      rv$elevation   <- NULL
      
      # In-memory cache
      desc_cache$countries   <- NULL
      desc_cache$rainfall    <- NULL
      desc_cache$temperature <- NULL
      desc_cache$typology    <- NULL
      desc_cache$elevation   <- NULL
      
      # File cache for full-extent elevation raster
      if (file.exists(elev_cache)) unlink(elev_cache)
    }, ignoreNULL = FALSE)
    
    
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
        
        plot_ly(x = elev_values, type = "histogram", histnorm = "percent",marker=list(color="#669900"), xbins=list(size=20)) %>% # 20  = 20 meters
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
    
    # ## section for removing modal after descriptive vars compelte
    # desc_tasks_remaining <- reactiveVal(0)
    # 
    # # Call this at the START of each descriptive observer
    # begin_desc_task <- function() desc_tasks_remaining(desc_tasks_remaining() + 1)
    # 
    # # Call this at the END of each descriptive observer  
    # # end_desc_task <- function() {
    # #   desc_tasks_remaining(max(0, desc_tasks_remaining() - 1))
    # #   if (desc_tasks_remaining() == 0) removeModal()
    # # }
    # end_desc_task <- function() {
    #   desc_tasks_remaining(max(0, desc_tasks_remaining() - 1))
    #   if (desc_tasks_remaining() == 0) {
    #     removeModal()
    #     # Only show on a full-extent run, not on refine reprocess
    #     if (!isTRUE(upload_module$is_subsetted())) {
    #       showModal(draw_instructions_modal)
    #     }
    #   }
    # }
    ## ---- Modal close for descriptive vars -------------------------------
    ## Computes are synchronous and sequential, so no task counter is needed.
    ## A single flag tells the rasters_original-triggered observer whether the
    ## latest rasters_original() change came from a genuine process run
    ## (recompute) or from a revert-to-full-extent (restore from cache instead).
    desc_process_pending <- reactiveVal(FALSE)
    
    # A real process click arms the flag; revert does not touch process_btn.
    observeEvent(process_btn(), {
      req(process_btn() > 0)
      desc_process_pending(TRUE)
    }, ignoreInit = TRUE)
    
    # No counter needed — one synchronous compute pass.
    end_desc_task <- function() {
      message("[desc] end_desc_task reached; is_subsetted=", isolate(upload_module$is_subsetted()))
      removeModal()
      if (!isTRUE(isolate(upload_module$is_subsetted()))) {
        showModal(draw_instructions_modal)
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
    # Processing — compute descriptive variables when rasters_original is ready
    # ========================================================================
    # Triggered by rasters_original() becoming populated (set at the END of the
    # classification process observer), so it can never race ahead of the data.
    # The NULL transition (set at the START of that observer) is swallowed by req().
    observeEvent(upload_module$rasters_original(), {
      
      message("[desc] observer fired on rasters_original; run_description = ",
              deparse(isTRUE(isolate(upload_module$run_description()))),
              "; pending = ", isolate(desc_process_pending()),
              "; rasters_original null? ", is.null(upload_module$rasters_original()))
      
      # Only proceed when the data is genuinely ready
      req(upload_module$rasters_original())
      # Only run descriptive vars if the user asked for them (read without depending on it)
      req(isTRUE(isolate(upload_module$run_description())))
      # Only recompute for a real process run; revert restores from cache elsewhere
      req(isTRUE(isolate(desc_process_pending())))
      isolate(desc_process_pending(FALSE))   # consume the flag
      
      # begin_desc_task_placeholder <- NULL  # (no counter needed)
      
      tryCatch({
        # reset all reactive values to NULL first
        rv$countries    <- NULL
        rv$rainfall     <- NULL
        rv$temperature  <- NULL
        rv$elevation    <- NULL
        rv$typology     <- NULL
        
        years <- isolate(year_inputs())
        
        # Progress bar for descriptive variable extraction
        progress <- shiny::Progress$new(min = 0, max = 6)
        progress$set(message = "extracting data...", value = 0)
        on.exit(try(progress$close(), silent = TRUE), add = TRUE)
        
        latest <- isolate(upload_module$rasters_original())
        
        progress$inc(1, detail = "countries")
        message("[desc] >>> countries START")
        rv$countries <- tryCatch(compute_countries(latest, countries),
                                 error = function(e) { message("[desc] countries ERR: ", conditionMessage(e)); NULL })
        message("[desc] <<< countries DONE (null? ", is.null(rv$countries), ")")
        
        progress$inc(1, detail = "monthly temperature")
        message("[desc] >>> temperature START")
        rv$temperature <- tryCatch(compute_temperature(latest),
                                   error = function(e) { message("[desc] temperature ERR: ", conditionMessage(e)); NULL })
        message("[desc] <<< temperature DONE (null? ", is.null(rv$temperature), ")")
        
        progress$inc(1, detail = "elevation")
        message("[desc] >>> elevation START")
        rv$elevation <- tryCatch(compute_elevation(latest, cache_path = elev_cache),
                                 error = function(e) { message("[desc] elevation ERR: ", conditionMessage(e)); NULL })
        message("[desc] <<< elevation DONE (null? ", is.null(rv$elevation), ")")
        
        progress$inc(1, detail = "global ecosystem typology")
        message("[desc] >>> typology START")
        rv$typology <- tryCatch(compute_typology(latest, efg_lookup),
                                error = function(e) { message("[desc] typology ERR: ", conditionMessage(e)); NULL })
        message("[desc] <<< typology DONE (null? ", is.null(rv$typology), ")")
        
        progress$inc(1, detail = "monthly rainfall")
        message("[desc] >>> rainfall START")
        rv$rainfall <- tryCatch(compute_precipitation(latest),
                                error = function(e) { message("[desc] rainfall ERR: ", conditionMessage(e)); NULL })
        message("[desc] <<< rainfall DONE (null? ", is.null(rv$rainfall), ")")
        
        progress$inc(1, detail = "done")
        
        # ---- Cache full-extent results for instant revert -----------------
        if (!isTRUE(isolate(upload_module$is_subsetted()))) {
          desc_cache$countries   <- rv$countries
          desc_cache$rainfall    <- rv$rainfall
          desc_cache$temperature <- rv$temperature
          desc_cache$typology    <- rv$typology
          desc_cache$elevation   <- rv$elevation
        }
      },
      error = function(e) {
        message("[desc] unexpected top-level error: ", conditionMessage(e))
      },
      finally = {
        end_desc_task()   # ALWAYS closes the modal, whatever happened above
      })
    }, ignoreInit = TRUE)
    
    # ========================================================================
    # Restore descriptive variables on revert
    # ========================================================================
    # Upload module sets is_subsetted(FALSE) when "Revert to full extent" is clicked
    # AND does NOT increment process_trigger — so we restore from cache here instead
    # of recomputing. Mirror of cached_full restore in classification_upload.
    observeEvent(upload_module$is_subsetted(), {
      if (!isTRUE(upload_module$is_subsetted()) &&
          !is.null(desc_cache$countries)) {
        rv$countries   <- desc_cache$countries
        rv$rainfall    <- desc_cache$rainfall
        rv$temperature <- desc_cache$temperature
        rv$typology    <- desc_cache$typology
        rv$elevation   <- desc_cache$elevation
      }
    }, ignoreInit = TRUE)
    
    
    
    # ========================================================================
    # Render the leaflet map
    # ========================================================================

    
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
      # output$eu_map <- renderLeaflet({
      #   make_fig_distribution_leaflet(
      #     r_map  = upload_module$rasters(),
      #     r_loss = upload_module$rasters_change(),
      #     years  = isolate(upload_module$years())   # ← isolate prevents year-input re-renders
      #   )
      # })
      output$eu_map <- renderLeaflet({
        message("[trace] eu_map render START")
        on.exit(message("[trace] eu_map render END"))
        make_fig_distribution_leaflet(
          r_map = upload_module$rasters(),
          r_loss = upload_module$rasters_change(),
          years = isolate(upload_module$years()))
      })
    }, ignoreNULL = FALSE) # this resets teh observer desciption    
    
 
    # Drawn polygon from map
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
      
      #print(drawn_poly())
      
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

