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
    
    # # ========================================================================
    # # Processing 
    # # ========================================================================
    # # Process uploaded TIFs when the process_button is clicked
    # observeEvent(process_btn(), {
    # # observeEvent(upload_module$rasters_original(), {
    #       message("[desc] observer fired; run_description = ",
    #           deparse(isTRUE(upload_module$run_description())),
    #           "; rasters_original null? ", is.null(upload_module$rasters_original()))
    #   # req(isTRUE(upload_module$run_description()))   # guard — only runs if checked
    #   req(isTRUE(isolate(upload_module$run_description()))) # isolate means toggling the checkbox alone doesn't re-fire the compute
    #   # req(upload_module$run_description())
    #   # req(upload_module$rasters_original()) # see app.R server for tifs_inputs
    #   
    #   # begin_desc_task()
    #   
    #   tryCatch({
    #   # reset all reactive values to NULL first
    #   rv$countries    <- NULL
    #   rv$rainfall     <- NULL
    #   rv$temperature  <- NULL
    #   rv$elevation    <- NULL
    #   rv$typology     <- NULL
    #   
    # 
    #   years <- year_inputs()                # see app.R server for year_inputs
    #   
    #   # # Add drawn_poly store near other reactiveVals in the server
    #   # drawn_poly <- reactiveVal(NULL)
    #   
    #   
    #   # Description data ------------------------------------------------------
    #   # Create a Progress object
    #   progress <- shiny::Progress$new(min = 0,
    #                                   max = 2)
    #   progress$set(message = "extacting data...", value = 0)
    #   
    #   latest <- upload_module$rasters_original()#[[length(years)]]
    #   
    #   # # 1. country overlap
    #   # progress$inc(2/6, detail = "countries")
    #   # rv$countries <- tryCatch(compute_countries(latest, countries), error = function(e) {
    #   #   message("[desc] countries failed: ", conditionMessage(e)); NULL
    #   # })
    #   # 
    #   # # 2. temperature
    #   # progress$inc(3/6, detail = "monthly temperature")
    #   # rv$temperature <- tryCatch(compute_temperature(latest), error = function(e) {
    #   #   message("[desc] temperature failed: ", conditionMessage(e)); NULL
    #   # })
    #   # #      rv$temperature <- tryCatch(compute_temperature(...), error = function(e) NULL)
    #   # # if (!is.null(rv$temperature)) {
    #   # #   # update temperature display
    #   # # } else {
    #   # #   # show "no data for this area" message
    #   # # }
    #   # 
    #   # # 3. elevation
    #   # progress$inc(4/6, detail = "elevation")
    #   # rv$elevation <- tryCatch(compute_elevation(latest, cache_path = elev_cache), error = function(e) {
    #   #   message("[desc] elevation failed: ", conditionMessage(e)); NULL
    #   # })
    #   # 
    #   # # 4. global ecosystem typology (get)
    #   # progress$inc(5/6, detail = "global ecosystem typology")
    #   # rv$typology <- tryCatch(compute_typology(latest, efg_lookup), error = function(e) {
    #   #   message("[desc] typology failed: ", conditionMessage(e)); NULL
    #   # })
    #   # 
    #   # # 5. precipitation
    #   # progress$inc(6/6, detail = "monthly rainfall")
    #   # rv$rainfall <- tryCatch(compute_precipitation(latest), error = function(e) {
    #   #   message("[desc] rainfall failed: ", conditionMessage(e)); NULL
    #   # })
    #   message("[desc] >>> countries START")
    #   rv$countries <- tryCatch(compute_countries(latest, countries),
    #                            error = function(e) { message("[desc] countries ERR: ", conditionMessage(e)); NULL })
    #   message("[desc] <<< countries DONE (null? ", is.null(rv$countries), ")")
    #   
    #   message("[desc] >>> temperature START")
    #   rv$temperature <- tryCatch(compute_temperature(latest),
    #                              error = function(e) { message("[desc] temperature ERR: ", conditionMessage(e)); NULL })
    #   message("[desc] <<< temperature DONE (null? ", is.null(rv$temperature), ")")
    #   
    #   message("[desc] >>> elevation START")
    #   rv$elevation <- tryCatch(compute_elevation(latest, cache_path = elev_cache),
    #                            error = function(e) { message("[desc] elevation ERR: ", conditionMessage(e)); NULL })
    #   message("[desc] <<< elevation DONE (null? ", is.null(rv$elevation), ")")
    #   
    #   message("[desc] >>> typology START")
    #   rv$typology <- tryCatch(compute_typology(latest, efg_lookup),
    #                           error = function(e) { message("[desc] typology ERR: ", conditionMessage(e)); NULL })
    #   message("[desc] <<< typology DONE (null? ", is.null(rv$typology), ")")
    #   
    #   message("[desc] >>> rainfall START")
    #   rv$rainfall <- tryCatch(compute_precipitation(latest),
    #                           error = function(e) { message("[desc] rainfall ERR: ", conditionMessage(e)); NULL })
    #   message("[desc] <<< rainfall DONE (null? ", is.null(rv$rainfall), ")")
    #   
    #   # # hide modal once done
    #   progress$close()
    #   
    #   # ---- Cache full-extent results for instant revert -------------------
    #   # Only cache when this run was on the unmasked rasters_original.
    #   # Subset runs leave the cache untouched (it still holds the full-extent result).
    #   if (!isTRUE(upload_module$is_subsetted())) {
    #     desc_cache$countries   <- rv$countries
    #     desc_cache$rainfall    <- rv$rainfall
    #     desc_cache$temperature <- rv$temperature
    #     desc_cache$typology    <- rv$typology
    #     desc_cache$elevation   <- rv$elevation
    #   }
    # }, error = function(e) {
    #   message("[desc] unexpected top-level error: ", conditionMessage(e))
    #   if (exists("progress")) try(progress$close(), silent = TRUE)
    # }, finally = {
    #   end_desc_task()    # ALWAYS runs now, no if any above fails
    # }) # close trycatch
    # }) # close observeevent
    
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
      desc_process_pending(FALSE)   # consume the flag
      
      begin_desc_task_placeholder <- NULL  # (no counter needed)
      
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
    #  observeEvent(upload_module$rasters(), {
    #    r_list <- upload_module$rasters()
    #    r_change <- upload_module$rasters_change()
    #    req(r_list)
    #    
    #    # extract name of the latest raster (should be the year, e.g. "2024")
    #    latest_year <- names(r_list[[length(r_list)]])
    #    if (is.null(latest_year) || latest_year == "") latest_year <- "Latest"
    # 
    #    # Display label for users
    #    display_label <- paste0(latest_year, " classification")
    # 
    #    # overlay groups
    #    overlay_groups <- display_label  # always include the raster
    # 
    #    # get eau extent for mapping zoom
    #    ext_r <- as.vector(ext(r_list[[length(r_list)]]))
    # 
    #    # replot
    #    dat <- r_list[[length(r_list)]]
    #    dat <- st_as_stars(dat)
    #    change <- st_as_stars(r_change)
    #    leafletProxy(ns("eu_map")) %>%
    #      clearImages() %>%
    #      addStarsImage(dat,
    #                    opacity = 0.7, colors = "#000000",
    #                    group = display_label,
    #                    layerId = display_label,
    #                    project = FALSE) %>%
    #      addStarsImage(change,
    #                    opacity = 0.7, colors = "#563369",
    #                    # group = display_label,
    #                    # layerId = display_label,
    #                    project = FALSE) %>%
    #      addLayersControl(
    #        baseGroups = c("OSM", "ESRI Satellite"),
    #        overlayGroups = overlay_groups,
    #        options = layersControlOptions(collapsed = FALSE)) %>%
    #      fitBounds(unname(ext_r["xmin"]),
    #                unname(ext_r["ymin"]),
    #                unname(ext_r["xmax"]),
    #                unname(ext_r["ymax"]))
    # 
    #    removeModal()
    # })
    
    #  observeEvent(upload_module$rasters(), {
    #   dat      <- upload_module$rasters()
    #   r_change <- upload_module$rasters_change()
    #   req(dat)
    #   
    #   # extract year name from the raster layer
    #   latest_year   <- names(dat)[1]
    #   if (is.null(latest_year) || latest_year == "") latest_year <- "Latest"
    #   display_label <- paste0(latest_year, " classification")
    #   
    #   # extent for zoom
    #   ext_r <- as.vector(ext(dat))
    #   
    #   # convert to stars
    #   dat_stars <- st_as_stars(dat)
    #   
    #   # base map with classification layer
    #   proxy <- leafletProxy(ns("eu_map")) %>%
    #     clearImages() %>%
    #     addStarsImage(dat_stars,
    #                   opacity   = 0.7,
    #                   colors    = "#000000",
    #                   group     = display_label,
    #                   layerId   = display_label,
    #                   project   = FALSE) %>%
    #     fitBounds(unname(ext_r["xmin"]),
    #               unname(ext_r["ymin"]),
    #               unname(ext_r["xmax"]),
    #               unname(ext_r["ymax"]))
    #   
    #   # add change layer only if it exists (i.e. >1 file uploaded)
    #   overlay_groups <- display_label
    #   if (!is.null(r_change)) {
    #     change_stars <- st_as_stars(r_change)
    #     proxy <- proxy %>%
    #       addStarsImage(change_stars,
    #                     opacity = 0.7,
    #                     colors  = "#563369",
    #                     group   = "Change map",
    #                     layerId = "change_map",
    #                     project = FALSE)
    #     overlay_groups <- c(display_label, "Change map")
    #   }
    #   
    #   proxy %>%
    #     addLayersControl(
    #       baseGroups    = c("OSM", "ESRI Satellite"),
    #       overlayGroups = overlay_groups,
    #       options       = layersControlOptions(collapsed = FALSE)
    #     )
    #   
    #   removeModal()
    # })
    
    # observeEvent(upload_module$rasters(), {
    #   dat    <- upload_module$rasters()
    #   r_loss <- upload_module$rasters_change()
    #   req(dat)
    #   
    #   all_years   <- sort(as.numeric(na.omit(unlist(upload_module$years()))))
    #   latest_year <- if (length(all_years) > 0) max(all_years) else "Latest"
    #   class_label <- paste0(latest_year, " classification")
    #   
    #   ext_r     <- as.vector(ext(dat))
    #   dat_stars <- st_as_stars(dat)
    #   
    #   proxy <- leafletProxy(ns("eu_map")) %>%
    #     clearImages() %>%
    #     clearControls() %>%                          # clears old legend + layer control
    #     addStarsImage(dat_stars, opacity = 0.7, colors = "#000000",
    #                   group = class_label, layerId = class_label, project = FALSE) %>%
    #     fitBounds(unname(ext_r["xmin"]), unname(ext_r["ymin"]),
    #               unname(ext_r["xmax"]), unname(ext_r["ymax"]))
    #   
    #   overlay_groups <- class_label
    #   
    #   if (!is.null(r_loss)) {
    #     loss_label <- if (length(all_years) >= 2) {
    #       paste0(min(all_years), "\u2013", max(all_years), " loss")
    #     } else "Loss map"
    #     
    #     loss_pal <- colorFactor(palette = "#d73027", domain = 1, na.color = "transparent")
    #     
    #     proxy <- proxy %>%
    #       addStarsImage(st_as_stars(r_loss), opacity = 0.85, colors = loss_pal,
    #                     group = loss_label, layerId = "loss_map", project = FALSE) %>%
    #       addLegend(position = "bottomright", colors = "#d73027", labels = "Loss",
    #                 title = loss_label, opacity = 0.85,
    #                 group = loss_label, layerId = "loss_legend")
    #     
    #     overlay_groups <- c(class_label, loss_label)
    #   }
    #   
    #   proxy %>%
    #     addLayersControl(
    #       baseGroups    = c("OSM", "ESRI Satellite"),
    #       overlayGroups = overlay_groups,
    #       options       = layersControlOptions(collapsed = FALSE)
    #     ) %>%
    #     hideGroup(overlay_groups[overlay_groups != class_label])   # loss hidden by default
    #   
    #   removeModal()
    # })
    
    
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
    
    # observeEvent(upload_module$rasters(), {
    #   req(upload_module$rasters())
    #   
    #   output$eu_map <- renderLeaflet({
    #     make_fig_distribution_leaflet(
    #       r_map  = upload_module$rasters(),
    #       r_loss = upload_module$rasters_change(),
    #       years  = upload_module$years()
    #     )
    #   })
    #   
    #  # removeModal()
    # })
    
    
        # observeEvent(upload_module$is_subsetted(), {
        #   if (isTRUE(upload_module$is_subsetted())) {
        #     # Subset just applied — cache current full-extent values before they're overwritten
        #     # Only cache if not already cached (first subset after this upload)
        #     for (nm in desc_vars) {
        #       if (is.null(desc_cache[[nm]]) && !is.null(rv[[nm]]))
        #         desc_cache[[nm]] <- rv[[nm]]
        #     }
        #   } else {
        #     # Reverted to full extent — restore from cache if available, else let
        #     # existing reset logic trigger a recalculation
        #     for (nm in desc_vars) {
        #       if (!is.null(desc_cache[[nm]]))
        #         rv[[nm]] <- desc_cache[[nm]]
        #     }
        #   }
        # }, ignoreNULL = FALSE)
    
    
    # ── Drawn polygon from map ──────────────────────────────────────────────────
    drawn_poly <- reactiveVal(NULL)
    
    # observeEvent(input$eu_map_draw_new_feature, {
    #   feat   <- input$eu_map_draw_new_feature
    #   coords <- feat$geometry$coordinates[[1]]
    #   mat    <- do.call(rbind, lapply(coords, function(p) c(p[[1]], p[[2]])))
    #   poly   <- terra::vect(mat, type = "polygon", crs = "EPSG:4326")
    #   drawn_poly(poly)
    # })
    # 
    # observeEvent(input$eu_map_draw_deleted_features, {
    #   drawn_poly(NULL)
    # })
    
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
    
    
    # ## attempt 2
    # observeEvent(upload_module$rasters(), {
    # 
    #   r_list <- upload_module$rasters()
    #   req(r_list)
    # 
    #   r <- r_list[[length(r_list)]]
    # 
    #   # downsample
    #   fact <- max(1, round(sqrt(ncell(r) / 1e6)))
    #   r_small <- terra::aggregate(r, fact = fact, fun = modal)
    # 
    #   ext_r <- as.vector(ext(r))
    # 
    #   leafletProxy(ns("eu_map")) %>%
    #     clearImages() %>%
    #     addRasterImage(
    #       r_small,
    #       opacity = 0.7,
    #       colors = "#000000",
    #       project = FALSE
    #     ) %>%
    #     fitBounds(ext_r[1], ext_r[2], ext_r[3], ext_r[4])
    # 
    # })
    
    ## attempt 3
    # observeEvent(
    #   list(input$eu_map_bounds, input$eu_map_zoom, rv$raster),
    #   {
    #
    #     req(rv$raster, input$eu_map_bounds)
    #
    #     b <- input$eu_map_bounds
    #
    #     # crop to visible area
    #     r_crop <- terra::crop(
    #       rv$raster,
    #       terra::ext(
    #         b$west, b$east,
    #         b$south, b$north
    #       )
    #     )
    #     fact <- dplyr::case_when(
    #       input$eu_map_zoom <= 4 ~ 25,
    #       input$eu_map_zoom <= 6 ~ 10,
    #       input$eu_map_zoom <= 8 ~ 4,
    #       TRUE ~ 1
    #     )
    #
    #     r_display <- terra::aggregate(r_crop, fact = fact, fun = modal)
    #     leafletProxy(ns("eu_map")) %>%
    #       clearImages() %>%
    #       addRasterImage(
    #         r_display,
    #         project = FALSE,
    #         opacity = 0.7
    #       )
    #   })
    
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










# # modules/description_eu.R
# 
# # =============================================================================
# # Description Module
# # =============================================================================
# 
# # This module processes data to present the following descriptive variables:
# # temperature
# # rainfall
# # country overlap
# # global ecosystem typology overlap
# # elevation
# # =============================================================================
# 
# 
# 
# # ==============================================================================
# # UI
# # ==============================================================================
# 
# description_module_ui <- function(id) {
#   ns <- NS(id)
# 
#   nav_panel(
#     "Description",
#     layout_column_wrap(
#       width = 1/2,
#       height = 600,
# 
#       # ----------------------------------------------------------------------
#       # Ecosystem / ROI map
#       card(
#         full_screen = TRUE,
# 
#               card_header(
#         tags$div(
#           style = "
#       display:flex;
#       justify-content:space-between;
#       align-items:center;
#       margin-bottom:4px;
#     ",
# 
#           # Left side
#           tags$strong("Ecosystem Distribution",  tooltip(
#             bs_icon("info-circle"),
#             "Distribution of uploaded ecosystem from the most recent mapped year"
#           )),
# 
#           # Right side
#           tags$div(
#             style = "
#         display:flex;
#         align-items:center;
#         gap:12px;
#         font-size:12px;
#       ",
# 
# 
# 
#             actionLink(
#               ns("show_draw_help"),
#               tagList(bs_icon("pencil"), " How to refine area"),
#               style = "
#           padding:0;
#           text-decoration:none;
#         "
#             )
#           )
#         )
#       ),
# 
# 
# 
#         card_body(leafletOutput(ns("eu_map"), height = 800),class = "p-0")),
#       layout_column_wrap(
#         width = 1,
#         heights_equal = "row",
# 
#         # ----------------------------------------------------------------------
#         # Rainfall
#         card(
#           full_screen = TRUE,
#           card_header(tagList( tags$strong("Rainfall"), uiOutput(ns("r_value"), inline = TRUE)),
#                       tooltip(bs_icon("info-circle"),"Mean monthly rainfall across mapped pixels (1970-2000 average) extracted from WorldClim v2.1")),
# 
#           card_body(plotlyOutput(ns("rain_graph")),  min_height = 150)
#         ),
# 
#         # ----------------------------------------------------------------------
#         # Temperature
#         card(
#           full_screen = TRUE,
#           card_header(
#             tagList( tags$strong("Temperature"), uiOutput(ns("t_value"), inline = TRUE)),
#             tooltip(bs_icon("info-circle"), "Mean monthly temperature across mapped pixels (1970-2000 average) extracted from WorldClim v2.1")),
#           card_body(plotlyOutput(ns("temp_graph")),  min_height = 150)
#         ),
# 
#         # ----------------------------------------------------------------------
#         # Elevation
#         card(
#           full_screen = TRUE,
#           card_header(tagList(tags$strong("Elevation"), uiOutput(ns("e_value"), inline = TRUE)),
#                        tooltip(bs_icon("info-circle"), "Elevation distribution within the ecosystem extracted from Amazon Web Services Terrain Tiles")),
#           card_body(plotlyOutput(ns("elev_graph")),  min_height = 150)
#         ),
# 
#         # ----------------------------------------------------------------------
#         # Country overlap
#         card(
#           full_screen = TRUE,
#           card_header(tags$strong("Ecosystem Country Overlap"),
#                       tooltip(bs_icon("info-circle"), "Proportion of the ecosystem within each country"
#                       )),
#           # card_body(DT::dataTableOutput(ns("roi_country_table")))
#           card_body(uiOutput(ns("roi_country_table")))
#         ),
# 
#         # ----------------------------------------------------------------------
#         # Typology
#         card(
#           full_screen = TRUE,
#           card_header(tags$strong(tags$a(href = "https://global-ecosystems.org/", "Global Ecosystem Typology", target = "_blank"), " overlap"),
#                       tooltip(bs_icon("info-circle"),
#                               "Shows overlap between the classified map and the IUCN Global Ecosystem Typology (v. 2.1, Levels 1–3).
#                             Only ecosystem functional groups with overlap exceeding 1% of major or minor occurrence are included. Results are indicative due to the coarse resolution and ongoing development of the global typology.
#                             The assessor should determine the most appropriate ecosystem typology."
#                       )),
#           card_body(uiOutput(ns("get_table")))
#         )
#       ))#,
# 
#     # layout_column_wrap(
#     #   width = 1/2,
#     #   height = 300,
# 
# 
#     # )
#   )
# }
# # ==============================================================================
# # -----------------------------------------------------------------------------
# # SERVER
# # -----------------------------------------------------------------------------
# # ==============================================================================
# description_module_server <- function(id, upload_module, countries,
#                                       selected_layer, process_btn,
#                                       tifs_input,
#                                       year_inputs
#                                       ) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
# 
#       observeEvent(input$show_draw_help, {
#       showModal(draw_instructions_modal)
#     })
# 
#     # =========================================================================
#     # set up reactive variables
#     # =========================================================================
#     rv <- reactiveValues(
#       countries = NULL,
#       rainfall = NULL,
#       temperature = NULL,
#       elevation = NULL,
#       typology = NULL
#     )
# 
#       # Cache descriptive data for full extent
#       desc_cache <- reactiveValues(
#         countries   = NULL,
#         temperature = NULL,
#         elevation   = NULL,
#        rainfall    = NULL
#       )     typology    = NULL,
#     
# 
#       desc_vars <- c("countries", "temperature", "elevation", "typology", "rainfall")
# 
#     # =========================================================================
#     # base map (leaflet)
#     # =========================================================================
#     output$eu_map <- renderLeaflet({
#       leaflet() %>%
#         addMapPane("rasterPane", zIndex = 410) %>%  # create persistent raster pane
#         addTiles(group = "OSM") %>%
#         addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
#         addLayersControl(
#           baseGroups = c("OSM", "ESRI Satellite"),
#           options = layersControlOptions(collapsed = FALSE)) %>%
#             addDrawToolbar(
#               targetGroup   = "drawn",
#               polylineOptions = FALSE,
#               circleOptions   = FALSE,
#               rectangleOptions = drawRectangleOptions(repeatMode = FALSE),
#               polygonOptions   = drawPolygonOptions(repeatMode = FALSE),
#               markerOptions    = FALSE,
#               circleMarkerOptions = FALSE,
#               editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
#             )
#     })
# 
#     # =========================================================================
#     # Functions for descriptive variables
#     # =========================================================================
#     # Compute overlap with countries
#     source("helpers/country_overlap.R") #compute_countries
# 
#     # Compute temperature
#     source("helpers/temperature.R") # compute_temperature
# 
#     # Compute elevation
#     source("helpers/typology.R") # compute_elevation
# 
#     # Compute Global Ecosystem Typology
#     source("helpers/elevation.R") # compute_typology
# 
#     # Compute rainfall
#     source("helpers/precipitation.R") # compute_precipitation
# 
# 
#     # =========================================================================
#     # Outputs - graphs and tables
#     # =========================================================================
#     # blank plot when varibles calcualtion not selected
#     biotic_blank <- plot_ly(type = "scatter") %>%
#       layout(
#         xaxis = list(visible = FALSE),
#         yaxis = list(visible = FALSE),
#         annotations = list(
#           list(
#             text = "Upload data to view results",
#             x = 0.5,
#             y = 0.5,
#             xref = "paper",
#             yref = "paper",
#             showarrow = FALSE,
#             font = list(size = 14, color = "grey")
#           )
#         ),
#         paper_bgcolor = "white",
#         plot_bgcolor = "white"
#       )
# 
#     # -------------------------------------------------------------------------
#     # output rainfall value
#     output$r_value <- renderUI({
#       if (is.null(rv$rainfall)) return(NULL)
#       span(
#         paste0(" – (Annual average: ", round(sum(rv$rainfall$mean, na.rm=T),0), "mm)"),
#         class = "text-muted ms-1"
#       )
#     })
# 
#     # -------------------------------------------------------------------------
#     # rainfall graph
#     output$rain_graph <- renderPlotly({
#      # req(rv$rainfall)
#       print(str(rv$rainfall))
#       p <- tryCatch(rv$rainfall, error = function(e) NULL)
#       if (is.null(p)) {
#         biotic_blank
#       } else {
#         plot_ly(rv$rainfall, x = ~month) %>%
#           add_ribbons(
#             ymin = ~min_smooth,
#           ymax = ~max_smooth,
#           line = list(color = 'transparent'),
#           fillcolor = 'lightblue',
#           opacity = 0.4,
#           name = "95% range"
#         ) %>%
#         add_lines(
#           y = ~mean_smooth,
#           line = list(color = 'blue', width = 2),
#           name = "Mean"
#         ) %>%
#         layout(
#           # title = "Monthly Rainfall (average 2010-2024)",
#           xaxis = list(title = "Month"),
#           yaxis = list(title = "Rainfall (mm)")
#         )
#       }
#     })
# 
#     # -------------------------------------------------------------------------
#     # output temperature value
#     output$t_value <- renderUI({
#       if (is.null(rv$temperature)) return(NULL)
# 
#       span(
#         paste0(" – (Annual average: ", sprintf("%.1f °C) ", mean(rv$temperature$mean, na.rm=T))),
#         class = "text-muted ms-1"
#       )
#     })
# 
#     # -------------------------------------------------------------------------
#     # output temperature graph
#     output$temp_graph <- renderPlotly({
#       # req(rv$temperature)
#       p <- tryCatch(rv$temperature, error = function(e) NULL)
#       if (is.null(p)) {
#         biotic_blank
#       } else {
#       plot_ly(rv$temperature, x = ~month ) %>%
#         add_ribbons(
#           ymin = ~min_smooth,
#           ymax = ~max_smooth,
#           line = list(color = 'transparent'),
#           fillcolor = 'firebrick',
#           opacity = 0.1,
#           name = "95% range"
#         ) %>%
#         add_lines(
#           y = ~mean_smooth,
#           line = list(color = 'red', width = 2),
#           name = "Mean"
#         ) %>%
#         layout(
#           xaxis = list(title = "Month"),
#           yaxis = list(title = "Temperature (°C)")
#         )
#       }
#     })
# 
#     # -------------------------------------------------------------------------
#     # output elevation value
#     output$e_value <- renderUI({
#       if (is.null(rv$elevation)) return(NULL)
#       e_vals <- global(rv$elevation, quantile, probs=c(0.05, .95), na.rm=T)
#       span(
#         paste0(" – (95% range: ", e_vals[1], "m - ", e_vals[2], "m)"),
#         class = "text-muted ms-1"
#       )
#     })
# 
#     # -------------------------------------------------------------------------
#     # elevation histogram
#     output$elev_graph <- renderPlotly({
#       # req(rv$elevation)
#       p <- tryCatch(rv$elevation, error = function(e) NULL)
#       if (is.null(p)) {
#         biotic_blank
#       } else {
# 
#       elev_values <- values(rv$elevation, na.rm = TRUE)  # get numeric vector without NAs
#       elev_values <- as.numeric(elev_values)             # ensure it's a vector, not matrix
# 
#       plot_ly(x = elev_values, type = "histogram", histnorm = "percent",marker=list(color="#669900")) %>%
#         layout(xaxis = list(title = "Elevation (m)"),
#                yaxis = list(title = "Percentage of mapped pixels"),
#                bargap = 0.01)
#       }
#     })
# 
#     # -------------------------------------------------------------------------
#     # typology table
#     output$get_dt <- DT::renderDataTable({
#         req(rv$typology)
#         rv$typology
#       },
#       options = list(
#       paging = FALSE,        # no pagination
#       searching = FALSE,     # no search box
#       info = FALSE,          # no "Showing X of Y entries"
#       dom = 't'              # only show table
#     ), rownames = FALSE)
# 
#     output$get_blank <- renderPlotly({ biotic_blank })
# 
#     # renderUI just decides which one to show
#     output$get_table <- renderUI({
#       dat <- tryCatch(rv$countries, error = function(e) NULL)
#       if (is.null(dat)) {
#         plotlyOutput(ns("get_blank"))
#       } else {
#         dataTableOutput(ns("get_dt"))
#       }
#     })
# 
#     # -------------------------------------------------------------------------
#     # country table
#     # Define the two outputs independently
#     output$roi_country_dt <- DT::renderDataTable({
#       req(rv$countries)
#       rv$countries
#     }, options = list(pageLength = 5, dom = "t"))
# 
#     output$roi_country_blank <- renderPlotly({ biotic_blank })
# 
#     # renderUI just decides which one to show
#     output$roi_country_table <- renderUI({
#       dat <- tryCatch(rv$countries, error = function(e) NULL)
#       if (is.null(dat)) {
#         plotlyOutput(ns("roi_country_blank"))
#       } else {
#         dataTableOutput(ns("roi_country_dt"))
#       }
#     })
# 
#     ## section for removing modal after descriptive vars compelte
#     desc_tasks_remaining <- reactiveVal(0)
# 
#     # Call this at the START of each descriptive observer
#     begin_desc_task <- function() desc_tasks_remaining(desc_tasks_remaining() + 1)
# 
#     # Call this at the END of each descriptive observer
#     # end_desc_task <- function() {
#     #   desc_tasks_remaining(max(0, desc_tasks_remaining() - 1))
#     #   if (desc_tasks_remaining() == 0) removeModal()
#     # }
#     end_desc_task <- function() {
#       desc_tasks_remaining(max(0, desc_tasks_remaining() - 1))
#       if (desc_tasks_remaining() == 0) {
#         removeModal()
#         # Only show on a full-extent run, not on refine reprocess
#         if (!isTRUE(upload_module$is_subsetted())) {
#           showModal(draw_instructions_modal)
#         }
#       }
#     }
# 
# 
#     # Capture drawn polygon — fires on each new draw
#     observeEvent(input$eu_map_draw_new_feature, {
#       feat   <- input$eu_map_draw_new_feature
#       coords <- feat$geometry$coordinates[[1]]
#       mat    <- do.call(rbind, lapply(coords, function(p) c(p[[1]], p[[2]])))
#       poly   <- terra::vect(mat, type = "polygon", crs = "EPSG:4326")
#       drawn_poly(poly)
#     })
# 
#     # Clear drawn_poly if the user deletes the shape
#     observeEvent(input$eu_map_draw_deleted_features, {
#       drawn_poly(NULL)
#     })
# 
#     observeEvent(drawn_poly(), {
#       if (is.null(drawn_poly())) {
#         leafletProxy(ns("eu_map")) %>% clearGroup("drawn")
#       }
#     }, ignoreNULL = FALSE)
# 
#     # ========================================================================
#     # Processing
#     # ========================================================================
#     # Process uploaded TIFs when the process_button is clicked
#     observeEvent(process_btn(), {
#       req(isTRUE(upload_module$run_description()))   # guard — only runs if checked
# 
#       begin_desc_task()
# 
#       # reset all reactive values to NULL first
#       rv$countries    <- NULL
#       rv$rainfall     <- NULL
#       rv$temperature  <- NULL
#       rv$elevation    <- NULL
#       rv$typology     <- NULL
# 
#       req(upload_module$run_description())
#       req(upload_module$rasters_original()) # see app.R server for tifs_inputs
#       years <- year_inputs()                # see app.R server for year_inputs
# 
#       # Add drawn_poly store near other reactiveVals in the server
#       drawn_poly <- reactiveVal(NULL)
# 
# 
#       # Description data ------------------------------------------------------
#       # Create a Progress object
#       progress <- shiny::Progress$new(min = 0,
#                                       max = 2)
#       progress$set(message = "extracting data...", value = 0)
# 
#       latest <- upload_module$rasters_original()#[[length(years)]]
# 
#       # 1. country overlap
#       progress$inc(2/6, detail = "countries") # update progress
#       rv$countries <- compute_countries(latest, countries)
# 
#       # 2. temperature
#       progress$inc(3/6,detail = "monthly temperature") # update progress
#       rv$temperature <- compute_temperature(latest)
# 
#       # 3. elevation
#       progress$inc(4/6, detail = "elevation")
#       rv$elevation <- compute_elevation(latest)
# 
#       # 4. global ecosystem typology (get)
#       progress$inc(5/6, detail = "global ecosystem typology")
#       rv$typology <- compute_typology(latest, efg_lookup)
# 
#       # 5. precipitation
#       progress$inc(6/6, detail = "monthly rainfall") # update progress
#       rv$rainfall <- compute_precipitation(latest)
# 
#       # # hide modal once done
#       progress$close()
# 
#       end_desc_task()   # closes modal only when ALL tasks finish
#       # hide modal once done
# 
#     })
# 
#     # ========================================================================
#     # Render the leaflet map
#     # ========================================================================
# 
#     # 0.  Reset when source data changes (new upload, new file selection)
#     observeEvent(upload_module$tifs(), {
#       for (nm in desc_vars) desc_cache[[nm]] <- NULL
#       rv$countries   <- NULL
#       rv$temperature <- NULL
#       rv$elevation   <- NULL
#       rv$typology    <- NULL
#       rv$rainfall    <- NULL
#     }, ignoreNULL = FALSE)
# 
#     # Reset when the description checkbox is toggled off
#     observeEvent(input$run_description, {
#       if (!isTRUE(input$run_description)) {
#         rv$countries   <- NULL
#         rv$temperature <- NULL
#         rv$elevation   <- NULL
#         rv$typology    <- NULL
#         rv$rainfall    <- NULL
#       }
#     }, ignoreNULL = FALSE)
# 
#     # Reset when the subset is TRUE
#     observeEvent(input$run_description, {
#       if (!isTRUE(upload_module$is_subsetted())) {
#         rv$countries   <- NULL
#         rv$temperature <- NULL
#         rv$elevation   <- NULL
#         rv$typology    <- NULL
#         rv$rainfall    <- NULL
#       }
#     }, ignoreNULL = FALSE)
# 
# 
#     # 1. Reset drawn_poly and map when rasters are cleared (new upload)
#     observeEvent(  list(upload_module$rasters(), upload_module$revert_counter()), {
# 
#       if (is.null(upload_module$rasters())) {
#         # New data incoming — clear draw state and show blank base map
#         drawn_poly(NULL)
#         output$eu_map <- renderLeaflet({
#           leaflet() |>
#             addTiles(group = "OSM") %>%
#             addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
#             addLayersControl(
#               baseGroups = c("OSM", "ESRI Satellite"),
#               options = layersControlOptions(collapsed = FALSE)) %>%
#             leaflet.extras::addDrawToolbar(
#               targetGroup         = "drawn",
#               polylineOptions     = FALSE, circleOptions = FALSE,
#               markerOptions       = FALSE, circleMarkerOptions = FALSE,
#               rectangleOptions    = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
#               polygonOptions      = leaflet.extras::drawPolygonOptions(repeatMode  = TRUE),
#               editOptions         = leaflet.extras::editToolbarOptions()
#             )
#         })
#         return()
#       }
# 
#       # 2. Use isolate() for years so year selection doesn't re-render the map
#       output$eu_map <- renderLeaflet({
#         make_fig_distribution_leaflet(
#           r_map  = upload_module$rasters(),
#           r_loss = upload_module$rasters_change(),
#           years  = isolate(upload_module$years())   # ← isolate prevents year-input re-renders
#         )
#       })
#     }, ignoreNULL = FALSE) # this resets teh observer desciption
# 
# 
#     observeEvent(upload_module$is_subsetted(), {
#       if (isTRUE(upload_module$is_subsetted())) {
#         # Subset just applied — cache current full-extent values before they're overwritten
#         # Only cache if not already cached (first subset after this upload)
#         for (nm in desc_vars) {
#           if (is.null(desc_cache[[nm]]) && !is.null(rv[[nm]]))
#             desc_cache[[nm]] <- rv[[nm]]
#         }
#       } else {
#         # Reverted to full extent — restore from cache if available, else let
#         # existing reset logic trigger a recalculation
#         for (nm in desc_vars) {
#           if (!is.null(desc_cache[[nm]]))
#             rv[[nm]] <- desc_cache[[nm]]
#         }
#       }
#     }, ignoreNULL = FALSE)
# 
# 
#     # ── Drawn polygon from map ──────────────────────────────────────────────────
#     drawn_poly <- reactiveVal(NULL)
# 
#     # Replaces both the new_feature and deleted_features observers
#     observeEvent(input$eu_map_draw_all_features, {
#       features <- input$eu_map_draw_all_features$features
# 
#       # No features left (all deleted) — clear
#       if (is.null(features) || length(features) == 0) {
#         drawn_poly(NULL)
#         return()
#       }
# 
#       # Convert every drawn shape to a SpatVector and combine
#       polys <- lapply(features, function(feat) {
#         coords <- feat$geometry$coordinates[[1]]
#         mat    <- do.call(rbind, lapply(coords, function(p) c(p[[1]], p[[2]])))
#         terra::vect(mat, type = "polygon", crs = "EPSG:4326")
#       })
# 
#       drawn_poly(do.call(rbind, polys))   # single multi-polygon SpatVector
# 
# 
#     }, ignoreNULL = FALSE)
# 
# 
# 
#    # =========================================================================
#    # Return reactive outputs
#    # =========================================================================
#     return(list(
#       countries = reactive({rv$countries}),
#       rainfall = reactive({rv$rainfall}),
#       typology = reactive({rv$typology}),
#       temperature = reactive({rv$temperature}),
#       elevation = reactive({rv$elevation}),
#       drawn_poly = drawn_poly
#     ))
# 
#   })
# }
