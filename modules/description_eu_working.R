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
        card_header(tags$strong("Ecosystem Distribution"), tooltip(bs_icon("info-circle"),
                                                         "Distribution of uploaded ecosystem from the most recent mapped year"
        )),
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
        card_header(tags$strong(tags$a(href = "https://global-ecosystems.org/", "Global Ecosystem Typology", target = "_blank")),
                    tooltip(bs_icon("info-circle"), "Global Ecosystem Typology (Levels 1-3) within the ecosystem (> 1% of major or minor occurrence)"
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
                                      year_inputs
                                      ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
          options = layersControlOptions(collapsed = FALSE)
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
    
    
    
    
    # ========================================================================
    # Processing 
    # ========================================================================
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {

      # reset all reactive values to NULL first
      rv$countries    <- NULL
      rv$rainfall     <- NULL
      rv$temperature  <- NULL
      rv$elevation    <- NULL
      rv$typology     <- NULL

      req(upload_module$run_description())
      req(upload_module$rasters_original()) # see app.R server for tifs_inputs
      years <- year_inputs()                # see app.R server for year_inputs


      # Description data ------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0,
                                      max = 2)
      progress$set(message = "extacting data...", value = 0)

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

      # hide modal once done
      progress$close()
    })

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
    
    
    observeEvent(upload_module$rasters(), {
      req(upload_module$rasters())
      
      output$eu_map <- renderLeaflet({
        make_fig_distribution_leaflet(
          r_map  = upload_module$rasters(),
          r_loss = upload_module$rasters_change(),
          years  = upload_module$years()
        )
      })
      
      removeModal()
    })
    
    

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
      elevation = reactive({rv$elevation})
    ))

  })
}
