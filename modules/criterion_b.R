# makeEOO.SpatRaster(), compute_aoo_fast/slow() -> helpers/analysis_functions.R
# thresholds, assess_criterion_b1/b2(), worst_rle_status() -> helpers/rle_assessment.R

# ==============================================================================
# UI
# ==============================================================================
criterion_b_module_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Criterion B",
    # Map for both criterion -------------------------------------------------
    card(
      full_screen = TRUE,
      # card_header("Criterion B1 and B2"),
      card_body(
        leafletOutput(ns("map_B"), height = 300),
        class = "p-0" # remove padding
      )
    ),
    # buttons ----------------------------------------------------------------
    layout_column_wrap(
      width = "250px",
      fill = FALSE,
      actionButton(ns("get_mcp"), "Calculate the Extent of Occurrence (EOO) – Subcriterion B1", width = "100%"),
      actionButton(ns("get_AOO"), "Calculate the Area of Occupancy (AOO) – Subcriterion B2", width = "100%")),
    
    # subcriterion value boxes -----------------------------------------------
    layout_column_wrap(
      width = "250px",
      fill = FALSE,
      uiOutput(ns("criterion_boxes_b1")),  # placeholder for the dynamic box, 
      uiOutput(ns("criterion_boxes_b2"))  # placeholder for the dynamic box
    )
  )
}

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------
criterion_b_module_server <- function(id, rasters_original, year_inputs, rasters, areas,
                                      upload_module, main_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # modal to provide some user info -----------------------------------------
    # 1) prevent the modal from popping up multiple times per session
    modal_shown <- reactiveVal(FALSE)
    
    # 2) model pop up on tab opening 
    observeEvent(main_tab(), {
      # Only fire when Criterion B is selected
      if (identical(main_tab(), "Criterion B")) {
        # If you want it every time, remove the guard:
        if (!modal_shown()) {
          showModal(b_quick_modal)
          modal_shown(TRUE)
        }
      }
    }, ignoreInit = TRUE)
    
    
    # 3) additional help button inside the modal footer
    observeEvent(input$open_help, {
      showModal(b_help_modal)
    })
    
    
    # reactive values to store data --------------------------------------------
    shown_once   <- reactiveVal(FALSE)
    mcp_vectors  <- reactiveVal(NULL)   # stores all MCP SpatVector (projected)
    mcp_table    <- reactiveVal(NULL)   # df summary for B1
    mcp_poly     <- reactiveVal(NULL)   # list of per-year polygons (if you need)
    b2_table     <- reactiveVal(NULL)   # df summary for B2
    b2_poly      <- reactiveVal(NULL)
    aoo_sel_ids  <- reactiveVal(NULL)
    aoo_all_ids  <- reactiveVal(NULL)
    # criterion_b_status <- reactiveVal(NULL)
    
    # reset when new rasters uploaded ------------------------------------------
    observeEvent(rasters(), {
      
      # reset UI and reactive holders -----------------------------------------
      output$criterion_boxes_b1 <- renderUI({ NULL })
      output$criterion_boxes_b2 <- renderUI({ NULL })
      mcp_vectors(NULL)
      mcp_poly(NULL)
      mcp_table(NULL)
      b2_table(NULL)
      b2_poly(NULL)
      aoo_sel_ids(NULL)
      aoo_all_ids(NULL)
      # criterion_b_status(NULL)
      
      # clear map -------------------------------------------------------------
      leafletProxy(ns("map_B")) %>%
        clearShapes() %>%
        clearControls()
    }, ignoreNULL = TRUE)
    
    # initialize map ----------------------------------------------------------
    output$map_B <- renderLeaflet({
      if (is.null(rasters()) || length(rasters()) < 1) {
        leaflet() %>% addTiles()
      } else {
        # ext_r <- as.vector(ext(rasters()[[length(rasters())]])) # get eau extent for mapping zoom
        yrs <- year_inputs()
        latest_year <- max((yrs))
        latest <- rasters()[[length(rasters())]]
        # ext_r  <- as.vector(ext(latest))   # get extent for zoom
        ext_r  <- as.vector(ext(if (is.lonlat(latest)) latest else project(latest, "EPSG:4326")))
        
        leaflet() %>%
          addTiles(group = "OSM") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
          fitBounds(unname(ext_r["xmin"]),
                    unname(ext_r["ymin"]),
                    unname(ext_r["xmax"]),
                    unname(ext_r["ymax"])) %>%
          addRasterImage(latest, opacity = 0.7, colors = "#000000",method  = "ngb",
                         group = paste0(latest_year, " classification")) %>%
          addLayersControl(
            baseGroups    = c("OSM", "ESRI Satellite"),
            overlayGroups = c(paste0(latest_year, " classification")),
            options       = layersControlOptions(collapsed = FALSE)
          )
      }
    })
    
    # Subcriterion B1 (EOO) ---------------------------------------------------
    observeEvent(input$get_mcp, {
      req(rasters())
      yrs <- year_inputs()
      mcp_list <- list()
      
      
      print("calculating MCP/EOO")
      
      progress <- shiny::Progress$new(min = 0, max = length(yrs))
      on.exit({
        progress$close()
        removeModal()
      }, add = TRUE)
      
      # show a simple modal (no module-scoped spinner) ----------------------
      # showModal(modalDialog(
      #   title = "Calculating MCP for EOO",
      #   tagList(p("Please wait...")),
      #   easyClose = FALSE,
      #   footer = NULL
      # ))
      showModal(b1_modal)
      
      # for (i in seq_along(rasters_original())) {
      progress$set(message = "Calculating MCP...", value = 1)
      # r <- rasters_original()[[i]]
      print("running makeEOO function")
      b1 <- assess_criterion_b1(rasters_original(), max(yrs))
      mcp_list[[1]] <- b1$poly
      # }
      
      # store list and combined SpatVector -----------------------------------
      mcp_poly(mcp_list)
      if (length(mcp_list) > 0) {
        mcp_all <- do.call(rbind, mcp_list)
        mcp_vectors(mcp_all)       # <-- set the reactiveVal properly
        
        mcp_table(b1$table)
      }
      
      # update map layers only if mcp_vectors available ----------------------
      if (!is.null(mcp_vectors())) {
        latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
        r_list <- rasters()
        latest  <- r_list[[length(r_list)]]
        
        # Include B2 groups in the controls if AOO has already been calculated
        b2_groups <- if (!is.null(b2_poly())) {
          c("10x10 km grid", "AOO (>1% occupancy)")
        } else {
          character(0)
        }
        all_groups <- c(paste0(latest_year, " classification"),
                        paste0(latest_year, " EOO (MCP)"),
                        b2_groups)
        
        leafletProxy(ns("map_B")) %>%
          clearGroup(paste0(latest_year, " EOO (MCP)")) %>%
          addPolygons(
            data = sf::st_as_sf(terra::project(
              mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
            fillOpacity = 0.3, color = "black", weight = 2,
            group = paste0(latest_year, " EOO (MCP)"),
            label = ~htmltools::HTML(
              paste0("<strong>Year:</strong> ", year,
                     "<br><strong>Area:</strong> ", round(area_km2, 2), " km²")
            ),
            highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
          ) %>%
          clearControls() %>%
          addLayersControl(
            baseGroups    = c("OSM", "ESRI Satellite"),
            overlayGroups = all_groups,
            options       = layersControlOptions(collapsed = FALSE)
          )
      }
      
      # show value box -------------------------------------------------------
      if (!is.null(mcp_table())) {
        b1_box <- make_b_value_boxes(
          title = "Subcriterion B1 - Restricted EOO",
          reactive_data = reactive(mcp_table()),
          status_col = "assessment",
          data_col = "area_km2",
          hover_text = "To qualify as threatened under Criterion B, an ecosystem must also meet at least one of the conditions for continuing
          decline or the number of threat-defined locations.
          See the RLE Criteria for details",#"EOO (km2)",
          type = "b1"
        )
        
        output$criterion_boxes_b1 <- renderUI({
          layout_column_wrap(width = "250px", b1_box())
        })
      }
    })
    
    # Subcriterion B2 (AOO) ---------------------------------------------------
    observeEvent(input$get_AOO, {
      progress <- shiny::Progress$new(min = 0, max = 3)
      on.exit({
        progress$close()
        removeModal()
      }, add = TRUE)
      
      # showModal(modalDialog(
      #   title = "Calculating grids for AOO",
      #   tagList(p("Please wait...")),
      #   easyClose = FALSE,
      #   footer = NULL
      # ))
      showModal(b2_modal)
      
      req(rasters_original(), length(rasters_original()) > 0)
      progress$set(message = "projecting", value = 1)
      
      eau <- rasters_original()[[length(rasters_original())]]
      
      progress$set(message = "computing AOO grid", value = 2)
      
      result <- assess_criterion_b2(eau, method = "fast")
      
      if (is.null(result)) {
        showNotification("No ecosystem cells found in raster.", type = "error")
        return()
      }
      
      progress$set(message = "calculating 1% rule", value = 3)
      
      b2_table(result$table_df)
      
      # Prepare map layers
      dat_all <- result$dat_all
      dat_sel <- result$dat_sel
      
      dat_all$type <- "all"
      dat_sel$type <- "assessment"
      cells10km <- terra::vect(rbind(dat_all, dat_sel))
      b2_poly(cells10km)
      cells_sf <- sf::st_as_sf(b2_poly())
      
      all_ids <- data.frame(year = result$table_df$year,
                            cell_ids = I(list(dat_all$cell_id)))
      sel_ids <- data.frame(year = result$table_df$year,
                            cell_ids = I(list(dat_sel$cell_id)))
      aoo_all_ids(all_ids)
      aoo_sel_ids(sel_ids)
      # leafletProxy(ns("map_B")) %>%
      #   clearGroup("classification") %>%
      #   clearGroup("10x10 km grid") %>%
      #   clearGroup("AOO (>1% occupancy)") %>%
      #   make_fig_b2_layers( dat_all     = dplyr::filter(cells_sf, type == "all"),
      #                       dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
      #                       r_recent    = rasters()[[length(rasters())]],
      #                      latest_year = max(years(), na.rm = TRUE))
      
      
      # Build the full overlay group list — include B1 EOO group if already computed
      # Classification is always on the map (added in renderLeaflet), list it first
      latest_year <- max(year_inputs(), na.rm = TRUE)
      b1_eoo_groups <- if (!is.null(mcp_vectors())) {
        paste0(max(mcp_vectors()$year, na.rm = TRUE), " EOO (MCP)")
      } else {
        character(0)
      }
      b2_groups <- c("10x10 km grid", "AOO (>1% occupancy)")
      all_groups <- c(paste0(latest_year, " classification"),
                      b1_eoo_groups,
                      b2_groups)
      
      leafletProxy(ns("map_B")) %>%
        clearGroup("10x10 km grid") %>%
        clearGroup("AOO (>1% occupancy)") %>%
        make_fig_b2_layers(
          dat_all     = dplyr::filter(cells_sf, type == "all"),
          dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
          r_recent    = rasters()[[length(rasters())]],
          latest_year = max(year_inputs(), na.rm = TRUE)
        ) %>%
        clearControls() %>%
        addLayersControl(
          baseGroups    = c("OSM", "ESRI Satellite"),
          overlayGroups = all_groups,
          options       = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(
          layerId  = "b2_legend",
          position = "bottomright",
          colors   = c("black", "orange"),
          labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
          opacity  = 0.7,
          title    = "AOO layers"
        )
      
      
      # if (!is.null(mcp_vectors())) {
      #   latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
      #   
      #   leafletProxy(ns("map_B")) %>%
      #     clearGroup("10x10 km grid") %>%
      #     clearGroup("AOO (>1% occupancy)") %>%
      #     clearControls() %>%
      #   addPolygons(data = dat_all, group = "10x10 km grid",
      #               fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
      #   addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
      #               fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
      #   addLayersControl(
      #     baseGroups = c("OSM", "ESRI Satellite"),
      #     overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification"), '10x10 km grid', 'AOO (>1% occupancy)'),
      #     options = layersControlOptions(collapsed = FALSE)
      #   ) %>% 
      #     
      #     addLegend(
      #       position = "bottomright",
      #       colors = c("black", "orange"),
      #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
      #       opacity = 0.7,
      #       title = "Polygon Layers"
      #     )
      #   
      # } else {
      #   leafletProxy(ns("map_B")) %>%
      #     clearGroup("10x10 km grid") %>%
      #     clearGroup("AOO (>1% occupancy)") %>%
      #     addPolygons(data = dat_all, group = "10x10 km grid",
      #                 fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
      #     addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
      #                 fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
      #     addLayersControl(
      #       baseGroups = c("OSM", "ESRI Satellite"),
      #       overlayGroups = c( '10x10 km grid', 'AOO (>1% occupancy)'),
      #       options = layersControlOptions(collapsed = FALSE)
      #     ) %>%
      #     
      #     addLegend(
      #       position = "bottomright",
      #       colors = c("black", "orange"),
      #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
      #       opacity = 0.7,
      #       title = "Polygon Layers"
      #     )
      #   
      # }
      
      
      # show value boxes ----------------------------------------------------
      b2_box <- make_b_value_boxes(
        title = "Subcriterion B2 - Restricted AOO",
        reactive_data = reactive(b2_table()),
        status_col = "assessment",
        data_col_1 = "n",
        data_col_2 = "n_exceed",
        hover_text = "To qualify as threatened under Criterion B, an ecosystem must also meet at least one of the conditions for continuing
          decline or the number of threat-defined locations.",
        type = "b2"
      )
      output$criterion_boxes_b2 <- renderUI({
        layout_column_wrap(width = "250px", b2_box())
      })
    })
    
    
    # ==============================================================================
    # Persist assessments so they can be passed to the report.
    # Overall criterion-A status = worst (most severe) across A1, A2a, A2b
    # over all method x scenario combinations.
    # ==============================================================================
    # criterion_b_status  <- worst_rle_status(mcp_table()$assessment, b2_table()$assessment)
    
    criterion_b_status_r <- reactive({
      if (is.null(mcp_table()) && is.null(b2_table())) return(NULL)
      criterion_b_status(mcp_table(), b2_table())
    })
    
    
    # return reactiveVals ----------------------------------------------------
    return(list(
      mcp_table   = mcp_table,
      mcp_poly    = mcp_poly,
      mcp_vectors = mcp_vectors,
      aoo_sel_ids = aoo_sel_ids,
      aoo_all_ids = aoo_all_ids,
      b2_table    = b2_table,
      b2_poly     = b2_poly,
      criterion_b_status   = criterion_b_status_r
      
    ))
  })
}



# -----------------------------------------------------------------------------
# old version of the b2 usign exact_extract - perhaps do this if low resolution > 100m?
# -----------------------------------------------------------------------------
#       eau <- rasters_original()[[length(rasters_original())]]

#       # Compute centre of data extent in WGS84
#       eau_4326 <- project(eau, "EPSG:4326")
#       ext_4326 <- ext(eau_4326)
#       center_lon <- (ext_4326$xmin + ext_4326$xmax) / 2
#       center_lat <- (ext_4326$ymin + ext_4326$ymax) / 2
#       
#       # Build a custom LAEA projection centred on this data
#       grid.prj <- sprintf(
#         "+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
#         center_lat, center_lon
#       )
#       # grid.prj <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#       
#       eau.wcea <- project(eau, grid.prj)
#       
#       progress$set(message = "creating grid", value = 2)
#       grid <- rast(ext(eau.wcea), res = c(10000, 10000), crs = crs(eau.wcea))
#       grid.expanded <- extend(grid, c(2,2))
#       grid.expanded[] <- 0
#       grid.polygons <- as.polygons(grid.expanded, aggregate = FALSE)
#       grid.polygons.sf <- st_as_sf(grid.polygons)
#       grid.polygons.sf$cell_id <- 1:nrow(grid.polygons.sf)
#       
#       aoo_grid_area <- exact_extract(eau.wcea, grid.polygons.sf, 'sum', coverage_area=TRUE, force_df=TRUE)
#       names(aoo_grid_area) <- names(eau.wcea)
#       
#       progress$set(message = "calculating 1% rule", value = 3)
#       aoo_grid_counts <- cbind(cell_id = grid.polygons.sf$cell_id, aoo_grid_area) %>%
#         pivot_longer(cols = -cell_id, names_to = "year", values_to = "area")
#       
#       aoo_grid_counts_cumulative <- aoo_grid_counts %>%
#         group_by(year) %>%
#         arrange(area, .by_group = TRUE) %>%
#         mutate(cum_area = cumsum(area),
#                cum_prop = cum_area / sum(area, na.rm = TRUE)) %>%
#         ungroup()
#       
#       n_cells <- aoo_grid_counts_cumulative %>%
#         group_by(year) %>%
#         summarise(n = sum(area > 0, na.rm = TRUE),
#                   n_exceed = sum(cum_prop > 0.01, na.rm = TRUE))
#       
#       df.eoo <- as.data.frame(n_cells)
#       df.eoo$assessment <- dplyr::case_when(
#         df.eoo$n_exceed < 2  ~ "CR",
#         df.eoo$n_exceed < 20 ~ "EN",
#         df.eoo$n_exceed < 50 ~ "VU",
#         df.eoo$n_exceed < 55 ~ "NT", # hard coded currently, could be related to a user input easily.
#         TRUE                 ~ "LC"
#       )
#       b2_table(df.eoo)
#       
#       # prepare polygons and map layers
#       grid_sf <- grid.polygons.sf %>% st_transform(crs = 4326)
# 
#       
#       all_ids <- aoo_grid_counts_cumulative %>%
#         filter(cum_prop > 0) %>%
#         group_by(year) %>%
#         summarise(cell_ids = list(cell_id))
#       aoo_all_ids(all_ids)
#       
#       sel_ids <- aoo_grid_counts_cumulative %>%
#         filter(cum_prop > 0.01) %>%
#         group_by(year) %>%
#         summarise(cell_ids = list(cell_id))
#       aoo_sel_ids(sel_ids)
#       
#       dat_all <- grid_sf[grid_sf$cell_id %in% all_ids$cell_ids[[1]], ]
#       dat_sel <- grid_sf[grid_sf$cell_id %in% sel_ids$cell_ids[[1]], ]
#       
#       # combine and save the above
#       dat_all$type  <- "all"
#       dat_sel$type  <- "assessment"
#       cells10km <- vect(rbind(dat_all, dat_sel))
#       b2_poly(cells10km)
#       
#       cells_sf <- sf::st_as_sf(b2_poly())
#       # leafletProxy(ns("map_B")) %>%
#       #   clearGroup("classification") %>%
#       #   clearGroup("10x10 km grid") %>%
#       #   clearGroup("AOO (>1% occupancy)") %>%
#       #   make_fig_b2_layers( dat_all     = dplyr::filter(cells_sf, type == "all"),
#       #                       dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
#       #                       r_recent    = rasters()[[length(rasters())]],
#       #                      latest_year = max(years(), na.rm = TRUE))
#       
#       
#       # Build the full overlay group list — include B1 EOO group if already computed
#       # Classification is always on the map (added in renderLeaflet), list it first
#       latest_year <- max(year_inputs(), na.rm = TRUE)
#       b1_eoo_groups <- if (!is.null(mcp_vectors())) {
#         paste0(max(mcp_vectors()$year, na.rm = TRUE), " EOO (MCP)")
#       } else {
#         character(0)
#       }
#       b2_groups <- c("10x10 km grid", "AOO (>1% occupancy)")
#       all_groups <- c(paste0(latest_year, " classification"),
#                       b1_eoo_groups,
#                       b2_groups)


