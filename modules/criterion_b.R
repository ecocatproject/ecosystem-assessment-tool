# modules/criterion_b.R

# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------
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
      actionButton(ns("get_mcp"), "Calculate EOO - Subcriteria B1", width = "100%"),
      actionButton(ns("get_AOO"), "Calculate AOO - Subcriteria B2", width = "100%")),
      
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
        ctr <- crds(centroids(vect(ext(rasters()[[length(rasters())]]))))
        leaflet() %>% addTiles() %>% setView(lng = ctr[1], lat = ctr[2], zoom = 7) %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
          addLayersControl(baseGroups = c("OSM", "ESRI Satellite"),
                           options = layersControlOptions(collapsed = FALSE))
      }
    })
    
    # Subcriterion B1 (EOO) ---------------------------------------------------
    observeEvent(input$get_mcp, {
      req(rasters())
      yrs <- year_inputs()
      mcp_list <- list()
      
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
      
      for (i in seq_along(rasters_original())) {
        progress$set(message = "Calculating MCP...", value = i)
        r <- rasters_original()[[i]]
        eau_poly <- makeEOO.SpatRaster(r, yrs[i])
        if (!is.null(eau_poly)) {
          mcp_list[[i]] <- eau_poly
        }
      }
      
      # store list and combined SpatVector -----------------------------------
      mcp_poly(mcp_list)
      if (length(mcp_list) > 0) {
        mcp_all <- do.call(rbind, mcp_list)
        mcp_vectors(mcp_all)       # <-- set the reactiveVal properly
        
        df <- as.data.frame(mcp_all)[, c("year", "area_km2")]
        df$assessment <- dplyr::case_when(
          df$area_km2 < 2000  ~ "CR",
          df$area_km2 < 20000 ~ "EN",
          df$area_km2 < 50000 ~ "VU",
          df$area_km2 < 55000 ~ "NT", # hard coded currently, could be related to a user input easily.
          TRUE                ~ "LC"
        )
        mcp_table(df)
      }
      
      # update map layers only if mcp_vectors available ----------------------
      if (!is.null(mcp_vectors())) {
        latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
        r_list <- rasters()
        latest  <- r_list[[length(r_list)]]
        
        leafletProxy(ns("map_B")) %>%
          clearGroup(paste0(latest_year, " MCP")) %>%
          clearGroup(paste0(latest_year, " classification")) %>%
          addPolygons(
            data = sf::st_as_sf(terra::project(
              mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
            fillOpacity = 0.3, color = "black", weight = 2,
            group = paste0(latest_year, " MCP"),
            label = ~paste0("Year: ", year, "<br>Area: ", round(area_km2, 2), " km²"),
            highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
          ) %>%
          addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
          addLayersControl(
            baseGroups = c("OSM", "ESRI Satellite"),
            overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification")),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
      # show value box -------------------------------------------------------
      if (!is.null(mcp_table())) {
        b1_box <- make_b_value_boxes(
          title = "Subcriterion B1",
          reactive_data = reactive(mcp_table()),
          status_col = "assessment",
          data_col = "area_km2",
          hover_text = "EOO (km2)",
          type = "single"
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
      grid.prj <- readChar("data/World_Cylindrical_Equal_Area_WKT.txt", file.info("data/World_Cylindrical_Equal_Area_WKT.txt")$size)
      eau.wcea <- project(eau, grid.prj)
      
      progress$set(message = "creating grid", value = 2)
      grid <- rast(ext(eau.wcea), res = c(10000, 10000), crs = crs(eau.wcea))
      grid.expanded <- extend(grid, c(2,2))
      grid.expanded[] <- 0
      grid.polygons <- as.polygons(grid.expanded, aggregate = FALSE)
      grid.polygons.sf <- st_as_sf(grid.polygons)
      grid.polygons.sf$cell_id <- 1:nrow(grid.polygons.sf)
      
      aoo_grid_area <- exact_extract(eau.wcea, grid.polygons.sf, 'sum', coverage_area=TRUE, force_df=TRUE)
      names(aoo_grid_area) <- names(eau.wcea)
      
      progress$set(message = "calculating 1% rule", value = 3)
      aoo_grid_counts <- cbind(cell_id = grid.polygons.sf$cell_id, aoo_grid_area) %>%
        pivot_longer(cols = -cell_id, names_to = "year", values_to = "area")
      
      aoo_grid_counts_cumulative <- aoo_grid_counts %>%
        group_by(year) %>%
        arrange(area, .by_group = TRUE) %>%
        mutate(cum_area = cumsum(area),
               cum_prop = cum_area / sum(area, na.rm = TRUE)) %>%
        ungroup()
      
      n_cells <- aoo_grid_counts_cumulative %>%
        group_by(year) %>%
        summarise(n = sum(area > 0, na.rm = TRUE),
                  n_exceed = sum(cum_prop > 0.01, na.rm = TRUE))
      
      df.eoo <- as.data.frame(n_cells)
      df.eoo$assessment <- dplyr::case_when(
        df.eoo$n_exceed < 2  ~ "CR",
        df.eoo$n_exceed < 20 ~ "EN",
        df.eoo$n_exceed < 50 ~ "VU",
        df.eoo$n_exceed < 55 ~ "NT", # hard coded currently, could be related to a user input easily.
        TRUE                 ~ "LC"
      )
      b2_table(df.eoo)
      
      # prepare polygons and map layers
      grid_sf <- grid.polygons.sf %>% st_transform(crs = 4326)

      
      all_ids <- aoo_grid_counts_cumulative %>%
        filter(cum_prop > 0) %>%
        group_by(year) %>%
        summarise(cell_ids = list(cell_id))
      aoo_all_ids(all_ids)
      
      sel_ids <- aoo_grid_counts_cumulative %>%
        filter(cum_prop > 0.01) %>%
        group_by(year) %>%
        summarise(cell_ids = list(cell_id))
      aoo_sel_ids(sel_ids)
      
      dat_all <- grid_sf[grid_sf$cell_id %in% all_ids$cell_ids[[1]], ]
      dat_sel <- grid_sf[grid_sf$cell_id %in% sel_ids$cell_ids[[1]], ]
      
      # combine and save the above
      cells10km <- vect(rbind(dat_all,dat_sel))
      cells10km$type <- c("all", "assessement")
      b2_poly(cells10km)
      
      
      if (!is.null(mcp_vectors())) {
        latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
        
        leafletProxy(ns("map_B")) %>%
        clearGroup("10x10 km grid") %>%
        clearGroup("AOO (>1% occupancy)") %>%
        addPolygons(data = dat_all, group = "10x10 km grid",
                    fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
        addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
                    fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification"), '10x10 km grid', 'AOO (>1% occupancy)'),
          options = layersControlOptions(collapsed = FALSE)
        ) 
      } else {
        leafletProxy(ns("map_B")) %>%
          clearGroup("10x10 km grid") %>%
          clearGroup("AOO (>1% occupancy)") %>%
          addPolygons(data = dat_all, group = "10x10 km grid",
                      fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
          addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
                      fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
          addLayersControl(
            baseGroups = c("OSM", "ESRI Satellite"),
            overlayGroups = c( '10x10 km grid', 'AOO (>1% occupancy)'),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      

      # show value boxes ----------------------------------------------------
      b2_box <- make_b_value_boxes(
        title = "Subcriterion B2",
        reactive_data = reactive(b2_table()),
        status_col = "assessment",
        data_col_1 = "n",
        data_col_2 = "n_exceed",
        hover_text = "AOO (10x10 km cells)",
        type = "double"
      )
      output$criterion_boxes_b2 <- renderUI({
        layout_column_wrap(width = "250px", b2_box())
      })
    })
    
    # return reactiveVals ----------------------------------------------------
    return(list(
      mcp_table   = mcp_table,
      mcp_poly    = mcp_poly,
      mcp_vectors = mcp_vectors,
      aoo_sel_ids = aoo_sel_ids,
      aoo_all_ids = aoo_all_ids,
      b2_table    = b2_table,
      b2_poly     = b2_poly
    ))
  })
}
