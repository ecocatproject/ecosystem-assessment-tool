# AOO grid calculation — memory-safe and exact for any raster size or CRS.
# Strategy: aggregate raster to ~100K cells (sum of occupied cells per block),
# then project the small aggregated points to LAEA and do grid assignment.
# No full-raster reprojection. No sampling. Every original cell counted via sum.
# compute_aoo_fast <- function(eau) {
# 
#   # 0. Normalize: 0 → NA so only ecosystem cells remain
#   mm <- terra::minmax(eau)
#   if (!any(is.na(mm)) && mm["min", 1] <= 0) {
#     eau <- terra::classify(eau, matrix(c(-Inf, 0, NA), ncol = 3, byrow = TRUE))
#   }
# 
#   # 1. Cell area in km² — cheap arithmetic, avoids expanse() on full raster
#   res_xy <- terra::res(eau)
#   if (terra::is.lonlat(eau)) {
#     ctr_lat       <- (terra::ext(eau)$ymin + terra::ext(eau)$ymax) / 2
#     cell_area_km2 <- res_xy[1] * res_xy[2] * (111.32)^2 * cos(ctr_lat * pi / 180)
#   } else {
#     cell_area_km2 <- prod(res_xy) / 1e6     # m² → km²
#   }
# 
#   # 2. Aggregate to ~100K cells max using fun="sum" — counts occupied cells per block
#   #    This collapses the raster safely before any heavy operations
#   agg_factor <- max(1L, as.integer(ceiling(sqrt(terra::ncell(eau) / 100000))))
#   if (agg_factor > 1) {
#     eau_agg <- terra::aggregate(eau, fact = agg_factor, fun = "sum", na.rm = TRUE)
#   } else {
#     eau_agg <- eau
#   }
#   # Each agg cell value = count of occupied original cells in that block
# 
#   # 3. Extract occupied aggregated cells as points (na.rm skips empty blocks)
#   pts <- terra::as.points(eau_agg, na.rm = TRUE)
#   if (nrow(pts) == 0) return(NULL)
# 
#   # 4. Build LAEA centred on data
#   ext_4326 <- if (terra::is.lonlat(eau)) {
#     terra::ext(eau)
#   } else {
#     terra::ext(terra::project(
#       terra::as.polygons(terra::ext(eau), crs = terra::crs(eau)), "EPSG:4326"))
#   }
#   cx   <- (ext_4326$xmin + ext_4326$xmax) / 2
#   cy   <- (ext_4326$ymin + ext_4326$ymax) / 2
#   laea <- sprintf(
#     "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs", cy, cx)
# 
#   # 5. Project points (small aggregated raster, not full raster)
#   pts_laea <- terra::project(pts, laea)
#   coords   <- terra::crds(pts_laea)
#   agg_vals <- terra::values(pts_laea)[, 1]   # sum of orig occupied cells per agg block
# 
#   # 6. Assign to 10km grid and compute exact coverage area
#   cell_size <- 10000L
#   xi <- floor(coords[, 1] / cell_size)
#   yi <- floor(coords[, 2] / cell_size)
# 
#   # Sum agg_vals per 10km cell to get total original ecosystem cells → exact area
#   cell_df <- data.frame(xi = xi, yi = yi, orig_cells = agg_vals) |>
#     dplyr::group_by(xi, yi) |>
#     dplyr::summarise(total_orig = sum(orig_cells, na.rm = TRUE), .groups = "drop") |>
#     dplyr::mutate(
#       cell_id  = paste(xi, yi, sep = "_"),
#       area_km2 = total_orig * cell_area_km2   # exact ecosystem area in this 10km cell
#     ) |>
#     dplyr::arrange(area_km2) |>
#     dplyr::mutate(
#       cum_area = cumsum(area_km2),
#       cum_prop = cum_area / sum(area_km2)
#     )
# 
#   all_cells <- cell_df
#   sel_cells  <- dplyr::filter(cell_df, cum_prop > 0.01)
# 
#   # 7. Build sf polygons for occupied cells only
#   make_polys_sf <- function(df) {
#     if (nrow(df) == 0)
#       return(sf::st_sf(cell_id = character(0),
#                        geometry = sf::st_sfc(crs = 4326)))
#     polys <- purrr::pmap(df[, c("xi", "yi")], function(xi, yi) {
#       x0 <- xi * cell_size; x1 <- x0 + cell_size
#       y0 <- yi * cell_size; y1 <- y0 + cell_size
#       sf::st_polygon(list(matrix(
#         c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0),
#         ncol = 2, byrow = TRUE)))
#     })
#     sf::st_sf(cell_id  = df$cell_id,
#               geometry = sf::st_sfc(polys, crs = laea)) |>
#       sf::st_transform(4326)
#   }
# 
#   yr <- names(eau)[1]
#   list(
#     dat_all  = dplyr::mutate(make_polys_sf(all_cells), type = "all"),
#     dat_sel  = dplyr::mutate(make_polys_sf(sel_cells),  type = "assessment"),
#     table_df = data.frame(
#       year       = yr,
#       n          = nrow(all_cells),
#       n_exceed   = nrow(sel_cells),
#       assessment = dplyr::case_when(
#         nrow(sel_cells) < 2  ~ "CR",
#         nrow(sel_cells) < 20 ~ "EN",
#         nrow(sel_cells) < 50 ~ "VU",
#         nrow(sel_cells) < 55 ~ "NT",
#         TRUE                 ~ "LC"
#       ),
#       stringsAsFactors = FALSE
#     )
#   )
# }

# AOO grid calculation via rasterize onto a 10km LAEA grid.
# Projects ecosystem points to LAEA, rasterizes to count cells per 10km block,
# then converts occupied blocks back to polygons for display.
compute_aoo_fast <- function(eau) {
  
  # Normalize: 0 → NA
  mm <- terra::minmax(eau)
  if (!any(is.na(mm)) && mm["min", 1] <= 0)
    eau <- terra::classify(eau, matrix(c(-Inf, 0, NA), ncol = 3, byrow = TRUE))
  
  # Cell area in km² — cheap, no expanse() call
  res_xy <- terra::res(eau)
  cell_area_km2 <- if (terra::is.lonlat(eau)) {
    ctr <- mean(c(terra::ext(eau)$ymin, terra::ext(eau)$ymax))
    prod(res_xy) * 111.32^2 * cos(ctr * pi / 180)
  } else {
    prod(res_xy) / 1e6
  }
  
  # LAEA centred on data extent
  ext_ll <- if (terra::is.lonlat(eau)) terra::ext(eau) else
    terra::ext(terra::project(
      terra::as.polygons(terra::ext(eau), crs = terra::crs(eau)), "EPSG:4326"))
  laea <- sprintf(
    "+proj=laea +lat_0=%.4f +lon_0=%.4f +datum=WGS84 +units=m +no_defs",
    mean(c(ext_ll$ymin, ext_ll$ymax)),
    mean(c(ext_ll$xmin, ext_ll$xmax)))
  
  # Extract and project ecosystem points (na.rm keeps only occupied cells)
  pts <- terra::as.points(eau, na.rm = TRUE)
  if (nrow(pts) == 0) return(NULL)
  pts_laea <- terra::project(pts, laea)
  
  # Rasterize onto 10km grid — each cell value = count of ecosystem cells within
  grid   <- terra::rast(terra::ext(pts_laea), res = 10000, crs = laea)
  counts <- terra::rasterize(pts_laea, grid, fun = "sum")
  
  # 1% cumulative threshold (same logic as original)
  vals         <- terra::values(counts, na.rm = TRUE)
  areas        <- sort(vals * cell_area_km2)
  cum_props    <- cumsum(areas) / sum(areas)
  thresh_n     <- if (any(cum_props > 0.01))
    areas[which(cum_props > 0.01)[1]] / cell_area_km2 else 0
  
  # Polygons: terra builds 10km squares, sf transforms for leaflet
  polys_all <- terra::as.polygons(counts) |>
    sf::st_as_sf() |> sf::st_transform(4326) |> dplyr::mutate(type = "all")
  
  polys_sel <- terra::as.polygons(terra::ifel(counts >= thresh_n, counts, NA)) |>
    sf::st_as_sf() |> sf::st_transform(4326) |> dplyr::mutate(type = "assessment")
  
  yr <- names(eau)[1]
  list(
    dat_all  = polys_all,
    dat_sel  = polys_sel,
    table_df = data.frame(
      year       = yr,
      n          = nrow(polys_all),
      n_exceed   = nrow(polys_sel),
      assessment = dplyr::case_when(
        nrow(polys_sel) < 2  ~ "CR",
        nrow(polys_sel) < 20 ~ "EN",
        nrow(polys_sel) < 50 ~ "VU",
        nrow(polys_sel) < 55 ~ "NT",
        TRUE                 ~ "LC"
      ),
      stringsAsFactors = FALSE
    )
  )
}


# ==============================================================================
# Helper: most severe (worst-case) IUCN RLE status across a vector of codes
# CR > EN > VU > NT > LC. Unknown / NA values are dropped. Returns NA when
# nothing valid is supplied.
# ==============================================================================
worst_rle_status <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[!is.na(vals)]
  assessment <- c(CR = 1, EN = 2, VU = 3, NT = 4, LC = 5)
  vals <- vals[vals %in% names(assessment)]
  if (length(vals) == 0) return(NA_character_)
  names(which.min(assessment[vals]))
}



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
        ext_r  <- as.vector(ext(latest))   # get extent for zoom
        
        leaflet() %>%
          addTiles(group = "OSM") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
          fitBounds(unname(ext_r["xmin"]),
                    unname(ext_r["ymin"]),
                    unname(ext_r["xmax"]),
                    unname(ext_r["ymax"])) %>%
          addRasterImage(latest, opacity = 0.7, colors = "#000000",
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
      
      print(yrs)
      str(rasters_original())
      str(rasters())
      print(rasters())
      print(rasters_original())
      
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
      eau_poly <- makeEOO.SpatRaster(rasters_original(), max(yrs))
      if (!is.null(eau_poly)) {
        mcp_list[[1]] <- eau_poly
      }
      # }
      
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
          hover_text = "EOO (km2)",
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
      
      result <- compute_aoo_fast(eau)
      
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
        hover_text = "AOO (10x10 km cells)",
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
    
    criterion_b_status <- reactive({
      b1 <- mcp_table()
      b2 <- b2_table()
      if (is.null(b1) && is.null(b2)) return(NULL)
      
      latest_assessment <- function(df) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
            !all(c("year", "assessment") %in% names(df))) return(character())
        df$assessment[which.max(as.numeric(df$year))]
      }
      
      worst_rle_status(latest_assessment(b1), latest_assessment(b2))
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
      criterion_b_status   = criterion_b_status
      
    ))
  })
}
# # modules/criterion_b.R
# 
# # -----------------------------------------------------------------------------
# # ui
# # -----------------------------------------------------------------------------
# criterion_b_module_ui <- function(id) {
#   ns <- NS(id)
#     nav_panel(
#     "Criterion B",
#     # Map for both criterion -------------------------------------------------
#     card(
#       full_screen = TRUE,
#      # card_header("Criterion B1 and B2"),
#       card_body(
#         leafletOutput(ns("map_B"), height = 300),
#         class = "p-0" # remove padding
#       )
#     ),
#     # buttons ----------------------------------------------------------------
#     layout_column_wrap(
#       width = "250px",
#       fill = FALSE,
#       actionButton(ns("get_mcp"), "Calculate the Extent of Occurrence (EOO) – Subcriterion B1", width = "100%"),
#       actionButton(ns("get_AOO"), "Calculate the Area of Occupancy (AOO) – Subcriterion B2", width = "100%")),
#       
#     # subcriterion value boxes -----------------------------------------------
#     layout_column_wrap(
#       width = "250px",
#       fill = FALSE,
#       uiOutput(ns("criterion_boxes_b1")),  # placeholder for the dynamic box, 
#       uiOutput(ns("criterion_boxes_b2"))  # placeholder for the dynamic box
#     )
#   )
# }
# 
# # -----------------------------------------------------------------------------
# # server
# # -----------------------------------------------------------------------------
# criterion_b_module_server <- function(id, rasters_original, year_inputs, rasters, areas,
#                                       upload_module, main_tab) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # modal to provide some user info -----------------------------------------
#     # 1) prevent the modal from popping up multiple times per session
#     modal_shown <- reactiveVal(FALSE)
#                                
#     # 2) model pop up on tab opening 
#     observeEvent(main_tab(), {
#       # Only fire when Criterion B is selected
#       if (identical(main_tab(), "Criterion B")) {
#         # If you want it every time, remove the guard:
#         if (!modal_shown()) {
#           showModal(b_quick_modal)
#           modal_shown(TRUE)
#         }
#       }
#     }, ignoreInit = TRUE)
#     
#     
#     # 3) additional help button inside the modal footer
#     observeEvent(input$open_help, {
#       showModal(b_help_modal)
#     })
#     
#     
#     # reactive values to store data --------------------------------------------
#     shown_once   <- reactiveVal(FALSE)
#     mcp_vectors  <- reactiveVal(NULL)   # stores all MCP SpatVector (projected)
#     mcp_table    <- reactiveVal(NULL)   # df summary for B1
#     mcp_poly     <- reactiveVal(NULL)   # list of per-year polygons (if you need)
#     b2_table     <- reactiveVal(NULL)   # df summary for B2
#     b2_poly      <- reactiveVal(NULL)
#     aoo_sel_ids  <- reactiveVal(NULL)
#     aoo_all_ids  <- reactiveVal(NULL)
#     
#     # reset when new rasters uploaded ------------------------------------------
#     observeEvent(rasters(), {
#       
#       # reset UI and reactive holders -----------------------------------------
#       output$criterion_boxes_b1 <- renderUI({ NULL })
#       output$criterion_boxes_b2 <- renderUI({ NULL })
#       mcp_vectors(NULL)
#       mcp_poly(NULL)
#       mcp_table(NULL)
#       b2_table(NULL)
#       b2_poly(NULL)
#       aoo_sel_ids(NULL)
#       aoo_all_ids(NULL)
#       
#       # clear map -------------------------------------------------------------
#       leafletProxy(ns("map_B")) %>%
#         clearShapes() %>%
#         clearControls()
#     }, ignoreNULL = TRUE)
#     
#     # initialize map ----------------------------------------------------------
#     output$map_B <- renderLeaflet({
#       if (is.null(rasters()) || length(rasters()) < 1) {
#         leaflet() %>% addTiles()
#       } else {
#         # ext_r <- as.vector(ext(rasters()[[length(rasters())]])) # get eau extent for mapping zoom
#         yrs <- year_inputs()
#         latest_year <- max((yrs))
#         latest <- rasters()[[length(rasters())]]
#         ext_r  <- as.vector(ext(latest))   # get extent for zoom
#         
#         leaflet() %>% 
#           addTiles(group = "OSM") %>%
#           addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
#           fitBounds(unname(ext_r["xmin"]),
#                     unname(ext_r["ymin"]),
#                     unname(ext_r["xmax"]),
#                     unname(ext_r["ymax"])) %>%
#           addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
#           addLayersControl(baseGroups = c("OSM", "ESRI Satellite"),
#                            overlayGroups = c(paste0(latest_year, " classification")),
#                            options = layersControlOptions(collapsed = FALSE))
#       }
#     })
#     
#     # Subcriterion B1 (EOO) ---------------------------------------------------
#     observeEvent(input$get_mcp, {
#       req(rasters())
#       yrs <- year_inputs()
#       mcp_list <- list()
#       
#       print(yrs)
#       str(rasters_original())
#       str(rasters())
#       print(rasters())
#       print(rasters_original())
#       
#       progress <- shiny::Progress$new(min = 0, max = length(yrs))
#       on.exit({
#         progress$close()
#         removeModal()
#       }, add = TRUE)
#       
#       # show a simple modal (no module-scoped spinner) ----------------------
#       # showModal(modalDialog(
#       #   title = "Calculating MCP for EOO",
#       #   tagList(p("Please wait...")),
#       #   easyClose = FALSE,
#       #   footer = NULL
#       # ))
#       showModal(b1_modal)
#       
#      # for (i in seq_along(rasters_original())) {
#         progress$set(message = "Calculating MCP...", value = 1)
#        # r <- rasters_original()[[i]]
#         eau_poly <- makeEOO.SpatRaster(rasters_original(), max(yrs))
#         if (!is.null(eau_poly)) {
#           mcp_list[[1]] <- eau_poly
#         }
#      # }
#       
#       # store list and combined SpatVector -----------------------------------
#       mcp_poly(mcp_list)
#       if (length(mcp_list) > 0) {
#         mcp_all <- do.call(rbind, mcp_list)
#         mcp_vectors(mcp_all)       # <-- set the reactiveVal properly
#         
#         df <- as.data.frame(mcp_all)[, c("year", "area_km2")]
#         df$assessment <- dplyr::case_when(
#           df$area_km2 < 2000  ~ "CR",
#           df$area_km2 < 20000 ~ "EN",
#           df$area_km2 < 50000 ~ "VU",
#           df$area_km2 < 55000 ~ "NT", # hard coded currently, could be related to a user input easily.
#           TRUE                ~ "LC"
#         )
#         mcp_table(df)
#       }
#       
#       # update map layers only if mcp_vectors available ----------------------
#       if (!is.null(mcp_vectors())) {
#         latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
#         r_list <- rasters()
#         latest  <- r_list[[length(r_list)]]
#         
#         # Include B2 groups in the controls if AOO has already been calculated
#         b2_groups <- if (!is.null(b2_poly())) {
#           c("10x10 km grid", "AOO (>1% occupancy)")
#         } else {
#           character(0)
#         }
#         all_groups <- c(paste0(latest_year, " classification"),
#                         paste0(latest_year, " EOO (MCP)"),
#                         b2_groups)
#         
#         leafletProxy(ns("map_B")) %>%
#           clearGroup(paste0(latest_year, " EOO (MCP)")) %>%
#           # clearGroup(paste0(latest_year, " classification")) %>%
#           addPolygons(
#             data = sf::st_as_sf(terra::project(
#               mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
#             fillOpacity = 0.3, color = "black", weight = 2,
#             group = paste0(latest_year, " EOO (MCP)"),
#             label = ~htmltools::HTML(
#               paste0("<strong>Year:</strong> ", year,
#                      "<br><strong>Area:</strong> ", round(area_km2, 2), " km²")
#             ),
#             highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
#           ) %>%
#           addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
#           addLayersControl(
#             baseGroups = c("OSM", "ESRI Satellite"),
#             overlayGroups = all_groups,
#             options = layersControlOptions(collapsed = FALSE)
#           )
#       }
#       
#       # show value box -------------------------------------------------------
#       if (!is.null(mcp_table())) {
#         b1_box <- make_b_value_boxes(
#           title = "Subcriterion B1 - Restricted EOO",
#           reactive_data = reactive(mcp_table()),
#           status_col = "assessment",
#           data_col = "area_km2",
#           hover_text = "EOO (km2)",
#           type = "b1"
#         )
#         
#         output$criterion_boxes_b1 <- renderUI({
#           layout_column_wrap(width = "250px", b1_box())
#         })
#       }
#     })
#     
#     # Subcriterion B2 (AOO) ---------------------------------------------------
#     observeEvent(input$get_AOO, {
#       progress <- shiny::Progress$new(min = 0, max = 3)
#       on.exit({
#         progress$close()
#         removeModal()
#       }, add = TRUE)
#       
#       # showModal(modalDialog(
#       #   title = "Calculating grids for AOO",
#       #   tagList(p("Please wait...")),
#       #   easyClose = FALSE,
#       #   footer = NULL
#       # ))
#       showModal(b2_modal)
#       
#       req(rasters_original(), length(rasters_original()) > 0)
#       progress$set(message = "projecting", value = 1)
#       
#       eau <- rasters_original()[[length(rasters_original())]]
#       # grid.prj <- readChar("data/World_Cylindrical_Equal_Area_WKT.txt", file.info("data/World_Cylindrical_Equal_Area_WKT.txt")$size)
#       # eau.wcea <- project(eau, grid.prj)
#       
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
#       
#       eau.wcea <- project(eau, grid.prj)
#       
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
#       
#       leafletProxy(ns("map_B")) %>%
#         clearGroup("10x10 km grid") %>%
#         clearGroup("AOO (>1% occupancy)") %>%
#         make_fig_b2_layers(
#           dat_all     = dplyr::filter(cells_sf, type == "all"),
#           dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
#           r_recent    = rasters()[[length(rasters())]],
#           latest_year = max(year_inputs(), na.rm = TRUE)
#         ) %>%
#         clearControls() %>%
#         addLayersControl(
#           baseGroups    = c("OSM", "ESRI Satellite"),
#           overlayGroups = all_groups,
#           options       = layersControlOptions(collapsed = FALSE)
#         ) %>%
#         addLegend(
#           layerId  = "b2_legend",
#           position = "bottomright",
#           colors   = c("black", "orange"),
#           labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
#           opacity  = 0.7,
#           title    = "AOO layers"
#         )
#       
#       
#       # if (!is.null(mcp_vectors())) {
#       #   latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
#       #   
#       #   leafletProxy(ns("map_B")) %>%
#       #     clearGroup("10x10 km grid") %>%
#       #     clearGroup("AOO (>1% occupancy)") %>%
#       #     clearControls() %>%
#       #   addPolygons(data = dat_all, group = "10x10 km grid",
#       #               fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
#       #   addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
#       #               fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
#       #   addLayersControl(
#       #     baseGroups = c("OSM", "ESRI Satellite"),
#       #     overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification"), '10x10 km grid', 'AOO (>1% occupancy)'),
#       #     options = layersControlOptions(collapsed = FALSE)
#       #   ) %>% 
#       #     
#       #     addLegend(
#       #       position = "bottomright",
#       #       colors = c("black", "orange"),
#       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
#       #       opacity = 0.7,
#       #       title = "Polygon Layers"
#       #     )
#       #   
#       # } else {
#       #   leafletProxy(ns("map_B")) %>%
#       #     clearGroup("10x10 km grid") %>%
#       #     clearGroup("AOO (>1% occupancy)") %>%
#       #     addPolygons(data = dat_all, group = "10x10 km grid",
#       #                 fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
#       #     addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
#       #                 fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
#       #     addLayersControl(
#       #       baseGroups = c("OSM", "ESRI Satellite"),
#       #       overlayGroups = c( '10x10 km grid', 'AOO (>1% occupancy)'),
#       #       options = layersControlOptions(collapsed = FALSE)
#       #     ) %>%
#       #     
#       #     addLegend(
#       #       position = "bottomright",
#       #       colors = c("black", "orange"),
#       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
#       #       opacity = 0.7,
#       #       title = "Polygon Layers"
#       #     )
#       #   
#       # }
#       
# 
#       # show value boxes ----------------------------------------------------
#       b2_box <- make_b_value_boxes(
#         title = "Subcriterion B2 - Restricted AOO",
#         reactive_data = reactive(b2_table()),
#         status_col = "assessment",
#         data_col_1 = "n",
#         data_col_2 = "n_exceed",
#         hover_text = "AOO (10x10 km cells)",
#         type = "b2"
#       )
#       output$criterion_boxes_b2 <- renderUI({
#         layout_column_wrap(width = "250px", b2_box())
#       })
#     })
#     
#     # return reactiveVals ----------------------------------------------------
#     return(list(
#       mcp_table   = mcp_table,
#       mcp_poly    = mcp_poly,
#       mcp_vectors = mcp_vectors,
#       aoo_sel_ids = aoo_sel_ids,
#       aoo_all_ids = aoo_all_ids,
#       b2_table    = b2_table,
#       b2_poly     = b2_poly
#     ))
#   })
# }
# # modules/criterion_b.R
# 
# # -----------------------------------------------------------------------------
# # ui
# # -----------------------------------------------------------------------------
# criterion_b_module_ui <- function(id) {
#   ns <- NS(id)
#     nav_panel(
#     "Criterion B",
#     # Map for both criterion -------------------------------------------------
#     card(
#       full_screen = TRUE,
#      # card_header("Criterion B1 and B2"),
#       card_body(
#         leafletOutput(ns("map_B"), height = 300),
#         class = "p-0" # remove padding
#       )
#     ),
#     # buttons ----------------------------------------------------------------
#     layout_column_wrap(
#       width = "250px",
#       fill = FALSE,
#       actionButton(ns("get_mcp"), "Calculate the Extent of Occurrence (EOO) – Subcriterion B1", width = "100%"),
#       actionButton(ns("get_AOO"), "Calculate the Area of Occupancy (AOO) – Subcriterion B2", width = "100%")),
#       
#     # subcriterion value boxes -----------------------------------------------
#     layout_column_wrap(
#       width = "250px",
#       fill = FALSE,
#       uiOutput(ns("criterion_boxes_b1")),  # placeholder for the dynamic box, 
#       uiOutput(ns("criterion_boxes_b2"))  # placeholder for the dynamic box
#     )
#   )
# }
# 
# # -----------------------------------------------------------------------------
# # server
# # -----------------------------------------------------------------------------
# criterion_b_module_server <- function(id, rasters_original, year_inputs, rasters, areas,
#                                       upload_module, main_tab) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # modal to provide some user info -----------------------------------------
#     # 1) prevent the modal from popping up multiple times per session
#     modal_shown <- reactiveVal(FALSE)
#                                
#     # 2) model pop up on tab opening 
#     observeEvent(main_tab(), {
#       # Only fire when Criterion B is selected
#       if (identical(main_tab(), "Criterion B")) {
#         # If you want it every time, remove the guard:
#         if (!modal_shown()) {
#           showModal(b_quick_modal)
#           modal_shown(TRUE)
#         }
#       }
#     }, ignoreInit = TRUE)
#     
#     
#     # 3) additional help button inside the modal footer
#     observeEvent(input$open_help, {
#       showModal(b_help_modal)
#     })
#     
#     
#     # reactive values to store data --------------------------------------------
#     shown_once   <- reactiveVal(FALSE)
#     mcp_vectors  <- reactiveVal(NULL)   # stores all MCP SpatVector (projected)
#     mcp_table    <- reactiveVal(NULL)   # df summary for B1
#     mcp_poly     <- reactiveVal(NULL)   # list of per-year polygons (if you need)
#     b2_table     <- reactiveVal(NULL)   # df summary for B2
#     b2_poly      <- reactiveVal(NULL)
#     aoo_sel_ids  <- reactiveVal(NULL)
#     aoo_all_ids  <- reactiveVal(NULL)
#     
#     # reset when new rasters uploaded ------------------------------------------
#     observeEvent(rasters(), {
#       
#       # reset UI and reactive holders -----------------------------------------
#       output$criterion_boxes_b1 <- renderUI({ NULL })
#       output$criterion_boxes_b2 <- renderUI({ NULL })
#       mcp_vectors(NULL)
#       mcp_poly(NULL)
#       mcp_table(NULL)
#       b2_table(NULL)
#       b2_poly(NULL)
#       aoo_sel_ids(NULL)
#       aoo_all_ids(NULL)
#       
#       # clear map -------------------------------------------------------------
#       leafletProxy(ns("map_B")) %>%
#         clearShapes() %>%
#         clearControls()
#     }, ignoreNULL = TRUE)
#     
#     # initialize map ----------------------------------------------------------
#     output$map_B <- renderLeaflet({
#       if (is.null(rasters()) || length(rasters()) < 1) {
#         leaflet() %>% addTiles()
#       } else {
#         # ext_r <- as.vector(ext(rasters()[[length(rasters())]])) # get eau extent for mapping zoom
#         yrs <- year_inputs()
#         latest_year <- max((yrs))
#         latest <- rasters()[[length(rasters())]]
#         ext_r  <- as.vector(ext(latest))   # get extent for zoom
#         
#         leaflet() %>% 
#           addTiles(group = "OSM") %>%
#           addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
#           fitBounds(unname(ext_r["xmin"]),
#                     unname(ext_r["ymin"]),
#                     unname(ext_r["xmax"]),
#                     unname(ext_r["ymax"])) %>%
#           addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
#           addLayersControl(baseGroups = c("OSM", "ESRI Satellite"),
#                            overlayGroups = c(paste0(latest_year, " classification")),
#                            options = layersControlOptions(collapsed = FALSE))
#       }
#     })
#     
#     # Subcriterion B1 (EOO) ---------------------------------------------------
#     observeEvent(input$get_mcp, {
#       req(rasters())
#       yrs <- year_inputs()
#       mcp_list <- list()
#       
#       print(yrs)
#       str(rasters_original())
#       str(rasters())
#       print(rasters())
#       print(rasters_original())
#       
#       progress <- shiny::Progress$new(min = 0, max = length(yrs))
#       on.exit({
#         progress$close()
#         removeModal()
#       }, add = TRUE)
#       
#       # show a simple modal (no module-scoped spinner) ----------------------
#       # showModal(modalDialog(
#       #   title = "Calculating MCP for EOO",
#       #   tagList(p("Please wait...")),
#       #   easyClose = FALSE,
#       #   footer = NULL
#       # ))
#       showModal(b1_modal)
#       
#      # for (i in seq_along(rasters_original())) {
#         progress$set(message = "Calculating MCP...", value = 1)
#        # r <- rasters_original()[[i]]
#         eau_poly <- makeEOO.SpatRaster(rasters_original(), max(yrs))
#         if (!is.null(eau_poly)) {
#           mcp_list[[1]] <- eau_poly
#         }
#      # }
#       
#       # store list and combined SpatVector -----------------------------------
#       mcp_poly(mcp_list)
#       if (length(mcp_list) > 0) {
#         mcp_all <- do.call(rbind, mcp_list)
#         mcp_vectors(mcp_all)       # <-- set the reactiveVal properly
#         
#         df <- as.data.frame(mcp_all)[, c("year", "area_km2")]
#         df$assessment <- dplyr::case_when(
#           df$area_km2 < 2000  ~ "CR",
#           df$area_km2 < 20000 ~ "EN",
#           df$area_km2 < 50000 ~ "VU",
#           df$area_km2 < 55000 ~ "NT", # hard coded currently, could be related to a user input easily.
#           TRUE                ~ "LC"
#         )
#         mcp_table(df)
#       }
#       
#       # update map layers only if mcp_vectors available ----------------------
#       if (!is.null(mcp_vectors())) {
#         latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
#         r_list <- rasters()
#         latest  <- r_list[[length(r_list)]]
#         
#         # Include B2 groups in the controls if AOO has already been calculated
#         b2_groups <- if (!is.null(b2_poly())) {
#           c("10x10 km grid", "AOO (>1% occupancy)")
#         } else {
#           character(0)
#         }
#         all_groups <- c(paste0(latest_year, " classification"),
#                         paste0(latest_year, " EOO (MCP)"),
#                         b2_groups)
#         
#         leafletProxy(ns("map_B")) %>%
#           clearGroup(paste0(latest_year, " EOO (MCP)")) %>%
#           # clearGroup(paste0(latest_year, " classification")) %>%
#           addPolygons(
#             data = sf::st_as_sf(terra::project(
#               mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
#             fillOpacity = 0.3, color = "black", weight = 2,
#             group = paste0(latest_year, " EOO (MCP)"),
#             label = ~htmltools::HTML(
#               paste0("<strong>Year:</strong> ", year,
#                      "<br><strong>Area:</strong> ", round(area_km2, 2), " km²")
#             ),
#             highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
#           ) %>%
#           addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
#           addLayersControl(
#             baseGroups = c("OSM", "ESRI Satellite"),
#             overlayGroups = all_groups,
#             options = layersControlOptions(collapsed = FALSE)
#           )
#       }
#       
#       # show value box -------------------------------------------------------
#       if (!is.null(mcp_table())) {
#         b1_box <- make_b_value_boxes(
#           title = "Subcriterion B1 - Restricted EOO",
#           reactive_data = reactive(mcp_table()),
#           status_col = "assessment",
#           data_col = "area_km2",
#           hover_text = "EOO (km2)",
#           type = "b1"
#         )
#         
#         output$criterion_boxes_b1 <- renderUI({
#           layout_column_wrap(width = "250px", b1_box())
#         })
#       }
#     })
#     
#     # Subcriterion B2 (AOO) ---------------------------------------------------
#     observeEvent(input$get_AOO, {
#       progress <- shiny::Progress$new(min = 0, max = 3)
#       on.exit({
#         progress$close()
#         removeModal()
#       }, add = TRUE)
#       
#       # showModal(modalDialog(
#       #   title = "Calculating grids for AOO",
#       #   tagList(p("Please wait...")),
#       #   easyClose = FALSE,
#       #   footer = NULL
#       # ))
#       showModal(b2_modal)
#       
#       req(rasters_original(), length(rasters_original()) > 0)
#       progress$set(message = "projecting", value = 1)
#       
#       eau <- rasters_original()[[length(rasters_original())]]
#       # grid.prj <- readChar("data/World_Cylindrical_Equal_Area_WKT.txt", file.info("data/World_Cylindrical_Equal_Area_WKT.txt")$size)
#       # eau.wcea <- project(eau, grid.prj)
#       
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
#       
#       eau.wcea <- project(eau, grid.prj)
#       
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
#       
#       leafletProxy(ns("map_B")) %>%
#         clearGroup("10x10 km grid") %>%
#         clearGroup("AOO (>1% occupancy)") %>%
#         make_fig_b2_layers(
#           dat_all     = dplyr::filter(cells_sf, type == "all"),
#           dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
#           r_recent    = rasters()[[length(rasters())]],
#           latest_year = max(year_inputs(), na.rm = TRUE)
#         ) %>%
#         clearControls() %>%
#         addLayersControl(
#           baseGroups    = c("OSM", "ESRI Satellite"),
#           overlayGroups = all_groups,
#           options       = layersControlOptions(collapsed = FALSE)
#         ) %>%
#         addLegend(
#           layerId  = "b2_legend",
#           position = "bottomright",
#           colors   = c("black", "orange"),
#           labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
#           opacity  = 0.7,
#           title    = "AOO layers"
#         )
#       
#       
#       # if (!is.null(mcp_vectors())) {
#       #   latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
#       #   
#       #   leafletProxy(ns("map_B")) %>%
#       #     clearGroup("10x10 km grid") %>%
#       #     clearGroup("AOO (>1% occupancy)") %>%
#       #     clearControls() %>%
#       #   addPolygons(data = dat_all, group = "10x10 km grid",
#       #               fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
#       #   addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
#       #               fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
#       #   addLayersControl(
#       #     baseGroups = c("OSM", "ESRI Satellite"),
#       #     overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification"), '10x10 km grid', 'AOO (>1% occupancy)'),
#       #     options = layersControlOptions(collapsed = FALSE)
#       #   ) %>% 
#       #     
#       #     addLegend(
#       #       position = "bottomright",
#       #       colors = c("black", "orange"),
#       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
#       #       opacity = 0.7,
#       #       title = "Polygon Layers"
#       #     )
#       #   
#       # } else {
#       #   leafletProxy(ns("map_B")) %>%
#       #     clearGroup("10x10 km grid") %>%
#       #     clearGroup("AOO (>1% occupancy)") %>%
#       #     addPolygons(data = dat_all, group = "10x10 km grid",
#       #                 fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
#       #     addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
#       #                 fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
#       #     addLayersControl(
#       #       baseGroups = c("OSM", "ESRI Satellite"),
#       #       overlayGroups = c( '10x10 km grid', 'AOO (>1% occupancy)'),
#       #       options = layersControlOptions(collapsed = FALSE)
#       #     ) %>%
#       #     
#       #     addLegend(
#       #       position = "bottomright",
#       #       colors = c("black", "orange"),
#       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
#       #       opacity = 0.7,
#       #       title = "Polygon Layers"
#       #     )
#       #   
#       # }
#       
# 
#       # show value boxes ----------------------------------------------------
#       b2_box <- make_b_value_boxes(
#         title = "Subcriterion B2 - Restricted AOO",
#         reactive_data = reactive(b2_table()),
#         status_col = "assessment",
#         data_col_1 = "n",
#         data_col_2 = "n_exceed",
#         hover_text = "AOO (10x10 km cells)",
#         type = "b2"
#       )
#       output$criterion_boxes_b2 <- renderUI({
#         layout_column_wrap(width = "250px", b2_box())
#       })
#     })
#     
#     # return reactiveVals ----------------------------------------------------
#     return(list(
#       mcp_table   = mcp_table,
#       mcp_poly    = mcp_poly,
#       mcp_vectors = mcp_vectors,
#       aoo_sel_ids = aoo_sel_ids,
#       aoo_all_ids = aoo_all_ids,
#       b2_table    = b2_table,
#       b2_poly     = b2_poly
#     ))
#   })
# }
# # modules/criterion_b.R
# 
# # -----------------------------------------------------------------------------
# # ui
# # -----------------------------------------------------------------------------
# # Fast AOO grid calculation using point sampling — avoids full-raster reprojection.
# # Projects N points (fast) instead of M×N raster cells (slow).
# # Returns list: dat_all, dat_sel (sf polygons in EPSG:4326), and summary table.
# compute_aoo_fast <- function(eau, n_sample = 500000) {
#   
#   # Ensure only ecosystem cells are considered (classify 0 → NA if needed)
#   if (!is.na(terra::minmax(eau)["min", ]) && terra::minmax(eau)["min", ] <= 0) {
#     eau <- terra::classify(eau, matrix(c(-Inf, 0, NA), ncol = 3, byrow = TRUE))
#   }
#   
#   # 1. Sample ecosystem points in native CRS (cheap — no reprojection yet)
#   # pts <- terra::spatSample(eau, n_sample, method = "regular",
#   #                          as.points = TRUE, na.rm = TRUE)
#   # if (nrow(pts) == 0) return(NULL)
#   pts <- as.points(eau)
#   
#   # 2. LAEA projection centred on data — minimal distortion anywhere on Earth
#   ext_4326 <- if (terra::is.lonlat(eau)) {
#     terra::ext(eau)
#   } else {
#     terra::ext(terra::project(
#       terra::as.polygons(terra::ext(eau), crs = terra::crs(eau)), "EPSG:4326"))
#   }
#   cx   <- (ext_4326$xmin + ext_4326$xmax) / 2
#   cy   <- (ext_4326$ymin + ext_4326$ymax) / 2
#   laea <- sprintf(
#     "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs", cy, cx)
#   
#   # 3. Project only the sample points — thousands not millions of operations
#   pts_laea <- terra::project(pts, laea)
#   coords   <- terra::crds(pts_laea)
#   
#   # 4. Total ecosystem area for coverage estimation
#   total_area_km2 <- sum(terra::expanse(eau, unit = "km"), na.rm = TRUE)
#   area_per_pt    <- total_area_km2 / nrow(pts)   # km² each point represents
#   
#   # 5. Assign each point to a 10km grid cell via integer arithmetic (instant)
#   cell_size <- 10000L
#   xi      <- floor(coords[, 1] / cell_size)
#   yi      <- floor(coords[, 2] / cell_size)
#   
#   # 6. Aggregate: points per cell → coverage area
#   cell_df <- data.frame(xi = xi, yi = yi) |>
#     dplyr::group_by(xi, yi) |>
#     dplyr::summarise(n_pts = dplyr::n(), .groups = "drop") |>
#     dplyr::mutate(
#       cell_id  = paste(xi, yi, sep = "_"),
#       area_km2 = n_pts * area_per_pt
#     ) |>
#     dplyr::arrange(area_km2) |>
#     dplyr::mutate(
#       cum_area = cumsum(area_km2),
#       cum_prop = cum_area / sum(area_km2)
#     )
#   
#   all_cells <- cell_df                                   # all occupied cells
#   sel_cells <- dplyr::filter(cell_df, cum_prop > 0.01)  # >1% cumulative threshold
#   
#   # 7. Build sf polygons only for occupied cells (far fewer than full grid)
#   make_polys_sf <- function(df) {
#     if (nrow(df) == 0)
#       return(sf::st_sf(cell_id = character(0),
#                        geometry = sf::st_sfc(crs = 4326)))
#     polys <- purrr::pmap(df[, c("xi", "yi")], function(xi, yi) {
#       x0 <- xi * cell_size; x1 <- x0 + cell_size
#       y0 <- yi * cell_size; y1 <- y0 + cell_size
#       sf::st_polygon(list(matrix(
#         c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
#     })
#     sf::st_sf(cell_id  = df$cell_id,
#               geometry = sf::st_sfc(polys, crs = laea)) |>
#       sf::st_transform(4326)
#   }
#   
#   yr <- names(eau)[1]
#   list(
#     dat_all  = dplyr::mutate(make_polys_sf(all_cells), type = "all"),
#     dat_sel  = dplyr::mutate(make_polys_sf(sel_cells),  type = "assessment"),
#     table_df = data.frame(
#       year       = yr,
#       n          = nrow(all_cells),
#       n_exceed   = nrow(sel_cells),
#       assessment = dplyr::case_when(
#         nrow(sel_cells) < 2  ~ "CR",
#         nrow(sel_cells) < 20 ~ "EN",
#         nrow(sel_cells) < 50 ~ "VU",
#         nrow(sel_cells) < 55 ~ "NT",
#         TRUE                 ~ "LC"
#       ),
#       stringsAsFactors = FALSE
#     )
#   )
# }
# 
# criterion_b_module_ui <- function(id) {
#   ns <- NS(id)
#   nav_panel(
#     "Criterion B",
#     # Map for both criterion -------------------------------------------------
#     card(
#       full_screen = TRUE,
#       # card_header("Criterion B1 and B2"),
#       card_body(
#         leafletOutput(ns("map_B"), height = 300),
#         class = "p-0" # remove padding
#       )
#     ),
#     # buttons ----------------------------------------------------------------
#     layout_column_wrap(
#       width = "250px",
#       fill = FALSE,
#       actionButton(ns("get_mcp"), "Calculate the Extent of Occurrence (EOO) – Subcriterion B1", width = "100%"),
#       actionButton(ns("get_AOO"), "Calculate the Area of Occupancy (AOO) – Subcriterion B2", width = "100%")),
#     
#     # subcriterion value boxes -----------------------------------------------
#     layout_column_wrap(
#       width = "250px",
#       fill = FALSE,
#       uiOutput(ns("criterion_boxes_b1")),  # placeholder for the dynamic box, 
#       uiOutput(ns("criterion_boxes_b2"))  # placeholder for the dynamic box
#     )
#   )
# }
# 
# # -----------------------------------------------------------------------------
# # server
# # -----------------------------------------------------------------------------
# criterion_b_module_server <- function(id, rasters_original, year_inputs, rasters, areas,
#                                       upload_module, main_tab) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # modal to provide some user info -----------------------------------------
#     # 1) prevent the modal from popping up multiple times per session
#     modal_shown <- reactiveVal(FALSE)
#     
#     # 2) model pop up on tab opening 
#     observeEvent(main_tab(), {
#       # Only fire when Criterion B is selected
#       if (identical(main_tab(), "Criterion B")) {
#         # If you want it every time, remove the guard:
#         if (!modal_shown()) {
#           showModal(b_quick_modal)
#           modal_shown(TRUE)
#         }
#       }
#     }, ignoreInit = TRUE)
#     
#     
#     # 3) additional help button inside the modal footer
#     observeEvent(input$open_help, {
#       showModal(b_help_modal)
#     })
#     
#     
#     # reactive values to store data --------------------------------------------
#     shown_once   <- reactiveVal(FALSE)
#     mcp_vectors  <- reactiveVal(NULL)   # stores all MCP SpatVector (projected)
#     mcp_table    <- reactiveVal(NULL)   # df summary for B1
#     mcp_poly     <- reactiveVal(NULL)   # list of per-year polygons (if you need)
#     b2_table     <- reactiveVal(NULL)   # df summary for B2
#     b2_poly      <- reactiveVal(NULL)
#     aoo_sel_ids  <- reactiveVal(NULL)
#     aoo_all_ids  <- reactiveVal(NULL)
#     
#     # reset when new rasters uploaded ------------------------------------------
#     observeEvent(rasters(), {
#       
#       # reset UI and reactive holders -----------------------------------------
#       output$criterion_boxes_b1 <- renderUI({ NULL })
#       output$criterion_boxes_b2 <- renderUI({ NULL })
#       mcp_vectors(NULL)
#       mcp_poly(NULL)
#       mcp_table(NULL)
#       b2_table(NULL)
#       b2_poly(NULL)
#       aoo_sel_ids(NULL)
#       aoo_all_ids(NULL)
#       
#       # clear map -------------------------------------------------------------
#       leafletProxy(ns("map_B")) %>%
#         clearShapes() %>%
#         clearControls()
#     }, ignoreNULL = TRUE)
#     
#     # initialize map ----------------------------------------------------------
#     output$map_B <- renderLeaflet({
#       if (is.null(rasters()) || length(rasters()) < 1) {
#         leaflet() %>% addTiles()
#       } else {
#         # ext_r <- as.vector(ext(rasters()[[length(rasters())]])) # get eau extent for mapping zoom
#         yrs <- year_inputs()
#         latest_year <- max((yrs))
#         latest <- rasters()[[length(rasters())]]
#         ext_r  <- as.vector(ext(latest))   # get extent for zoom
#         
#         leaflet() %>%
#           addTiles(group = "OSM") %>%
#           addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
#           fitBounds(unname(ext_r["xmin"]),
#                     unname(ext_r["ymin"]),
#                     unname(ext_r["xmax"]),
#                     unname(ext_r["ymax"])) %>%
#           addRasterImage(latest, opacity = 0.7, colors = "#000000",
#                          group = paste0(latest_year, " classification")) %>%
#           addLayersControl(
#             baseGroups    = c("OSM", "ESRI Satellite"),
#             overlayGroups = c(paste0(latest_year, " classification")),
#             options       = layersControlOptions(collapsed = FALSE)
#           )
#       }
#     })
#     
#     # Subcriterion B1 (EOO) ---------------------------------------------------
#     observeEvent(input$get_mcp, {
#       req(rasters())
#       yrs <- year_inputs()
#       mcp_list <- list()
#       
#       print(yrs)
#       str(rasters_original())
#       str(rasters())
#       print(rasters())
#       print(rasters_original())
#       
#       progress <- shiny::Progress$new(min = 0, max = length(yrs))
#       on.exit({
#         progress$close()
#         removeModal()
#       }, add = TRUE)
#       
#       # show a simple modal (no module-scoped spinner) ----------------------
#       # showModal(modalDialog(
#       #   title = "Calculating MCP for EOO",
#       #   tagList(p("Please wait...")),
#       #   easyClose = FALSE,
#       #   footer = NULL
#       # ))
#       showModal(b1_modal)
#       
#       # for (i in seq_along(rasters_original())) {
#       progress$set(message = "Calculating MCP...", value = 1)
#       # r <- rasters_original()[[i]]
#       eau_poly <- makeEOO.SpatRaster(rasters_original(), max(yrs))
#       if (!is.null(eau_poly)) {
#         mcp_list[[1]] <- eau_poly
#       }
#       # }
#       
#       # store list and combined SpatVector -----------------------------------
#       mcp_poly(mcp_list)
#       if (length(mcp_list) > 0) {
#         mcp_all <- do.call(rbind, mcp_list)
#         mcp_vectors(mcp_all)       # <-- set the reactiveVal properly
#         
#         df <- as.data.frame(mcp_all)[, c("year", "area_km2")]
#         df$assessment <- dplyr::case_when(
#           df$area_km2 < 2000  ~ "CR",
#           df$area_km2 < 20000 ~ "EN",
#           df$area_km2 < 50000 ~ "VU",
#           df$area_km2 < 55000 ~ "NT", # hard coded currently, could be related to a user input easily.
#           TRUE                ~ "LC"
#         )
#         mcp_table(df)
#       }
#       
#       # update map layers only if mcp_vectors available ----------------------
#       if (!is.null(mcp_vectors())) {
#         latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
#         r_list <- rasters()
#         latest  <- r_list[[length(r_list)]]
#         
#         # Include B2 groups in the controls if AOO has already been calculated
#         b2_groups <- if (!is.null(b2_poly())) {
#           c("10x10 km grid", "AOO (>1% occupancy)")
#         } else {
#           character(0)
#         }
#         all_groups <- c(paste0(latest_year, " classification"),
#                         paste0(latest_year, " EOO (MCP)"),
#                         b2_groups)
#         
#         leafletProxy(ns("map_B")) %>%
#           clearGroup(paste0(latest_year, " EOO (MCP)")) %>%
#           addPolygons(
#             data = sf::st_as_sf(terra::project(
#               mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
#             fillOpacity = 0.3, color = "black", weight = 2,
#             group = paste0(latest_year, " EOO (MCP)"),
#             label = ~htmltools::HTML(
#               paste0("<strong>Year:</strong> ", year,
#                      "<br><strong>Area:</strong> ", round(area_km2, 2), " km²")
#             ),
#             highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
#           ) %>%
#           clearControls() %>%
#           addLayersControl(
#             baseGroups    = c("OSM", "ESRI Satellite"),
#             overlayGroups = all_groups,
#             options       = layersControlOptions(collapsed = FALSE)
#           )
#       }
#       
#       # show value box -------------------------------------------------------
#       if (!is.null(mcp_table())) {
#         b1_box <- make_b_value_boxes(
#           title = "Subcriterion B1 - Restricted EOO",
#           reactive_data = reactive(mcp_table()),
#           status_col = "assessment",
#           data_col = "area_km2",
#           hover_text = "EOO (km2)",
#           type = "b1"
#         )
#         
#         output$criterion_boxes_b1 <- renderUI({
#           layout_column_wrap(width = "250px", b1_box())
#         })
#       }
#     })
#     
#     # Subcriterion B2 (AOO) ---------------------------------------------------
#     observeEvent(input$get_AOO, {
#       progress <- shiny::Progress$new(min = 0, max = 3)
#       on.exit({
#         progress$close()
#         removeModal()
#       }, add = TRUE)
#       
#       # showModal(modalDialog(
#       #   title = "Calculating grids for AOO",
#       #   tagList(p("Please wait...")),
#       #   easyClose = FALSE,
#       #   footer = NULL
#       # ))
#       showModal(b2_modal)
#       
#       req(rasters_original(), length(rasters_original()) > 0)
#       progress$set(message = "projecting", value = 1)
#       
#       eau <- rasters_original()[[length(rasters_original())]]
#       
#       progress$set(message = "computing AOO grid", value = 2)
#       
#       result <- compute_aoo_fast(eau, n_sample = 500000)
#       
#       if (is.null(result)) {
#         showNotification("No ecosystem cells found in raster.", type = "error")
#         return()
#       }
#       
#       progress$set(message = "calculating 1% rule", value = 3)
#       
#       b2_table(result$table_df)
#       
#       # Prepare map layers
#       dat_all <- result$dat_all
#       dat_sel <- result$dat_sel
#       
#       dat_all$type <- "all"
#       dat_sel$type <- "assessment"
#       cells10km <- terra::vect(rbind(dat_all, dat_sel))
#       b2_poly(cells10km)
#       cells_sf <- sf::st_as_sf(b2_poly())
#       
#       all_ids <- data.frame(year = result$table_df$year,
#                             cell_ids = I(list(dat_all$cell_id)))
#       sel_ids <- data.frame(year = result$table_df$year,
#                             cell_ids = I(list(dat_sel$cell_id)))
#       aoo_all_ids(all_ids)
#       aoo_sel_ids(sel_ids)
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
#       
#       leafletProxy(ns("map_B")) %>%
#         clearGroup("10x10 km grid") %>%
#         clearGroup("AOO (>1% occupancy)") %>%
#         make_fig_b2_layers(
#           dat_all     = dplyr::filter(cells_sf, type == "all"),
#           dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
#           r_recent    = rasters()[[length(rasters())]],
#           latest_year = max(year_inputs(), na.rm = TRUE)
#         ) %>%
#         clearControls() %>%
#         addLayersControl(
#           baseGroups    = c("OSM", "ESRI Satellite"),
#           overlayGroups = all_groups,
#           options       = layersControlOptions(collapsed = FALSE)
#         ) %>%
#         addLegend(
#           layerId  = "b2_legend",
#           position = "bottomright",
#           colors   = c("black", "orange"),
#           labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
#           opacity  = 0.7,
#           title    = "AOO layers"
#         )
#       
#       
#       # if (!is.null(mcp_vectors())) {
#       #   latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
#       #   
#       #   leafletProxy(ns("map_B")) %>%
#       #     clearGroup("10x10 km grid") %>%
#       #     clearGroup("AOO (>1% occupancy)") %>%
#       #     clearControls() %>%
#       #   addPolygons(data = dat_all, group = "10x10 km grid",
#       #               fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
#       #   addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
#       #               fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
#       #   addLayersControl(
#       #     baseGroups = c("OSM", "ESRI Satellite"),
#       #     overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification"), '10x10 km grid', 'AOO (>1% occupancy)'),
#       #     options = layersControlOptions(collapsed = FALSE)
#       #   ) %>% 
#       #     
#       #     addLegend(
#       #       position = "bottomright",
#       #       colors = c("black", "orange"),
#       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
#       #       opacity = 0.7,
#       #       title = "Polygon Layers"
#       #     )
#       #   
#       # } else {
#       #   leafletProxy(ns("map_B")) %>%
#       #     clearGroup("10x10 km grid") %>%
#       #     clearGroup("AOO (>1% occupancy)") %>%
#       #     addPolygons(data = dat_all, group = "10x10 km grid",
#       #                 fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
#       #     addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
#       #                 fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
#       #     addLayersControl(
#       #       baseGroups = c("OSM", "ESRI Satellite"),
#       #       overlayGroups = c( '10x10 km grid', 'AOO (>1% occupancy)'),
#       #       options = layersControlOptions(collapsed = FALSE)
#       #     ) %>%
#       #     
#       #     addLegend(
#       #       position = "bottomright",
#       #       colors = c("black", "orange"),
#       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
#       #       opacity = 0.7,
#       #       title = "Polygon Layers"
#       #     )
#       #   
#       # }
#       
#       
#       # show value boxes ----------------------------------------------------
#       b2_box <- make_b_value_boxes(
#         title = "Subcriterion B2 - Restricted AOO",
#         reactive_data = reactive(b2_table()),
#         status_col = "assessment",
#         data_col_1 = "n",
#         data_col_2 = "n_exceed",
#         hover_text = "AOO (10x10 km cells)",
#         type = "b2"
#       )
#       output$criterion_boxes_b2 <- renderUI({
#         layout_column_wrap(width = "250px", b2_box())
#       })
#     })
#     
#     # return reactiveVals ----------------------------------------------------
#     return(list(
#       mcp_table   = mcp_table,
#       mcp_poly    = mcp_poly,
#       mcp_vectors = mcp_vectors,
#       aoo_sel_ids = aoo_sel_ids,
#       aoo_all_ids = aoo_all_ids,
#       b2_table    = b2_table,
#       b2_poly     = b2_poly
#     ))
#   })
# }
# # # modules/criterion_b.R
# # 
# # # -----------------------------------------------------------------------------
# # # ui
# # # -----------------------------------------------------------------------------
# # criterion_b_module_ui <- function(id) {
# #   ns <- NS(id)
# #     nav_panel(
# #     "Criterion B",
# #     # Map for both criterion -------------------------------------------------
# #     card(
# #       full_screen = TRUE,
# #      # card_header("Criterion B1 and B2"),
# #       card_body(
# #         leafletOutput(ns("map_B"), height = 300),
# #         class = "p-0" # remove padding
# #       )
# #     ),
# #     # buttons ----------------------------------------------------------------
# #     layout_column_wrap(
# #       width = "250px",
# #       fill = FALSE,
# #       actionButton(ns("get_mcp"), "Calculate the Extent of Occurrence (EOO) – Subcriterion B1", width = "100%"),
# #       actionButton(ns("get_AOO"), "Calculate the Area of Occupancy (AOO) – Subcriterion B2", width = "100%")),
# #       
# #     # subcriterion value boxes -----------------------------------------------
# #     layout_column_wrap(
# #       width = "250px",
# #       fill = FALSE,
# #       uiOutput(ns("criterion_boxes_b1")),  # placeholder for the dynamic box, 
# #       uiOutput(ns("criterion_boxes_b2"))  # placeholder for the dynamic box
# #     )
# #   )
# # }
# # 
# # # -----------------------------------------------------------------------------
# # # server
# # # -----------------------------------------------------------------------------
# # criterion_b_module_server <- function(id, rasters_original, year_inputs, rasters, areas,
# #                                       upload_module, main_tab) {
# #   moduleServer(id, function(input, output, session) {
# #     ns <- session$ns
# #     
# #     # modal to provide some user info -----------------------------------------
# #     # 1) prevent the modal from popping up multiple times per session
# #     modal_shown <- reactiveVal(FALSE)
# #                                
# #     # 2) model pop up on tab opening 
# #     observeEvent(main_tab(), {
# #       # Only fire when Criterion B is selected
# #       if (identical(main_tab(), "Criterion B")) {
# #         # If you want it every time, remove the guard:
# #         if (!modal_shown()) {
# #           showModal(b_quick_modal)
# #           modal_shown(TRUE)
# #         }
# #       }
# #     }, ignoreInit = TRUE)
# #     
# #     
# #     # 3) additional help button inside the modal footer
# #     observeEvent(input$open_help, {
# #       showModal(b_help_modal)
# #     })
# #     
# #     
# #     # reactive values to store data --------------------------------------------
# #     shown_once   <- reactiveVal(FALSE)
# #     mcp_vectors  <- reactiveVal(NULL)   # stores all MCP SpatVector (projected)
# #     mcp_table    <- reactiveVal(NULL)   # df summary for B1
# #     mcp_poly     <- reactiveVal(NULL)   # list of per-year polygons (if you need)
# #     b2_table     <- reactiveVal(NULL)   # df summary for B2
# #     b2_poly      <- reactiveVal(NULL)
# #     aoo_sel_ids  <- reactiveVal(NULL)
# #     aoo_all_ids  <- reactiveVal(NULL)
# #     
# #     # reset when new rasters uploaded ------------------------------------------
# #     observeEvent(rasters(), {
# #       
# #       # reset UI and reactive holders -----------------------------------------
# #       output$criterion_boxes_b1 <- renderUI({ NULL })
# #       output$criterion_boxes_b2 <- renderUI({ NULL })
# #       mcp_vectors(NULL)
# #       mcp_poly(NULL)
# #       mcp_table(NULL)
# #       b2_table(NULL)
# #       b2_poly(NULL)
# #       aoo_sel_ids(NULL)
# #       aoo_all_ids(NULL)
# #       
# #       # clear map -------------------------------------------------------------
# #       leafletProxy(ns("map_B")) %>%
# #         clearShapes() %>%
# #         clearControls()
# #     }, ignoreNULL = TRUE)
# #     
# #     # initialize map ----------------------------------------------------------
# #     output$map_B <- renderLeaflet({
# #       if (is.null(rasters()) || length(rasters()) < 1) {
# #         leaflet() %>% addTiles()
# #       } else {
# #         # ext_r <- as.vector(ext(rasters()[[length(rasters())]])) # get eau extent for mapping zoom
# #         yrs <- year_inputs()
# #         latest_year <- max((yrs))
# #         latest <- rasters()[[length(rasters())]]
# #         ext_r  <- as.vector(ext(latest))   # get extent for zoom
# #         
# #         leaflet() %>% 
# #           addTiles(group = "OSM") %>%
# #           addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
# #           fitBounds(unname(ext_r["xmin"]),
# #                     unname(ext_r["ymin"]),
# #                     unname(ext_r["xmax"]),
# #                     unname(ext_r["ymax"])) %>%
# #           addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
# #           addLayersControl(baseGroups = c("OSM", "ESRI Satellite"),
# #                            overlayGroups = c(paste0(latest_year, " classification")),
# #                            options = layersControlOptions(collapsed = FALSE))
# #       }
# #     })
# #     
# #     # Subcriterion B1 (EOO) ---------------------------------------------------
# #     observeEvent(input$get_mcp, {
# #       req(rasters())
# #       yrs <- year_inputs()
# #       mcp_list <- list()
# #       
# #       print(yrs)
# #       str(rasters_original())
# #       str(rasters())
# #       print(rasters())
# #       print(rasters_original())
# #       
# #       progress <- shiny::Progress$new(min = 0, max = length(yrs))
# #       on.exit({
# #         progress$close()
# #         removeModal()
# #       }, add = TRUE)
# #       
# #       # show a simple modal (no module-scoped spinner) ----------------------
# #       # showModal(modalDialog(
# #       #   title = "Calculating MCP for EOO",
# #       #   tagList(p("Please wait...")),
# #       #   easyClose = FALSE,
# #       #   footer = NULL
# #       # ))
# #       showModal(b1_modal)
# #       
# #      # for (i in seq_along(rasters_original())) {
# #         progress$set(message = "Calculating MCP...", value = 1)
# #        # r <- rasters_original()[[i]]
# #         eau_poly <- makeEOO.SpatRaster(rasters_original(), max(yrs))
# #         if (!is.null(eau_poly)) {
# #           mcp_list[[1]] <- eau_poly
# #         }
# #      # }
# #       
# #       # store list and combined SpatVector -----------------------------------
# #       mcp_poly(mcp_list)
# #       if (length(mcp_list) > 0) {
# #         mcp_all <- do.call(rbind, mcp_list)
# #         mcp_vectors(mcp_all)       # <-- set the reactiveVal properly
# #         
# #         df <- as.data.frame(mcp_all)[, c("year", "area_km2")]
# #         df$assessment <- dplyr::case_when(
# #           df$area_km2 < 2000  ~ "CR",
# #           df$area_km2 < 20000 ~ "EN",
# #           df$area_km2 < 50000 ~ "VU",
# #           df$area_km2 < 55000 ~ "NT", # hard coded currently, could be related to a user input easily.
# #           TRUE                ~ "LC"
# #         )
# #         mcp_table(df)
# #       }
# #       
# #       # update map layers only if mcp_vectors available ----------------------
# #       if (!is.null(mcp_vectors())) {
# #         latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
# #         r_list <- rasters()
# #         latest  <- r_list[[length(r_list)]]
# #         
# #         # Include B2 groups in the controls if AOO has already been calculated
# #         b2_groups <- if (!is.null(b2_poly())) {
# #           c("10x10 km grid", "AOO (>1% occupancy)")
# #         } else {
# #           character(0)
# #         }
# #         all_groups <- c(paste0(latest_year, " classification"),
# #                         paste0(latest_year, " EOO (MCP)"),
# #                         b2_groups)
# #         
# #         leafletProxy(ns("map_B")) %>%
# #           clearGroup(paste0(latest_year, " EOO (MCP)")) %>%
# #           # clearGroup(paste0(latest_year, " classification")) %>%
# #           addPolygons(
# #             data = sf::st_as_sf(terra::project(
# #               mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
# #             fillOpacity = 0.3, color = "black", weight = 2,
# #             group = paste0(latest_year, " EOO (MCP)"),
# #             label = ~htmltools::HTML(
# #               paste0("<strong>Year:</strong> ", year,
# #                      "<br><strong>Area:</strong> ", round(area_km2, 2), " km²")
# #             ),
# #             highlightOptions = highlightOptions(bringToFront = TRUE, color = "white", weight = 3)
# #           ) %>%
# #           addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
# #           addLayersControl(
# #             baseGroups = c("OSM", "ESRI Satellite"),
# #             overlayGroups = all_groups,
# #             options = layersControlOptions(collapsed = FALSE)
# #           )
# #       }
# #       
# #       # show value box -------------------------------------------------------
# #       if (!is.null(mcp_table())) {
# #         b1_box <- make_b_value_boxes(
# #           title = "Subcriterion B1 - Restricted EOO",
# #           reactive_data = reactive(mcp_table()),
# #           status_col = "assessment",
# #           data_col = "area_km2",
# #           hover_text = "EOO (km2)",
# #           type = "b1"
# #         )
# #         
# #         output$criterion_boxes_b1 <- renderUI({
# #           layout_column_wrap(width = "250px", b1_box())
# #         })
# #       }
# #     })
# #     
# #     # Subcriterion B2 (AOO) ---------------------------------------------------
# #     observeEvent(input$get_AOO, {
# #       progress <- shiny::Progress$new(min = 0, max = 3)
# #       on.exit({
# #         progress$close()
# #         removeModal()
# #       }, add = TRUE)
# #       
# #       # showModal(modalDialog(
# #       #   title = "Calculating grids for AOO",
# #       #   tagList(p("Please wait...")),
# #       #   easyClose = FALSE,
# #       #   footer = NULL
# #       # ))
# #       showModal(b2_modal)
# #       
# #       req(rasters_original(), length(rasters_original()) > 0)
# #       progress$set(message = "projecting", value = 1)
# #       
# #       eau <- rasters_original()[[length(rasters_original())]]
# #       # grid.prj <- readChar("data/World_Cylindrical_Equal_Area_WKT.txt", file.info("data/World_Cylindrical_Equal_Area_WKT.txt")$size)
# #       # eau.wcea <- project(eau, grid.prj)
# #       
# #       # Compute centre of data extent in WGS84
# #       eau_4326 <- project(eau, "EPSG:4326")
# #       ext_4326 <- ext(eau_4326)
# #       center_lon <- (ext_4326$xmin + ext_4326$xmax) / 2
# #       center_lat <- (ext_4326$ymin + ext_4326$ymax) / 2
# #       
# #       # Build a custom LAEA projection centred on this data
# #       grid.prj <- sprintf(
# #         "+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
# #         center_lat, center_lon
# #       )
# #       # grid.prj <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# #       
# #       
# #       eau.wcea <- project(eau, grid.prj)
# #       
# #       
# #       progress$set(message = "creating grid", value = 2)
# #       grid <- rast(ext(eau.wcea), res = c(10000, 10000), crs = crs(eau.wcea))
# #       grid.expanded <- extend(grid, c(2,2))
# #       grid.expanded[] <- 0
# #       grid.polygons <- as.polygons(grid.expanded, aggregate = FALSE)
# #       grid.polygons.sf <- st_as_sf(grid.polygons)
# #       grid.polygons.sf$cell_id <- 1:nrow(grid.polygons.sf)
# #       
# #       aoo_grid_area <- exact_extract(eau.wcea, grid.polygons.sf, 'sum', coverage_area=TRUE, force_df=TRUE)
# #       names(aoo_grid_area) <- names(eau.wcea)
# #       
# #       progress$set(message = "calculating 1% rule", value = 3)
# #       aoo_grid_counts <- cbind(cell_id = grid.polygons.sf$cell_id, aoo_grid_area) %>%
# #         pivot_longer(cols = -cell_id, names_to = "year", values_to = "area")
# #       
# #       aoo_grid_counts_cumulative <- aoo_grid_counts %>%
# #         group_by(year) %>%
# #         arrange(area, .by_group = TRUE) %>%
# #         mutate(cum_area = cumsum(area),
# #                cum_prop = cum_area / sum(area, na.rm = TRUE)) %>%
# #         ungroup()
# #       
# #       n_cells <- aoo_grid_counts_cumulative %>%
# #         group_by(year) %>%
# #         summarise(n = sum(area > 0, na.rm = TRUE),
# #                   n_exceed = sum(cum_prop > 0.01, na.rm = TRUE))
# #       
# #       df.eoo <- as.data.frame(n_cells)
# #       df.eoo$assessment <- dplyr::case_when(
# #         df.eoo$n_exceed < 2  ~ "CR",
# #         df.eoo$n_exceed < 20 ~ "EN",
# #         df.eoo$n_exceed < 50 ~ "VU",
# #         df.eoo$n_exceed < 55 ~ "NT", # hard coded currently, could be related to a user input easily.
# #         TRUE                 ~ "LC"
# #       )
# #       b2_table(df.eoo)
# #       
# #       # prepare polygons and map layers
# #       grid_sf <- grid.polygons.sf %>% st_transform(crs = 4326)
# # 
# #       
# #       all_ids <- aoo_grid_counts_cumulative %>%
# #         filter(cum_prop > 0) %>%
# #         group_by(year) %>%
# #         summarise(cell_ids = list(cell_id))
# #       aoo_all_ids(all_ids)
# #       
# #       sel_ids <- aoo_grid_counts_cumulative %>%
# #         filter(cum_prop > 0.01) %>%
# #         group_by(year) %>%
# #         summarise(cell_ids = list(cell_id))
# #       aoo_sel_ids(sel_ids)
# #       
# #       dat_all <- grid_sf[grid_sf$cell_id %in% all_ids$cell_ids[[1]], ]
# #       dat_sel <- grid_sf[grid_sf$cell_id %in% sel_ids$cell_ids[[1]], ]
# #       
# #       # combine and save the above
# #       dat_all$type  <- "all"
# #       dat_sel$type  <- "assessment"
# #       cells10km <- vect(rbind(dat_all, dat_sel))
# #       b2_poly(cells10km)
# #       
# #       cells_sf <- sf::st_as_sf(b2_poly())
# #       # leafletProxy(ns("map_B")) %>%
# #       #   clearGroup("classification") %>%
# #       #   clearGroup("10x10 km grid") %>%
# #       #   clearGroup("AOO (>1% occupancy)") %>%
# #       #   make_fig_b2_layers( dat_all     = dplyr::filter(cells_sf, type == "all"),
# #       #                       dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
# #       #                       r_recent    = rasters()[[length(rasters())]],
# #       #                      latest_year = max(years(), na.rm = TRUE))
# #       
# #       
# #       # Build the full overlay group list — include B1 EOO group if already computed
# #       # Classification is always on the map (added in renderLeaflet), list it first
# #       latest_year <- max(year_inputs(), na.rm = TRUE)
# #       b1_eoo_groups <- if (!is.null(mcp_vectors())) {
# #         paste0(max(mcp_vectors()$year, na.rm = TRUE), " EOO (MCP)")
# #       } else {
# #         character(0)
# #       }
# #       b2_groups <- c("10x10 km grid", "AOO (>1% occupancy)")
# #       all_groups <- c(paste0(latest_year, " classification"),
# #                       b1_eoo_groups,
# #                       b2_groups)
# #       
# #       leafletProxy(ns("map_B")) %>%
# #         clearGroup("10x10 km grid") %>%
# #         clearGroup("AOO (>1% occupancy)") %>%
# #         make_fig_b2_layers(
# #           dat_all     = dplyr::filter(cells_sf, type == "all"),
# #           dat_sel     = dplyr::filter(cells_sf, type == "assessment"),
# #           r_recent    = rasters()[[length(rasters())]],
# #           latest_year = max(year_inputs(), na.rm = TRUE)
# #         ) %>%
# #         clearControls() %>%
# #         addLayersControl(
# #           baseGroups    = c("OSM", "ESRI Satellite"),
# #           overlayGroups = all_groups,
# #           options       = layersControlOptions(collapsed = FALSE)
# #         ) %>%
# #         addLegend(
# #           layerId  = "b2_legend",
# #           position = "bottomright",
# #           colors   = c("black", "orange"),
# #           labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
# #           opacity  = 0.7,
# #           title    = "AOO layers"
# #         )
# #       
# #       
# #       # if (!is.null(mcp_vectors())) {
# #       #   latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
# #       #   
# #       #   leafletProxy(ns("map_B")) %>%
# #       #     clearGroup("10x10 km grid") %>%
# #       #     clearGroup("AOO (>1% occupancy)") %>%
# #       #     clearControls() %>%
# #       #   addPolygons(data = dat_all, group = "10x10 km grid",
# #       #               fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
# #       #   addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
# #       #               fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
# #       #   addLayersControl(
# #       #     baseGroups = c("OSM", "ESRI Satellite"),
# #       #     overlayGroups = c(paste0(latest_year, " MCP"), paste0(latest_year, " classification"), '10x10 km grid', 'AOO (>1% occupancy)'),
# #       #     options = layersControlOptions(collapsed = FALSE)
# #       #   ) %>% 
# #       #     
# #       #     addLegend(
# #       #       position = "bottomright",
# #       #       colors = c("black", "orange"),
# #       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
# #       #       opacity = 0.7,
# #       #       title = "Polygon Layers"
# #       #     )
# #       #   
# #       # } else {
# #       #   leafletProxy(ns("map_B")) %>%
# #       #     clearGroup("10x10 km grid") %>%
# #       #     clearGroup("AOO (>1% occupancy)") %>%
# #       #     addPolygons(data = dat_all, group = "10x10 km grid",
# #       #                 fillColor = "black", fillOpacity = 0.2, color = "black", weight = .5) %>%
# #       #     addPolygons(data = dat_sel, group = "AOO (>1% occupancy)",
# #       #                 fillColor = "orange", fillOpacity = 0.5, color = "black", weight = .5) %>%
# #       #     addLayersControl(
# #       #       baseGroups = c("OSM", "ESRI Satellite"),
# #       #       overlayGroups = c( '10x10 km grid', 'AOO (>1% occupancy)'),
# #       #       options = layersControlOptions(collapsed = FALSE)
# #       #     ) %>%
# #       #     
# #       #     addLegend(
# #       #       position = "bottomright",
# #       #       colors = c("black", "orange"),
# #       #       labels = c("10x10 km grid", "AOO (>1% occupancy)"),
# #       #       opacity = 0.7,
# #       #       title = "Polygon Layers"
# #       #     )
# #       #   
# #       # }
# #       
# # 
# #       # show value boxes ----------------------------------------------------
# #       b2_box <- make_b_value_boxes(
# #         title = "Subcriterion B2 - Restricted AOO",
# #         reactive_data = reactive(b2_table()),
# #         status_col = "assessment",
# #         data_col_1 = "n",
# #         data_col_2 = "n_exceed",
# #         hover_text = "AOO (10x10 km cells)",
# #         type = "b2"
# #       )
# #       output$criterion_boxes_b2 <- renderUI({
# #         layout_column_wrap(width = "250px", b2_box())
# #       })
# #     })
# #     
# #     # return reactiveVals ----------------------------------------------------
# #     return(list(
# #       mcp_table   = mcp_table,
# #       mcp_poly    = mcp_poly,
# #       mcp_vectors = mcp_vectors,
# #       aoo_sel_ids = aoo_sel_ids,
# #       aoo_all_ids = aoo_all_ids,
# #       b2_table    = b2_table,
# #       b2_poly     = b2_poly
# #     ))
# #   })
# # }
