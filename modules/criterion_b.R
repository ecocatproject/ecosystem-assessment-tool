# modules/criterion_b.R

# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------
# criterion_b_card_ui function in the helpers.R script
# criterion_B_module_ui <- function(id) {
#   ns <- NS(id)
#   nav_panel(
#     "Criterion B",
#     layout_column_wrap(
#       width = 1/2,
#       heights_equal = "row",
#       criterion_b_card_ui(ns("b1"), "Subcriteria B1", "Calculate EOO - Subcriteria B1"),
#       criterion_b_card_ui(ns("b2"), "Subcriteria B2", "Calculate AOO - Subcriteria B2")
#     )
#   )
# }

criterion_b_module_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Criterion B",
    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      
      # Card for Criterion B1 -------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("Subcriteria B1"),
        card_body(
          leafletOutput(ns("map_B1"), height = 300),
          class = "p-0",# remove padding
          hr(),
          actionButton(ns("get_mcp"), "Click here to Calculate EOO - Subcriteria B1", width = "100%"),
          layout_column_wrap(
            height = 100,
            page_fillable(
              uiOutput(ns("criterion_boxes_b1"))  # placeholder for the dynamic box
            )
          )
        )
      ),
      # Card for Criterion B2 -------------------------------------------------
      card(
        full_screen = TRUE,
        card_header("Subcriteria B2"),
        card_body(
          leafletOutput(ns("map_B2"), height = 300),
          class = "p-0", # remove padding
          hr(),
          actionButton(ns("get_AOO"), "Click here to Calculate AOO - Subcriteria B2", width = "100%"),
          layout_column_wrap(
            height = 100,
            page_fillable(
              uiOutput(ns("criterion_boxes_b2"))  # placeholder for the dynamic box
            )
          )
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------
criterion_b_module_server <- function(id, rasters_original, year_inputs, rasters, areas, data_module, years) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------
    # Initialize map
    output$map_B1 <- renderLeaflet({
      if (is.null(rasters()) || length(rasters()) < 1){
        # return a blank map
        leaflet() %>% addTiles()
      } else {
        # safe to access rasters()[[1]]
        r <- rasters()[[1]]
        ctr <- crds(centroids(vect(ext(r))))
        leaflet() %>% addTiles() %>% setView(lng = ctr[1], lat = ctr[2], zoom = 6)
      }
    })
    output$map_B2 <- renderLeaflet({
      if (is.null(rasters()) || length(rasters()) < 1) {
        # return a blank map
        leaflet() %>% addTiles()
      } else {
        # safe to access rasters()[[1]]
        r <- rasters()[[1]]
        ctr <- crds(centroids(vect(ext(r))))
        leaflet() %>% addTiles() %>% setView(lng = ctr[1], lat = ctr[2], zoom = 6)
      }
    })
    
    # Subcriterion B1 ---------------------------------------------------------
    # reactive vars to store data
    mcp_vectors <- reactiveVal(NULL)   # stores all MCPs
    mcp_table   <- reactiveVal(NULL)   # stores summary df
    mcp_poly <- reactiveVal(NULL)
    
    
    observeEvent(input$get_mcp, {
      req(rasters())
      years <- year_inputs()
      mcp_list <- list() # empty list to store each mcp
      
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = length(years))
      progress$set(message = "Calculating MCP...", value = 0)
      
      # calc eoo for each eau tif file
      for (i in seq_along(rasters_original())) {
        # Update progress
        progress$inc(i/nrow(years), detail = paste0("Processing ", years[i], " mcp"))   
        
        r <- rasters_original()[[i]]  # get the pre-loaded raster - NOW THE CORRECT DATA!
        eau_poly <- makeEOO.SpatRaster(r, years[i])
        if (!is.null(eau_poly)) {
          mcp_list[[i]] <- eau_poly
        }
      }
      
      # close progress
      progress$close()
      
      mcp_poly(mcp_list) # assign to reactive val
      
      # Merge into single SpatVector
      #mcp_vectors(mcp_list[order(years)])
      if (length(mcp_list) > 0) {
        if (length(mcp_list) > 0) {
          mcp_all <- do.call(rbind, mcp_list)
          mcp_data <- mcp_vectors(mcp_all)   # SpatVector # NOT REQUIRED  - MCP_POLY REACTIVE HAS THE DATA ALREADY.....
        } else {
          mcp_data <- terra::vect()  # empty SpatVector
        }
        
        df <- as.data.frame(mcp_all)[, c("year","area_km2")]
        
        df$assessment <- dplyr::case_when(
          df$area_km2 < 2000 ~ "CR",
          df$area_km2 < 20000 ~ "EN",
          df$area_km2 < 50000 ~ "VU",
          TRUE ~ "LC"
        )
        
        mcp_table(df) # store the table too
        
      }
      
      # -------------------------------------------------------
      # Clear old map content and re-add
      leafletProxy(ns("map_B1")) %>%
        clearGroup("mcp")
      
      if (!is.null(mcp_vectors())) {
        
        # Find the most recent year
        latest_year <- max(mcp_vectors()$year, na.rm = TRUE)
        r_list <- rasters()
        latest <- r_list[[length(r_list)]]
        
        leafletProxy(ns("map_B1")) %>%
          clearShapes() %>%   # Optional: remove old polygons
          addPolygons(
            data = sf::st_as_sf(terra::project(mcp_vectors()[mcp_vectors()$year == latest_year, ], "EPSG:4326")),
            fillOpacity = 0.3,
            color = "black",
            weight = 2,
            group = paste0(latest_year, " MCP"),
            label = ~paste0("Year: ", year, 
                            "<br>Area: ", round(area_km2, 2), " km²"),
            highlightOptions = highlightOptions(
              bringToFront = TRUE, color = "white", weight = 3
            )
          ) %>%
          addRasterImage(latest, opacity = 0.7, colors = "#000000", group = paste0(latest_year, " classification")) %>%
          addLayersControl(
            overlayGroups = c(paste0(latest_year, " MCP"),paste0(latest_year, " classification")) ,
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
      # create value_box - 1 data column input
      b1_box <- make_b_value_boxes(
        title = "Subcriterion B1",
        reactive_data = reactive(df),
        status_col = "assessment",
        data_col = "area_km2",
        hover_text = "Restricted geographic distribution as measured by a minimum convex polygon (km2) enclosing all occurrences (extent of  occurrence, EOO)",
        type = "single"
      )
      
      output$criterion_boxes_b1 <- renderUI({
        layout_column_wrap(
          width = "250px",
          b1_box()
        )
      })
    
    }) # end of observe event (button click here)
    
    # Subcriterion B2 ---------------------------------------------------------
    # reactive vars to store data
    b2_table   <- reactiveVal(NULL)   # stores summary df
    b2_poly <- reactiveVal(NULL)
    aoo_sel_ids <- reactiveVal(NULL)
    aoo_all_ids <- reactiveVal(NULL)
    
    observeEvent(input$get_AOO, {
      
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = 3)
      progress$set(message = "Calculating AOO...", value = 0)
      
      req(rasters_original(), length(rasters_original()) > 0) # checks the list of rasters is not empty
      
      # get the stored tifs and name by year
      eau <- rasters_original()[[length(rasters_original())]] # most recent eau
      
      # Update progress
      progress$inc(1, detail = "projecting to EAG")   
      
      # always need to project the raster to an equal area grid - PERHAPS THE SLOWEST PART?
      grid.prj <- readChar("data/World_Cylindrical_Equal_Area_WKT.txt", 
                           file.info("data/World_Cylindrical_Equal_Area_WKT.txt")$size)
      eau.wcea <- project(eau, grid.prj)

      # Update progress
      progress$inc(1, detail = "grid+ create")  
      
      # createGrid <-
      grid <- rast(ext(eau.wcea), res = c(10000, 10000), crs = crs(eau.wcea))
      grid.expanded <- extend(grid, c(2,2)) # grow the grid by 2 each way to be safe
      grid.expanded[] <- 0
      
      # converted to polygons (seems to be faster/easier processing just now)
      grid.polygons <- as.polygons(grid.expanded, aggregate = FALSE)# raster to polygons
      grid.polygons.sf <- st_as_sf(grid.polygons)                   # convert to sf
      grid.polygons.sf$cell_id <- 1:nrow(grid.polygons.sf)          # assign cell id
      
      # use exactextract for intersection/overlay
      aoo_grid_area <- exact_extract(eau.wcea, grid.polygons.sf, 'sum', coverage_area=TRUE, force_df=T)
      names(aoo_grid_area) <- names(eau.wcea) # rename columns
      
      
      # Update progress
      progress$inc(1, detail = "calc 1% rule")  
      
      # the 1% (NEW) rule
      # Add IDs and pivot longer
      aoo_grid_counts <- cbind(cell_id = grid.polygons.sf$cell_id, aoo_grid_area) %>%
        pivot_longer(cols = -cell_id, names_to = "year", values_to = "area")
      
      # process and calculate cumulative proportions
      aoo_grid_counts_cumulative <- aoo_grid_counts %>%
        group_by(year) %>%
        arrange(area, .by_group = TRUE) %>%
        mutate(cum_area = cumsum(area),
               cum_prop = cum_area / sum(area, na.rm = TRUE)) %>%
        ungroup()
      
      # Count cells exceeding 1% cumulative proportion, per year
      n_cells <- aoo_grid_counts_cumulative %>%
        group_by(year) %>%
        summarise(
          n = sum(area >0, na.rm=T), 
          n_exceed = sum(cum_prop > 0.01, na.rm = TRUE))
      
      #ADD N_CELLS TO A REACTIVE.................................................?
      
      df.eoo <- as.data.frame(n_cells)
      
      df.eoo$assessment <- dplyr::case_when(
        df.eoo$n_exceed < 2 ~ "CR",
        df.eoo$n_exceed < 20 ~ "EN",
        df.eoo$n_exceed < 50 ~ "VU",
        TRUE ~ "LC")
      
      b2_table(df.eoo) # store the table too
      
      # -------------------------------------------------------
      # visualise the grids
      grid_sf <- grid.polygons.sf %>% st_transform(crs = 4326) # reproject the grid
      b2_poly(grid_sf)
      
      # make a table of cells with eau
      all_ids <- aoo_grid_counts_cumulative %>%
        filter(cum_prop > 0) %>%
        group_by(year) %>%
        summarise(cell_ids = list(cell_id))
      aoo_all_ids(all_ids)
      
      # make a table of "which cell_ids exceed 0.01" per year
      sel_ids <- aoo_grid_counts_cumulative %>%
        filter(cum_prop > 0.01) %>%
        group_by(year) %>%
        summarise(cell_ids = list(cell_id))
      aoo_sel_ids(sel_ids)
      
      # data selection of the most recent data
      dat_all <- grid_sf[grid_sf$cell_id %in% all_ids$cell_ids[[1]], ]
      dat_sel <- grid_sf[grid_sf$cell_id %in% sel_ids$cell_ids[[1]], ]
      
      # map the most recent data
      leafletProxy(ns("map_B2"))  %>%
        addPolygons(data = dat_all, group = "10x10 km grid",
                    fillColor = "black",
                    fillOpacity = 0.2,
                    color = "black",
                    weight = .5) %>%
        addPolygons(data = dat_sel, group ="AOO (>1% occupancy)",
                    fillColor = "orange",
                    fillOpacity = 0.5,
                    color = "black",
                    weight = .5) %>%
        addLayersControl(
          overlayGroups = c(#'eau',
            '10x10 km grid', 'AOO (>1% occupancy)'),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      # close progress
      progress$close()
      
      # ----------------------------------------------
      # value boxes instead of table
      b2_box <- make_b_value_boxes(
        title = "Subcriterion B2",
        reactive_data = reactive(df.eoo),
        status_col = "assessment",
        data_col_1 = "n",
        data_col_2 = "n_exceed",
        hover_text = "Restricted geographic distribution as measured by the number of 10 × 10 km grid cells with >1% occupancy (area of occupancy, AOO)",
        type = "double"
      )
      
      output$criterion_boxes_b2 <- renderUI({
        layout_column_wrap(
          width = "250px",
          b2_box()
        )
      })

      
    }) # end of observeEvent(get_AOO)
    
    # Return reactive values for use outside the module
    return(list(
      mcp_data = reactive({mcp_table}),
      mcp_poly = reactive({mcp_poly}),
      aoo_sel_ids = reactive({aoo_sel_ids}),
      aoo_all_ids = reactive({aoo_all_ids}),
      b2_table = reactive({b2_table}),
      b2_poly = reactive({b2_poly})
    ))
  })
}
