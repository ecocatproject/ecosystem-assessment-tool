# classification_processing.R

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
raster_module_server <- function(id, tifs_input, year_inputs, process_btn, map_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store raster data
    rasters <- reactiveVal()
    rasters_original <- reactiveVal()
    areas <- reactiveVal()
    years <- reactiveVal()
    
    #--------------------------------------------------------------
    # Process rasters when button is clicked
    #--------------------------------------------------------------
    observeEvent(process_btn(), {
      req(tifs_input())
      files <- tifs_input()$datapath
      year_vec <- year_inputs()
      req(length(files) == length(year_vec))
      
      showModal(modalDialog(
        title = "Processing classification maps",
        tagList(
          div(style = "text-align:center;",
              tags$h4("Please wait..."),
              tags$p("This may take up to 5 seconds per map."),
              tags$p("Results will appear when done."),
              withSpinner(uiOutput(ns("dummy_spinner")), type = 5)
          )
        ),
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Actual processing (moved from data_module)
      progress <- shiny::Progress$new(min = 0, max = length(files))
      progress$set(message = "Processing classification maps...", value = 0)
      
      ras_list <- list()
      ras_list_original <- list()
      area_vals <- numeric(length(files))
      order_idx <- order(as.numeric(year_vec))
      files <- files[order_idx]
      year_vec <- year_vec[order_idx]
      
      areas(NULL)
      
      for (i in seq_along(files)) {
        progress$inc(1, detail = paste0("Processing ", year_vec[i], " classification"))
        r_orig <- rast(files[i])
        if (global(r_orig, "isNA") == 0) {
          r_orig <- classify(r_orig, matrix(c(0, NA), ncol = 2))
        }
        
        mask <- r_orig == 1
        if (!is.na(crs(r_orig)) && !is.lonlat(r_orig)) {
          cell_area <- prod(res(r_orig))
          area_vals[i] <- sum(values(mask, na.rm = TRUE)) * cell_area
        } else {
          area_vals[i] <- sum(expanse(mask, unit = "m"))
        }
        
        area_df <- data.frame(
          Year = year_vec[i],
          Area_ha = area_vals[i] / 10000,
          Area_km2 = area_vals[i] / 1e6
        )
        
        if (is.null(areas())) areas(area_df) else areas(rbind(areas(), area_df))
        
        r_map <- tryCatch({
          project(r_orig, "EPSG:4326")
        }, error = function(e) { crs(r_orig) <- "EPSG:4326"; r_orig })
        
        names(r_orig) <- names(r_map) <- year_vec[i]
        ras_list_original[[i]] <- r_orig
        r_map <- spatSample(r_map, 100000, method = "regular", as.raster = TRUE)
        ras_list[[i]] <- r_map
      }
      
      progress$close()
      removeModal()
      
      rasters(ras_list)
      rasters_original(ras_list_original)
      years(year_vec)
    })
    
    #--------------------------------------------------------------
    # Render leaflet map when rasters change
    #--------------------------------------------------------------
    observe({
      r_list <- rasters()
      req(r_list)
      
      latest <- r_list[[length(r_list)]]
      latest_year <- names(latest)
      if (is.null(latest_year) || latest_year == "") latest_year <- "Latest"
      display_label <- paste0(latest_year, " classification")
      
      leafletProxy(map_id()) %>%
        clearGroup(display_label) %>%
        addRasterImage(latest, opacity = 0.7, colors = "#000000",
                       group = display_label,
                       options = leaflet::pathOptions(pane = "rasterPane")) %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          overlayGroups = c(display_label, "ROI", "Elevation"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(lng = crds(centroids(vect(ext(latest))))[1],
                lat = crds(centroids(vect(ext(latest))))[2],
                zoom = 7)
    })
    
    #--------------------------------------------------------------
    # Return data reactives
    #--------------------------------------------------------------
    return(list(
      rasters = rasters,
      rasters_original = rasters_original,
      areas = areas,
      years = years
    ))
  })
}
