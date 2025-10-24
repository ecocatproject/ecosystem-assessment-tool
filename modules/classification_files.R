# classification_files.R

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
# raster_module_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fileInput(ns("tifs"), "Upload GeoTIFFs", multiple = TRUE),
#     uiOutput(ns("year_inputs")),
#     actionButton(ns("process"), "Process Rasters")
#   )
# }

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
raster_module_server <- function(id, data_module, tifs_input, year_inputs, process_btn, leaflet_proxy) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      req(tifs_input())                    # see app.R server for tifs_inputs
      files <- tifs_input()$datapath    
      years <- year_inputs()               # see app.R server for year_inputs
      req(length(years) == length(files))  # sanity check
      
      # Call the data module function (see data.R) to process rasters
      data_module$process_rasters(files, years)
    })
    
    # Render map once processed rasters are ready
    observe({
      r_list <- data_module$rasters()
      req(r_list)
      latest <- r_list[[length(r_list)]]
      
      leafletProxy("description-roi_map") %>%
      # leafletProxy() %>%
        addRasterImage(latest, opacity = 0.7, colors = "#000000", group = "Latest EAU") %>%
        addLayersControl(
          baseGroups = c("OSM", "ESRI Satellite"),
          overlayGroups = c("Latest EAU", "ROI", "Rainfall (10y avg.)", "Elevation"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(lng = crds(centroids(vect(ext(latest))))[1],
                lat = crds(centroids(vect(ext(latest))))[2],
                zoom = 7)
    })
  })
}