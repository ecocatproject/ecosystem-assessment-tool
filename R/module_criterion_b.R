# Criterion B

# Module UI
mapViewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

# Module server
mapViewerServer <- function(id, raster_data) {
  moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addRasterImage(raster_data(), colors = "viridis")
    })
  })
}