# helpers/elevation.R

# compute elevation function with optional caching.
# On first call (cache_path missing or NULL): downloads elevation via elevatr,
# saves the full-extent raster to cache_path, returns the masked elevation.
# On later calls (cache_path exists): loads the cached full-extent raster and
# masks it to the current ecosystem (e.g. a spatial subset) — no re-download.
compute_elevation <- function(eau, cache_path = NULL) {
  
  if (!is.null(cache_path) && file.exists(cache_path)) {
    # ---- Use cached full-extent elevation, mask to current eau --------------
    elev_roi <- terra::rast(cache_path)
    eau2     <- terra::resample(eau, elev_roi, method = "average")
    elev_map <- terra::mask(elev_roi, eau2)
    elev_map <- terra::trim(elev_map)   # shrink extent to the subset area
    return(elev_map)
  }
  
  # ---- No cache: download full extent -------------------------------------
  eau.ext <- terra::as.polygons(terra::ext(eau))
  terra::crs(eau.ext) <- terra::crs(eau)
  eau.ext.sf <- sf::st_as_sf(eau.ext)
  elev_roi   <- terra::rast(elevatr::get_elev_raster(locations = eau.ext.sf, z = 10))
  
  # Save the full-extent (unmasked) elevation to cache for reuse on subset
  if (!is.null(cache_path)) {
    terra::writeRaster(elev_roi, cache_path, overwrite = TRUE)
  }
  
  eau2     <- terra::resample(eau, elev_roi, method = "average")
  elev_map <- terra::mask(elev_roi, eau2)
  return(elev_map)
}




# # helpers/elevation.R
# 
# # compute elevation function
# 
# # library(elevatr)
# # library(sf)
# # library(terra)
# # eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# # roi <- vect("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiSumbuThicketEcoregion20kmBuffer.shp")
# 
# # -------------------------------------------------------------------------
# # Compute elevation
# # compute_elevation <- function(roi) {
# #   elev_roi <- rast(get_elev_raster(locations = st_as_sf(roi), z = 6)) |> crop(roi) |> mask(roi)
# #   return(elev_roi)
# # }
# 
# 
# compute_elevation <- function(eau) {
#   eau.ext <- as.polygons(ext(eau))
#   crs(eau.ext) <- crs(eau)
#   eau.ext.sf <- st_as_sf(eau.ext)
#   elev_roi <- rast(get_elev_raster(locations = eau.ext.sf, z = 10)) #|> crop(roi) |> mask(roi)
#   eau2 <- resample(eau, elev_roi, method = "average") 
#   elev_map <- mask(elev_roi, eau2)
#   return(elev_map)
# }
# # 
# # plet(masked)
# # hist(masked[!is.na(values(masked))][,1])
# # 
# # library(plotly)
# # hist(masked[!is.na(values(masked))][,1])