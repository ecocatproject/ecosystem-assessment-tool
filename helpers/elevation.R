# helpers/elevation.R

# compute elevation function

# library(elevatr)
# library(sf)
# library(terra)
# eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# roi <- vect("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiSumbuThicketEcoregion20kmBuffer.shp")

# -------------------------------------------------------------------------
# Compute elevation
# compute_elevation <- function(roi) {
#   elev_roi <- rast(get_elev_raster(locations = st_as_sf(roi), z = 6)) |> crop(roi) |> mask(roi)
#   return(elev_roi)
# }


compute_elevation <- function(eau) {
  eau.ext <- as.polygons(ext(eau))
  crs(eau.ext) <- crs(eau)
  eau.ext.sf <- st_as_sf(eau.ext)
  elev_roi <- rast(get_elev_raster(locations = eau.ext.sf, z = 6)) #|> crop(roi) |> mask(roi)
  eau2 <- resample(eau, elev_roi, method = "average") 
  elev_map <- mask(elev_roi, eau2)
  return(elev_map)
}
# 
# plet(masked)
# hist(masked[!is.na(values(masked))][,1])
# 
# library(plotly)
# hist(masked[!is.na(values(masked))][,1])