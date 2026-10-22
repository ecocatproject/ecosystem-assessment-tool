# helpers/elevation.R
# =============================================================================
# Elevation within the ecosystem
# =============================================================================
# Main source: Copernicus DEM GLO-90 (90 m), read as cloud-optimised GeoTIFFs
# straight from the AWS Open Data bucket (no auth, no bulk download). GDAL only
# fetches the overview level needed for the output grid, so this is quick even
# for large extents.
# Fallback: elevatr (AWS terrain tiles) with a zoom chosen from the extent size.
#
# Both work on a sampled ~1e6-cell copy of the ecosystem raster (same idea as
# compute_countries), rather than resampling the full-resolution raster, which
# was the other slow step.
#
# Returns a SpatRaster of elevation (m) masked to the ecosystem, as before.
# =============================================================================

# ---- Copernicus DEM tile URLs for an extent (EPSG:4326) ---------------------
cop90_urls <- function(xmin, xmax, ymin, ymax) {
  lats <- floor(ymin):(ceiling(ymax) - 1)
  lons <- floor(xmin):(ceiling(xmax) - 1)
  g <- expand.grid(lat = lats, lon = lons)
  lat_s <- sprintf("%s%02d_00", ifelse(g$lat < 0, "S", "N"), abs(g$lat))
  lon_s <- sprintf("%s%03d_00", ifelse(g$lon < 0, "W", "E"), abs(g$lon))
  id <- paste0("Copernicus_DSM_COG_30_", lat_s, "_", lon_s, "_DEM")
  paste0("/vsicurl/https://copernicus-dem-90m.s3.amazonaws.com/", id, "/", id, ".tif")
}

# ---- Fetch Copernicus DEM onto a target grid --------------------------------
get_dem_cop90 <- function(target, max_tiles = 150) {
  e <- terra::project(terra::ext(target), from = terra::crs(target), to = "EPSG:4326")
  urls <- cop90_urls(e$xmin, e$xmax, e$ymin, e$ymax)
  if (length(urls) > max_tiles) stop(length(urls), " DEM tiles needed; extent too large")
  
  # Faster, quieter remote reads
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif")
  terra::setGDALconfig("GDAL_HTTP_MULTIRANGE", "YES")
  terra::setGDALconfig("GDAL_HTTP_TIMEOUT", "30")
  
  # Ocean-only tiles don't exist; keep the ones that open (header read only)
  ok <- vapply(urls, function(u) !is.null(tryCatch(terra::rast(u), error = function(e) NULL)),
               logical(1))
  if (!any(ok)) stop("No Copernicus DEM tiles found for this extent")
  
  dem <- terra::vrt(urls[ok])
  # Warp to the target grid; GDAL uses COG overviews when downsampling
  terra::project(dem, target, method = "bilinear")
}

# ---- Fallback: elevatr with zoom chosen from extent -------------------------
get_dem_elevatr <- function(target) {
  e <- terra::project(terra::ext(target), from = terra::crs(target), to = "EPSG:4326")
  span <- max(e$xmax - e$xmin, e$ymax - e$ymin)          # degrees
  z <- if (span > 20) 5 else if (span > 8) 6 else if (span > 3) 7 else if (span > 1) 8 else 9
  ext_sf <- sf::st_as_sf(terra::as.polygons(terra::ext(target), crs = terra::crs(target)))
  r <- terra::rast(elevatr::get_elev_raster(locations = ext_sf, z = z, verbose = FALSE))
  terra::project(r, target, method = "bilinear")
}

# ---- Main function -----------------------------------------------------------
compute_elevation <- function(eau, cache_path = NULL, n_sample = 1e6) {
  if (terra::nlyr(eau) > 1) eau <- eau[[terra::nlyr(eau)]]
  
  # Coarse copy of the ecosystem: the grid elevation is computed on
  eco <- terra::spatSample(eau, n_sample, method = "regular", as.raster = TRUE)
  
  # ---- Cached full-extent DEM: just resample onto this (subset) grid --------
  if (!is.null(cache_path) && file.exists(cache_path)) {
    elev <- terra::resample(terra::rast(cache_path), eco, method = "bilinear")
    return(terra::trim(terra::mask(elev, eco)))
  }
  
  # ---- Fetch: Copernicus first, elevatr if that fails -----------------------
  elev <- tryCatch(get_dem_cop90(eco), error = function(e) {
    message("[elevation] Copernicus DEM failed: ", conditionMessage(e), " - trying elevatr")
    NULL
  })
  if (is.null(elev)) {
    elev <- tryCatch(get_dem_elevatr(eco), error = function(e) {
      message("[elevation] elevatr failed: ", conditionMessage(e))
      NULL
    })
  }
  if (is.null(elev)) return(NULL)   # observer shows the blank plot
  
  # Cache the unmasked full-extent DEM for refine/subset runs
  if (!is.null(cache_path)) terra::writeRaster(elev, cache_path, overwrite = TRUE)
  
  terra::mask(elev, eco)
}

# # test function
# eau <- terra::rast("path/to/ecosystem.tif")
# system.time(el <- compute_elevation(eau))
# terra::global(el, quantile, probs = c(0.05, 0.95), na.rm = TRUE)