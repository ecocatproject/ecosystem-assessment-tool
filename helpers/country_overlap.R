# # country_overlap.R
# # 
# # library(rnaturalearth) # ne_countries
# # library(rnaturalearthdata)
# # library(terra)
# # library(exactextractr)
# # library(sf)
# # # load data
# # countries <- vect(ne_countries(scale = "medium", returnclass = "sf"))
# 
# # country_overlap.R
# 
# # compute temperature function
# # will work for both spatvector and spatraster
# 
# compute_countries <- function(x, countries) {
#   if (is.null(countries)) stop("Countries data not available")
#   if (is.null(x)) stop("ROI not loaded")
#   # -------------------------------------------------------------------------
#   # spatvector 
#   if (inherits(x, "SpatVector")) {
#     
#     countries_proj <- terra::project(countries, "EPSG:4326")
#     roi_proj       <- terra::project(x,         "EPSG:4326")
#     
#     roi_int <- tryCatch({
#       terra::intersect(countries_proj, roi_proj)
#     }, error = function(e) {
#       message("Error during intersect: ", e$message)
#       NULL
#     })
#     
#     if (is.null(roi_int) || nrow(roi_int) == 0) return(data.frame())
#     
#     roi_int$area_km2 <- terra::expanse(roi_int, unit = "km")
#     total_area <- terra::expanse(roi_proj, unit = "km")
#     
#     result <- as.data.frame(roi_int) %>%
#       dplyr::group_by(admin) %>%
#       dplyr::summarise(area_km2 = sum(area_km2), .groups = "drop") %>%
#       dplyr::mutate(
#         perc_roi = 100 * area_km2 / total_area,
#         area_km2 = format(round(area_km2, 0), big.mark = ","),
#         perc_roi = format(round(perc_roi, 1))
#       ) %>%
#       dplyr::arrange(desc(perc_roi)) %>%
#       dplyr::rename(
#         Country = admin,
#         `Area (km2)` = area_km2,
#         `% of Ecosystem` = perc_roi
#       )
#     
#     return(result)
#   }
#   
#   # -------------------------------------------------------------------------
#   # spatraster
#   if (inherits(x, "SpatRaster")) {
#     
#     # # 0. Match CRS first — every spatial op below needs them aligned
#     # if (!terra::same.crs(countries, x)) {
#     #   countries <- terra::project(countries, terra::crs(x))
#     # }
#     
#     # # 1. Reduce country polygons to raster extent
#     # countries_eau <- terra::mask(countries, terra::ext(x))
#     
#     # Project the raster's extent to the countries CRS (cheap — just 4 corners)
#     # then crop countries before projecting (avoids domain errors for far countries)
#     x_extent_in_countries_crs <- terra::project(
#       terra::as.polygons(terra::ext(x), crs = terra::crs(x)),
#       terra::crs(countries)
#     )
#     countries_cropped <- terra::crop(countries, x_extent_in_countries_crs)
#     
#     # Now project just the cropped countries to match raster CRS
#     if (!terra::same.crs(countries_cropped, x)) {
#       countries_eau <- terra::project(countries_cropped, terra::crs(x))
#     } else {
#       countries_eau <- countries_cropped
#     }
#     
#     # 2. Aggregate raster (speed)
#     # eau_agg <- terra::aggregate(x, 2, fun = "min", na.rm = TRUE)
#     eau_agg <- spatSample(x, 1000000, method = "regular",
#                           as.raster = TRUE, na.rm = TRUE)
#     
#     if (is.null(eau_agg) || ncell(eau_agg) == 0) return(data.frame())
#     
#     # 3. Rasterize country polygons to match aggregated raster
#     eau_r <- terra::rasterize(countries_eau, eau_agg, field = "admin")
#     
#     # 4. Zonal count of non-NA raster pixels
#     # z_result <- terra::zonal(eau_agg, eau_r, fun = "notNA", na.rm = TRUE)
#     
#     # 5. Compute area per country zone
#     area_per_country <- terra::expanse(eau_agg, zones = eau_r, unit = "km")
#     
#     country_totals <- data.frame(
#       zone             = countries_eau$admin,
#       country_area_km2 = terra::expanse(countries_eau, unit = "km")
#     )
#     
#     total_area <- sum(area_per_country$area)
#     
#     
#     result <- area_per_country %>%
#       dplyr::left_join(country_totals, by = "zone") |>
#       dplyr::mutate(pct_cover = round(area / country_area_km2 * 100, 2)) %>%
#       dplyr::mutate(
#         perc_roi = 100 * area / total_area,
#         area_km2 = format(round(area, 0), big.mark = ","),
#         perc_roi = format(round(perc_roi, 1)),
#         pct_cover = format(round(pct_cover, 2))
#       ) %>%
#       dplyr::arrange(desc(perc_roi)) %>%
#       dplyr::select(zone, area_km2, perc_roi, pct_cover) %>%
#       dplyr::rename(
#         Country = zone,
#         `Area (km2)` = area_km2,
#         `% of Ecosystem` = perc_roi,
#         `% Country coverage` = pct_cover
#       )
#     
#     return(result)
#   }
#   
#   stop("Input x must be either a SpatVector or a SpatRaster.")
# }
# 
# # # # # test function
# # eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# # result_eau <- compute_countries(eau, countries)
# # result_eau
# 
# 
# 
# 
# # # test function
# # # roi <- vect("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiSumbuThicketEcoregion20kmBuffer.shp")
# # # result_shp <- compute_country_overlap_roi(roi, countries)
# # 
# # 
# # countries_filtered <- countries[countries$admin %in% result_eau$Country, ]
# # countries_filtered
# # 
# # library(wdpar)
# # test <- wdpa_fetch(countries_filtered$iso_a3[1])
# #
# 
# 

# country_overlap.R
#
# library(rnaturalearth) # ne_countries
# library(rnaturalearthdata)
# library(terra)
# library(exactextractr)
# library(sf)
# # load data
# countries <- vect(ne_countries(scale = "medium", returnclass = "sf"))

# country_overlap.R

# compute temperature function
# will work for both spatvector and spatraster

compute_countries <- function(x, countries) {
  if (is.null(countries)) stop("Countries data not available")
  if (is.null(x)) stop("ROI not loaded")
  # -------------------------------------------------------------------------
  # spatvector
  if (inherits(x, "SpatVector")) {

    countries_proj <- terra::project(countries, "EPSG:4326")
    roi_proj       <- terra::project(x,         "EPSG:4326")

    roi_int <- tryCatch({
      terra::intersect(countries_proj, roi_proj)
    }, error = function(e) {
      message("Error during intersect: ", e$message)
      NULL
    })

    if (is.null(roi_int) || nrow(roi_int) == 0) return(data.frame())

    roi_int$area_km2 <- terra::expanse(roi_int, unit = "km")
    total_area <- terra::expanse(roi_proj, unit = "km")

    result <- as.data.frame(roi_int) %>%
      dplyr::group_by(admin) %>%
      dplyr::summarise(area_km2 = sum(area_km2), .groups = "drop") %>%
      dplyr::mutate(
        perc_roi = 100 * area_km2 / total_area,
        area_km2 = format(round(area_km2, 0), big.mark = ","),
        perc_roi = format(round(perc_roi, 1))
      ) %>%
      dplyr::arrange(desc(perc_roi)) %>%
      dplyr::rename(
        Country = admin,
        `Area (km²)` = area_km2,
        `% of Ecosystem` = perc_roi
      )

    return(result)
  }

  # -------------------------------------------------------------------------
  # spatraster
  if (inherits(x, "SpatRaster")) {

    # # 0. Match CRS first — every spatial op below needs them aligned
    # if (!terra::same.crs(countries, x)) {
    #   countries <- terra::project(countries, terra::crs(x))
    # }

    # # 1. Reduce country polygons to raster extent
    # countries_eau <- terra::mask(countries, terra::ext(x))

    # Project the raster's extent to the countries CRS (cheap — just 4 corners)
    # then crop countries before projecting (avoids domain errors for far countries)
    roi_ext <- terra::project(
      terra::as.polygons(terra::ext(x), crs = terra::crs(x)),
      terra::crs(countries)
    )

    countries_touch <- countries[terra::is.related(countries, roi_ext, "intersects"), ]
    if (nrow(countries_touch) == 0) return(data.frame())
    
    #  Full-country areas, computed once in native lon/lat (geodesic)
    country_totals <- data.frame(
      zone             = countries_touch$admin,
      country_area_km2 = terra::expanse(countries_touch, unit = "km")
    )
    
    # Clip only the hit countries for projection/rasterising
    countries_eau <- terra::crop(countries_touch, roi_ext)
    if (!terra::same.crs(countries_eau, x)) {
      countries_eau <- terra::project(countries_eau, terra::crs(x))
    }
    
    
    # 2. Aggregate raster (speed)
    # eau_agg <- terra::aggregate(x, 2, fun = "min", na.rm = TRUE)
    eau_agg <- spatSample(x, 1000000, method = "regular",
                      as.raster = TRUE, na.rm = TRUE)
    if (is.null(eau_agg) || ncell(eau_agg) == 0) return(data.frame())

    # 3. Rasterize country polygons to match aggregated raster
    eau_r <- terra::rasterize(countries_eau, eau_agg, field = "admin")

    # 4. Zonal count of non-NA raster pixels
    # z_result <- terra::zonal(eau_agg, eau_r, fun = "notNA", na.rm = TRUE)

    # 5. Compute area per country zone
    area_per_country <- terra::expanse(eau_agg, zones = eau_r, unit = "km")

    total_area <- sum(area_per_country$area)

    result <- area_per_country %>%
      dplyr::left_join(country_totals, by = "zone") %>%
      dplyr::mutate(
        perc_roi  = 100 * area / total_area,
        pct_cover = 100 * area / country_area_km2
      ) %>%
      dplyr::arrange(dplyr::desc(perc_roi)) %>%   # sort BEFORE formatting
      dplyr::transmute(
        Country              = zone,
        `Area (km²)`         = format(round(area, 0), big.mark = ","),
        `% of Ecosystem`     = format(round(perc_roi, 1)),
        `% Country coverage` = format(round(pct_cover, 2))
      )

    return(result)
  }

  stop("Input x must be either a SpatVector or a SpatRaster.")
}

# # # # test function
eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924_2015.tif")
result_eau <- compute_countries(eau, countries)
result_eau




# # test function
# # roi <- vect("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiSumbuThicketEcoregion20kmBuffer.shp")
# # result_shp <- compute_country_overlap_roi(roi, countries)
#
#
# countries_filtered <- countries[countries$admin %in% result_eau$Country, ]
# countries_filtered
#
# library(wdpar)
# test <- wdpa_fetch(countries_filtered$iso_a3[1])
#



