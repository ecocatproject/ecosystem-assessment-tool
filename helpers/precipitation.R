# helpers/precipitation.R

# compute precipitaiton function
# data downloaded from - https://www.worldclim.org/data/worldclim21.html
compute_precipitation <- function(x, 
                                data = "data/precipitation",
                                n_sample = 1000000) {
  
  # load files
  data_files <- list.files(data, full.names = TRUE)
  
  # determine type
  if (inherits(x, "SpatVector")) {
    # -------------------------------------------------------------------------
    # spatvector 
    #x <- vect(x) # needed of use name an not already a terra object
    data_10m <- data_files |> rast(win = terra::ext(x)) |> crop(x) |> mask(x)
    
    stats <- terra::global(data_10m, c("mean", "range"), na.rm = TRUE)
    stats$month <- lubridate::month(1:12, label = TRUE)
    
    monthly_avg <- stats %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(
        mean = mean(mean, na.rm = TRUE),
        min  = min(min, na.rm = TRUE),
        max  = max(max, na.rm = TRUE)
      ) %>%
      dplyr::arrange(match(month, month.abb))
    
  } else if (inherits(x, "SpatRaster")) {
    
    # -------------------------------------------------------------------------
    # spatraster
    
    if(crs(rast(data_files)) != crs(x)){
      x <- project(x, crs(rast(data_files)))
    }

    data_10m <- data_files |> rast(win = terra::ext(x))
    
    pts <- spatSample(x, n_sample, method = "regular",
                      as.points = TRUE, na.rm = TRUE)
    
    vals <- terra::extract(data_10m, pts)[ , -1]  # remove ID col
    
    monthly_avg <- data.frame(
      month = lubridate::month(1:12, label = TRUE),
      mean  = apply(vals, 2, mean, na.rm = TRUE),
      min   = apply(vals, 2, min, na.rm = TRUE),
      max   = apply(vals, 2, max, na.rm = TRUE)
    )
    
  } else {
    stop("Input must be a SpatVector or SpatRaster.")
  }
  
  # ---------------------------------------------------------------------------
  # smooting for plots
  smooth_df <- monthly_avg %>%
    mutate(
      min_smooth  = predict(loess(min  ~ as.numeric(month), span = 0.5)),
      max_smooth  = predict(loess(max  ~ as.numeric(month), span = 0.5)),
      mean_smooth = predict(loess(mean ~ as.numeric(month), span = 0.5))
    ) %>% 
    mutate_if(is.numeric, round, 1) %>%
    mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))# <0 to 0s
  
  return(smooth_df)
}

# 
# eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# system.time({result_eau <- compute_precipitation(x=eau)})
# result_eau

# # t_10 <- list.files("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/EcoCat/Code/R/modularised/data/temperature/wc2.1_10m_tavg", full.names = TRUE) |>
# t_10 <- list.files("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/EcoCat/Code/R/modularised_testing/data/precipitation", full.names = TRUE) |>
#   rast(win = terra::ext(eau))
# pts <- spatSample(eau, n_sample, method = "random",
#                   as.points = TRUE, na.rm = TRUE)
# 
# vals <- terra::extract(t_10, pts)[ , -1]  # remove ID col
# 
# monthly_avg <- data.frame(
#   month = lubridate::month(1:12, label = TRUE),
#   mean  = apply(vals, 2, mean, na.rm = TRUE),
#   min   = apply(vals, 2, min, na.rm = TRUE),
#   max   = apply(vals, 2, max, na.rm = TRUE) %>%
# )



#################
# old version
# compute_rainfall <- function(roi) {
#   
#   # Load and crop TerraClim rainfall
#   roi_rast <- getTerraClim(roi, varname = "ppt",
#                            startDate = "2010-01-01",
#                            endDate = "2024-12-31") |>
#     {\(x) crop(x$ppt, roi) |> mask(roi)}()
#   
#   # Extract dates and months
#   dates <- as.Date(gsub("ppt_|_total", "", names(roi_rast)))
#   months <- lubridate::month(dates, label = TRUE)
#   
#   # Compute mean, min, max per layer
#   layer_stats <- terra::global(roi_rast, c("mean", "range"), na.rm = TRUE) |> as.data.frame()
#   layer_stats$date <- dates
#   layer_stats$month <- months
#   layer_stats$month_num <- as.numeric(months)  # numeric for loess smoothing
#   
#   # Aggregate by month
#   monthly_avg <- layer_stats %>%
#     dplyr::group_by(month) %>%
#     dplyr::summarise(
#       mean = mean(mean, na.rm = TRUE),
#       min = min(min, na.rm = TRUE),
#       max = max(max, na.rm = TRUE)
#     ) %>%
#     dplyr::arrange(match(month, month.abb))  # ensure Jan-Dec order
#   
#   
#   # Smooth min, mean, max with loess
#   smooth_df <- monthly_avg %>%
#     mutate(
#       min_smooth  = predict(loess(min  ~ as.numeric(month), span = 0.5)),
#       max_smooth  = predict(loess(max  ~ as.numeric(month), span = 0.5)),
#       mean_smooth = predict(loess(mean ~ as.numeric(month), span = 0.5))
#     ) %>% mutate_if(is.numeric, round, 1) %>%
#     mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))# <0 to 0s
#   
#   return(smooth_df)
# }

