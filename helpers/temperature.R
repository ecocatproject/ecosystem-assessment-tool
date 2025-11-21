# temperature.R

# compute temperature function
# will work for both spatvector and spatraster

compute_temperature <- function(x, 
                                temperature_data = "data/temperature/wc2.1_10m_tavg",
                                n_sample = 1000000) {
  
  # load files
  t_files <- list.files(temperature_data, full.names = TRUE)
  
  # determine type
  if (inherits(x, "SpatVector")) {
    # -------------------------------------------------------------------------
    # spatvector 
    #x <- vect(x) # needed of use name an not already a terra object
    t_10 <- t_files |> rast(win = terra::ext(x)) |> crop(x) |> mask(x)
    
    stats <- terra::global(t_10_crop, c("mean", "range"), na.rm = TRUE)
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
    #x <- rast(x) # needed of use name an not already a terra object
    t_10 <- t_files |> rast(win = terra::ext(x))
    
    pts <- spatSample(x, n_sample, method = "regular",
                      as.points = TRUE, na.rm = TRUE)
    
    vals <- terra::extract(t_10, pts)[ , -1]  # remove ID col
    
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
    mutate_if(is.numeric, round, 1)
  
  return(smooth_df)
}

# 
# eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# system.time({result_eau <- compute_temperature(x=eau)})
# result_eau

# t_10 <- list.files("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/EcoCat/Code/R/modularised/data/temperature/wc2.1_10m_tavg", full.names = TRUE) |>
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
#   max   = apply(vals, 2, max, na.rm = TRUE)
# )


