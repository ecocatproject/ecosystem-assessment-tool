calc_area_change <- function(area.data) {
  
  has_bound <- "Bound" %in% names(area.data)
  
  if (has_bound) {
    # ---- Bound raster path: calculate changes separately per bound ----------
    area.data <- area.data %>%
      dplyr::select(Bound, Year, Area_km2) %>%
      dplyr::mutate(
        year     = round(as.numeric(Year), 0),
        area     = Area_km2,
        Year     = NULL,
        Area_km2 = NULL
      ) %>%
      dplyr::group_by(Bound) %>%
      dplyr::arrange(year, .by_group = TRUE) %>%
      dplyr::mutate(
        dt                       = year - lag(year),
        area_change              = area - lag(area),
        proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
        absolute_rate_change     = area_change / dt
      )
    
    return(area.data)
    
  } else {
    # ---- Standard path (existing logic) -------------------------------------
    area.data <- area.data %>%
      dplyr::select(Year, Area_km2) %>%
      dplyr::mutate(
        year     = round(as.numeric(Year), 0),
        area     = Area_km2,
        Year     = NULL,
        Area_km2 = NULL
      ) %>%
      dplyr::arrange(year)
    
    obsv_change <- area.data %>%
      dplyr::mutate(
        dt                       = year - lag(year),
        area_change              = area - lag(area),
        proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
        absolute_rate_change     = area_change / dt
      )
    
    return(obsv_change)
  }
}


# Internal workhorse — operates on a single, ungrouped set of area-change rows.
# n_recent: how many recent periods to average for the near-term rate (default = 1)
predict_area_change_single <- function(obsv_change, horizon = 50, n_recent = 1) {
  
  obsv_change  <- obsv_change %>% dplyr::arrange(year)
  oldest       <- obsv_change %>% slice(1)
  recent       <- obsv_change %>% slice(n())
  
  nYearsPast <- horizon - (max(obsv_change$year) - min(obsv_change$year))
  if (nYearsPast < 0) nYearsPast <- 0
  
  project_seq <- function(start_area, rate, type, years,
                          direction = c("future", "past")) {
    direction <- match.arg(direction)
    data.frame(i = 0:years) %>%
      mutate(
        area = case_when(
          type == "PRC" & direction == "future" ~ start_area * (1 + rate/100)^i,
          type == "PRC" & direction == "past"   ~ start_area * (1 + rate/100)^(-i),
          type == "ARC" & direction == "future" ~ start_area + rate * i,
          type == "ARC" & direction == "past"   ~ start_area - rate * i,
          TRUE ~ NA_real_
        ),
        area   = ifelse(direction == "past" & !is.na(area) & area <= 0, NA_real_, area),
        area   = ifelse(direction == "future" & !is.na(area), pmax(area, 0), area),
        year   = if (direction == "future") recent$year + i else oldest$year - i,
        method = type
      )
  }
  
  med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
  med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
  
  # Near-term rate: mean of the last n_recent observed periods
  # n_recent = 1 (default) = most recent period only
  # n_recent = 2           = mean of 2 most recent periods
  recent_PRC <- mean(
    tail(na.omit(obsv_change$proportional_rate_change), n_recent),
    na.rm = TRUE
  )
  recent_ARC <- mean(
    tail(na.omit(obsv_change$absolute_rate_change), n_recent),
    na.rm = TRUE
  )
  
  proj <- imap_dfr(
    list(recent = NULL, median = NULL),
    function(., scen) {
      
      # Past uses median rate — oldest-period rate can give 0/negative backward projections
      past_PRC <- med_PRC
      past_ARC <- med_ARC
      fut_PRC  <- if (scen == "recent") recent_PRC else med_PRC
      fut_ARC  <- if (scen == "recent") recent_ARC else med_ARC
      
      bind_rows(
        project_seq(oldest$area, past_PRC, "PRC", nYearsPast, "past")   %>% mutate(segment = "past"),
        project_seq(oldest$area, past_ARC, "ARC", nYearsPast, "past")   %>% mutate(segment = "past"),
        project_seq(recent$area, fut_PRC,  "PRC", horizon,    "future") %>% mutate(segment = "future"),
        project_seq(recent$area, fut_ARC,  "ARC", horizon,    "future") %>% mutate(segment = "future")
      ) %>%
        mutate(scenario = scen, type = "projected") %>%
        dplyr::select(year, area, type, method, scenario, segment) %>%
        dplyr::arrange(method, year)
    }
  )
  
  observed <- obsv_change %>%
    dplyr::select(year, area) %>%
    mutate(type = "observed", method = "observed", scenario = "observed", segment = "observed")
  
  bind_rows(observed, proj)
}


# # Public function — dispatches to single-bound or per-bound processing.
# # When obsv_change contains a "Bound" column (min/opt/max raster type),
# # predictions are run independently for each bound and tagged with Bound.
# # calculate area change
# # Handles both standard area data (Year, Area_km2) and
# # bound-type data (Year, Bound, Area_km2) where Bound ∈ {"min","opt","max"}.
# calc_area_change <- function(area.data) {
#   
#   has_bound <- "Bound" %in% names(area.data)
#   
#   if (has_bound) {
#     # ---- Bound raster path: calculate changes separately per bound ----------
#     area.data <- area.data %>%
#       dplyr::select(Bound, Year, Area_km2) %>%
#       dplyr::mutate(
#         year     = round(as.numeric(Year), 0),
#         area     = Area_km2,
#         Year     = NULL,
#         Area_km2 = NULL
#       ) %>%
#       dplyr::group_by(Bound) %>%
#       dplyr::arrange(year, .by_group = TRUE) %>%
#       dplyr::mutate(
#         dt                       = year - lag(year),
#         area_change              = area - lag(area),
#         proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
#         absolute_rate_change     = area_change / dt
#       )
#     
#     return(area.data)
#     
#   } else {
#     # ---- Standard path (existing logic) -------------------------------------
#     area.data <- area.data %>%
#       dplyr::select(Year, Area_km2) %>%
#       dplyr::mutate(
#         year     = round(as.numeric(Year), 0),
#         area     = Area_km2,
#         Year     = NULL,
#         Area_km2 = NULL
#       ) %>%
#       dplyr::arrange(year)
#     
#     obsv_change <- area.data %>%
#       dplyr::mutate(
#         dt                       = year - lag(year),
#         area_change              = area - lag(area),
#         proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
#         absolute_rate_change     = area_change / dt
#       )
#     
#     return(obsv_change)
#   }
# }


# Internal workhorse — operates on a single, ungrouped set of area-change rows.
# (was previously the top-level predict_area_change)
predict_area_change_single <- function(obsv_change, horizon = 50, n_recent = 1) {
  
  obsv_change <- dplyr::ungroup(obsv_change) %>% dplyr::arrange(year)
  
  # Need ≥ 2 observations to compute any rates at all
  if (nrow(obsv_change) < 2) return(data.frame())
  
  oldest <- obsv_change %>% slice(1)
  recent <- obsv_change %>% slice(n())
  
  nYearsPast <- horizon - (max(obsv_change$year) - min(obsv_change$year))
  if (nYearsPast < 0) nYearsPast <- 0
  
  project_seq <- function(start_area, rate, type, years,
                          direction = c("future", "past")) {
    direction <- match.arg(direction)
    data.frame(i = 0:years) %>%
      mutate(
        area = case_when(
          type == "PRC" & direction == "future" ~ start_area * (1 + rate/100)^i,
          type == "PRC" & direction == "past"   ~ start_area * (1 + rate/100)^(-i),
          type == "ARC" & direction == "future" ~ start_area + rate * i,
          type == "ARC" & direction == "past"   ~ start_area - rate * i,
          TRUE ~ NA_real_
        ),
        area   = ifelse(direction == "past" & !is.na(area) & area <= 0, NA_real_, area),
        area   = ifelse(direction == "future" & !is.na(area), pmax(area, 0), area),
        year   = if (direction == "future") recent$year + i else oldest$year - i,
        method = type
      )
  }
  
  med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
  med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
  
  # Near-term rate: mean of the last n_recent observed periods
  recent_PRC <- mean(tail(na.omit(obsv_change$proportional_rate_change), n_recent), na.rm = TRUE)
  recent_ARC <- mean(tail(na.omit(obsv_change$absolute_rate_change),     n_recent), na.rm = TRUE)
  med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
  
  proj <- imap_dfr(
    list(recent = NULL, median = NULL),
    function(., scen) {
      
      # Past uses median rate — oldest-period rate can give 0/negative backward projections
      past_PRC <- med_PRC
      past_ARC <- med_ARC
      fut_PRC  <- if (scen == "recent") recent_PRC else med_PRC
      fut_ARC  <- if (scen == "recent") recent_ARC else med_ARC
      
      bind_rows(
        project_seq(oldest$area, past_PRC, "PRC", nYearsPast, "past")   %>% mutate(segment = "past"),
        project_seq(oldest$area, past_ARC, "ARC", nYearsPast, "past")   %>% mutate(segment = "past"),
        project_seq(recent$area, fut_PRC,  "PRC", horizon,    "future") %>% mutate(segment = "future"),
        project_seq(recent$area, fut_ARC,  "ARC", horizon,    "future") %>% mutate(segment = "future")
      ) %>%
        mutate(scenario = scen, type = "projected") %>%
        dplyr::select(year, area, type, method, scenario, segment) %>%
        dplyr::arrange(method, year)
    }
  )
  
  observed <- obsv_change %>%
    dplyr::select(year, area) %>%
    mutate(type = "observed", method = "observed", scenario = "observed", segment = "observed")
  
  bind_rows(observed, proj)
}


# Public function — dispatches to single-bound or per-bound processing.
# When obsv_change contains a "Bound" column (min/opt/max raster type),
# predictions are run independently for each bound and tagged with Bound.
predict_area_change <- function(obsv_change, horizon = 50, n_recent = 1) {
  
  if ("Bound" %in% names(obsv_change)) {
    bounds <- unique(dplyr::pull(obsv_change, Bound))
    
    purrr::map_dfr(bounds, function(b) {
      predict_area_change_single(
        obsv_change %>%
          dplyr::filter(Bound == b) %>%
          dplyr::ungroup(),
        horizon, n_recent
      ) %>%
        dplyr::mutate(Bound = b)
    })
    
  } else {
    predict_area_change_single(dplyr::ungroup(obsv_change), horizon, n_recent)
  }
}

# -----------------------------------------------------------------------------
# Criterion B functions
# -----------------------------------------------------------------------------

# function needed for MCP calculation
makeEOO.SpatRaster <- function(input.data, year){
  EOO.polygon <- as.polygons(input.data, na.rm=T)
  EOO.poly <- hull(EOO.polygon, type="convex")
  # add attributes directly
  EOO.poly$year <- year
  EOO.poly$area_km2 <- terra::expanse(EOO.poly, unit = "km")
  
  return(EOO.poly)
}

# calc_area_change <- function(area.data) {
#   
#   has_bound <- "Bound" %in% names(area.data)
#   
#   if (has_bound) {
#     # ---- Bound raster path: calculate changes separately per bound ----------
#     area.data <- area.data %>%
#       dplyr::select(Bound, Year, Area_km2) %>%
#       dplyr::mutate(
#         year     = round(as.numeric(Year), 0),
#         area     = Area_km2,
#         Year     = NULL,
#         Area_km2 = NULL
#       ) %>%
#       dplyr::group_by(Bound) %>%
#       dplyr::arrange(year, .by_group = TRUE) %>%
#       dplyr::mutate(
#         dt                       = year - lag(year),
#         area_change              = area - lag(area),
#         proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
#         absolute_rate_change     = area_change / dt
#       )
#     
#     return(area.data)
#     
#   } else {
#     # ---- Standard path (existing logic) -------------------------------------
#     area.data <- area.data %>%
#       dplyr::select(Year, Area_km2) %>%
#       dplyr::mutate(
#         year     = round(as.numeric(Year), 0),
#         area     = Area_km2,
#         Year     = NULL,
#         Area_km2 = NULL
#       ) %>%
#       dplyr::arrange(year)
#     
#     obsv_change <- area.data %>%
#       dplyr::mutate(
#         dt                       = year - lag(year),
#         area_change              = area - lag(area),
#         proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
#         absolute_rate_change     = area_change / dt
#       )
#     
#     return(obsv_change)
#   }
# }
# 
# 
# # Internal workhorse — operates on a single, ungrouped set of area-change rows.
# # (was previously the top-level predict_area_change)
# predict_area_change_single <- function(obsv_change, horizon = 50) {
#   
#   obsv_change  <- obsv_change %>% dplyr::arrange(year)
#   oldest       <- obsv_change %>% slice(1)
#   oldest_rates <- obsv_change %>% slice(2)   # rates from the earliest observed period
#   recent       <- obsv_change %>% slice(n())
#   
#   nYearsPast <- horizon - (max(obsv_change$year) - min(obsv_change$year))
#   if (nYearsPast < 0) nYearsPast <- 0
#   
#   project_seq <- function(start_area, rate, type, years,
#                           direction = c("future", "past")) {
#     direction <- match.arg(direction)
#     data.frame(i = 0:years) %>%
#       mutate(
#         area = case_when(
#           type == "PRC" & direction == "future" ~ start_area * (1 + rate/100)^i,
#           type == "PRC" & direction == "past"   ~ start_area * (1 + rate/100)^(-i),
#           type == "ARC" & direction == "future" ~ start_area + rate * i,
#           type == "ARC" & direction == "past"   ~ start_area - rate * i,
#           TRUE ~ NA_real_
#         ),
#         area   = ifelse(direction == "past" & !is.na(area) & area <= 0, NA_real_, area),
#         area   = ifelse(direction == "future" & !is.na(area), pmax(area, 0), area),
#         year   = if (direction == "future") recent$year + i else oldest$year - i,
#         method = type
#       )
#   }
#   
#   med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
#   med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
#   
#   proj <- imap_dfr(
#     list(recent = NULL, median = NULL),
#     function(., scen) {
#       
#       # Past uses median rate — oldest-period rate can give 0/negative backward projections
#       past_PRC <- med_PRC
#       past_ARC <- med_ARC
#       fut_PRC  <- if (scen == "recent") recent$proportional_rate_change       else med_PRC
#       fut_ARC  <- if (scen == "recent") recent$absolute_rate_change           else med_ARC
#       
#       bind_rows(
#         project_seq(oldest$area, past_PRC, "PRC", nYearsPast, "past")   %>% mutate(segment = "past"),
#         project_seq(oldest$area, past_ARC, "ARC", nYearsPast, "past")   %>% mutate(segment = "past"),
#         project_seq(recent$area, fut_PRC,  "PRC", horizon,    "future") %>% mutate(segment = "future"),
#         project_seq(recent$area, fut_ARC,  "ARC", horizon,    "future") %>% mutate(segment = "future")
#       ) %>%
#         mutate(scenario = scen, type = "projected") %>%
#         dplyr::select(year, area, type, method, scenario, segment) %>%
#         dplyr::arrange(method, year)
#     }
#   )
#   
#   observed <- obsv_change %>%
#     dplyr::select(year, area) %>%
#     mutate(type = "observed", method = "observed", scenario = "observed", segment = "observed")
#   
#   bind_rows(observed, proj)
# }
# 
# 
# # # Public function — dispatches to single-bound or per-bound processing.
# # # When obsv_change contains a "Bound" column (min/opt/max raster type),
# # # predictions are run independently for each bound and tagged with Bound.
# # # calculate area change
# # # Handles both standard area data (Year, Area_km2) and
# # # bound-type data (Year, Bound, Area_km2) where Bound ∈ {"min","opt","max"}.
# # calc_area_change <- function(area.data) {
# #   
# #   has_bound <- "Bound" %in% names(area.data)
# #   
# #   if (has_bound) {
# #     # ---- Bound raster path: calculate changes separately per bound ----------
# #     area.data <- area.data %>%
# #       dplyr::select(Bound, Year, Area_km2) %>%
# #       dplyr::mutate(
# #         year     = round(as.numeric(Year), 0),
# #         area     = Area_km2,
# #         Year     = NULL,
# #         Area_km2 = NULL
# #       ) %>%
# #       dplyr::group_by(Bound) %>%
# #       dplyr::arrange(year, .by_group = TRUE) %>%
# #       dplyr::mutate(
# #         dt                       = year - lag(year),
# #         area_change              = area - lag(area),
# #         proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
# #         absolute_rate_change     = area_change / dt
# #       )
# #     
# #     return(area.data)
# #     
# #   } else {
# #     # ---- Standard path (existing logic) -------------------------------------
# #     area.data <- area.data %>%
# #       dplyr::select(Year, Area_km2) %>%
# #       dplyr::mutate(
# #         year     = round(as.numeric(Year), 0),
# #         area     = Area_km2,
# #         Year     = NULL,
# #         Area_km2 = NULL
# #       ) %>%
# #       dplyr::arrange(year)
# #     
# #     obsv_change <- area.data %>%
# #       dplyr::mutate(
# #         dt                       = year - lag(year),
# #         area_change              = area - lag(area),
# #         proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
# #         absolute_rate_change     = area_change / dt
# #       )
# #     
# #     return(obsv_change)
# #   }
# # }
# 
# 
# # Internal workhorse — operates on a single, ungrouped set of area-change rows.
# # (was previously the top-level predict_area_change)
# predict_area_change_single <- function(obsv_change, horizon = 50) {
#   
#   obsv_change <- dplyr::ungroup(obsv_change) %>% dplyr::arrange(year)
#   
#   # Need ≥ 2 observations to compute any rates at all
#   if (nrow(obsv_change) < 2) return(data.frame())
#   
#   oldest       <- obsv_change %>% slice(1)
#   oldest_rates <- obsv_change %>% slice(2)   # rates from the earliest observed period
#   recent       <- obsv_change %>% slice(n())
#   
#   # Guard: oldest_rates must carry valid (non-NA, non-empty) rate columns
#   if (nrow(oldest_rates) == 0 ||
#       is.na(oldest_rates$proportional_rate_change) ||
#       is.na(oldest_rates$absolute_rate_change)) {
#     # Fall back to using the most-recent rates for both scenarios
#     oldest_rates <- recent
#   }
#   
#   nYearsPast <- horizon - (max(obsv_change$year) - min(obsv_change$year))
#   if (nYearsPast < 0) nYearsPast <- 0
#   
#   project_seq <- function(start_area, rate, type, years,
#                           direction = c("future", "past")) {
#     direction <- match.arg(direction)
#     data.frame(i = 0:years) %>%
#       mutate(
#         area = case_when(
#           type == "PRC" & direction == "future" ~ start_area * (1 + rate/100)^i,
#           type == "PRC" & direction == "past"   ~ start_area * (1 + rate/100)^(-i),
#           type == "ARC" & direction == "future" ~ start_area + rate * i,
#           type == "ARC" & direction == "past"   ~ start_area - rate * i,
#           TRUE ~ NA_real_
#         ),
#         area   = ifelse(direction == "past" & !is.na(area) & area <= 0, NA_real_, area),
#         area   = ifelse(direction == "future" & !is.na(area), pmax(area, 0), area),
#         year   = if (direction == "future") recent$year + i else oldest$year - i,
#         method = type
#       )
#   }
#   
#   med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
#   med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
#   
#   proj <- imap_dfr(
#     list(recent = NULL, median = NULL),
#     function(., scen) {
#       
#       # Past uses median rate — oldest-period rate can give 0/negative backward projections
#       past_PRC <- med_PRC
#       past_ARC <- med_ARC
#       fut_PRC  <- if (scen == "recent") recent$proportional_rate_change       else med_PRC
#       fut_ARC  <- if (scen == "recent") recent$absolute_rate_change           else med_ARC
#       
#       bind_rows(
#         project_seq(oldest$area, past_PRC, "PRC", nYearsPast, "past")   %>% mutate(segment = "past"),
#         project_seq(oldest$area, past_ARC, "ARC", nYearsPast, "past")   %>% mutate(segment = "past"),
#         project_seq(recent$area, fut_PRC,  "PRC", horizon,    "future") %>% mutate(segment = "future"),
#         project_seq(recent$area, fut_ARC,  "ARC", horizon,    "future") %>% mutate(segment = "future")
#       ) %>%
#         mutate(scenario = scen, type = "projected") %>%
#         dplyr::select(year, area, type, method, scenario, segment) %>%
#         dplyr::arrange(method, year)
#     }
#   )
#   
#   observed <- obsv_change %>%
#     dplyr::select(year, area) %>%
#     mutate(type = "observed", method = "observed", scenario = "observed", segment = "observed")
#   
#   bind_rows(observed, proj)
# }
# 
# 
# # Public function — dispatches to single-bound or per-bound processing.
# # When obsv_change contains a "Bound" column (min/opt/max raster type),
# # predictions are run independently for each bound and tagged with Bound.
# predict_area_change <- function(obsv_change, horizon = 50) {
#   
#   if ("Bound" %in% names(obsv_change)) {
#     bounds <- unique(dplyr::pull(obsv_change, Bound))
#     
#     purrr::map_dfr(bounds, function(b) {
#       predict_area_change_single(
#         obsv_change %>%
#           dplyr::filter(Bound == b) %>%
#           dplyr::ungroup(),
#         horizon
#       ) %>%
#         dplyr::mutate(Bound = b)
#     })
#     
#   } else {
#     predict_area_change_single(dplyr::ungroup(obsv_change), horizon)
#   }
# }
# 
# # -----------------------------------------------------------------------------
# # Criterion B functions
# # -----------------------------------------------------------------------------
# 
# # function needed for MCP calculation
# makeEOO.SpatRaster <- function(input.data, year){
#   EOO.polygon <- as.polygons(input.data, na.rm=T)
#   EOO.poly <- hull(EOO.polygon, type="convex")
#   # add attributes directly
#   EOO.poly$year <- year
#   EOO.poly$area_km2 <- terra::expanse(EOO.poly, unit = "km")
#   
#   return(EOO.poly)
# }


