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
# (was previously the top-level predict_area_change)
predict_area_change_single <- function(obsv_change, horizon = 50) {
  
  obsv_change  <- obsv_change %>% dplyr::arrange(year)
  oldest       <- obsv_change %>% slice(1)
  oldest_rates <- obsv_change %>% slice(2)   # rates from the earliest observed period
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
        area   = pmax(area, 0),
        year   = if (direction == "future") recent$year + i else oldest$year - i,
        method = type
      )
  }
  
  med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
  med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
  
  proj <- imap_dfr(
    list(recent = NULL, median = NULL),
    function(., scen) {
      
      past_PRC <- if (scen == "recent") oldest_rates$proportional_rate_change else med_PRC
      past_ARC <- if (scen == "recent") oldest_rates$absolute_rate_change     else med_ARC
      fut_PRC  <- if (scen == "recent") recent$proportional_rate_change       else med_PRC
      fut_ARC  <- if (scen == "recent") recent$absolute_rate_change           else med_ARC
      
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
# calculate area change
# Handles both standard area data (Year, Area_km2) and
# bound-type data (Year, Bound, Area_km2) where Bound ∈ {"min","opt","max"}.
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
# (was previously the top-level predict_area_change)
predict_area_change_single <- function(obsv_change, horizon = 50) {
  
  obsv_change <- dplyr::ungroup(obsv_change) %>% dplyr::arrange(year)
  
  # Need ≥ 2 observations to compute any rates at all
  if (nrow(obsv_change) < 2) return(data.frame())
  
  oldest       <- obsv_change %>% slice(1)
  oldest_rates <- obsv_change %>% slice(2)   # rates from the earliest observed period
  recent       <- obsv_change %>% slice(n())
  
  # Guard: oldest_rates must carry valid (non-NA, non-empty) rate columns
  if (nrow(oldest_rates) == 0 ||
      is.na(oldest_rates$proportional_rate_change) ||
      is.na(oldest_rates$absolute_rate_change)) {
    # Fall back to using the most-recent rates for both scenarios
    oldest_rates <- recent
  }
  
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
        area   = pmax(area, 0),
        year   = if (direction == "future") recent$year + i else oldest$year - i,
        method = type
      )
  }
  
  med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
  med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
  
  proj <- imap_dfr(
    list(recent = NULL, median = NULL),
    function(., scen) {
      
      past_PRC <- if (scen == "recent") oldest_rates$proportional_rate_change else med_PRC
      past_ARC <- if (scen == "recent") oldest_rates$absolute_rate_change     else med_ARC
      fut_PRC  <- if (scen == "recent") recent$proportional_rate_change       else med_PRC
      fut_ARC  <- if (scen == "recent") recent$absolute_rate_change           else med_ARC
      
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
predict_area_change <- function(obsv_change, horizon = 50) {
  
  if ("Bound" %in% names(obsv_change)) {
    bounds <- unique(dplyr::pull(obsv_change, Bound))
    
    purrr::map_dfr(bounds, function(b) {
      predict_area_change_single(
        obsv_change %>%
          dplyr::filter(Bound == b) %>%
          dplyr::ungroup(),
        horizon
      ) %>%
        dplyr::mutate(Bound = b)
    })
    
  } else {
    predict_area_change_single(dplyr::ungroup(obsv_change), horizon)
  }
}

# # analysis functions
# 
# # -----------------------------------------------------------------------------
# # Criterion A functions
# # -----------------------------------------------------------------------------
# 
# 
# # calculate area change
# calc_area_change <- function(area.data) {
#   
#   # calculate stats for observed changes 
#   area.data <- area.data %>%
#     # group_by(Bound) %>%
#     # dplyr::select(Bound, Year, Area_km2) %>%
#     dplyr::select(Year, Area_km2) %>%
#     dplyr::mutate(year = round(as.numeric(Year),0), area = Area_km2, Year = NULL, Area_km2 = NULL) %>%
#     dplyr::arrange(year)
#   
#   obsv_change <- area.data %>%
#     dplyr::mutate(
#       dt = year - lag(year),
#       area_change = area - lag(area),   # later - earlier (signed correctly)
#       proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
#       absolute_rate_change = area_change / dt,
#     )
# # 
# #   obsv_change <- area.data %>%
# #     dplyr::mutate(
# #       area_change =area - lag(area),
# #       proportional_rate_change = (1 - (lag(area)/area)^(1/(lag(year)-year))) * 100,
# #       absolute_rate_change = (lag(area) - area) / (lag(year)-year)#,
# #     )
# #   
#   return(obsv_change)
# }
# 
# 
# 
# # Function to calculate decline statistics + projections
# # predict_area_change <- function(obsv_change, horizon = 50) {
# #   
# #   # helper data for projections
# #   oldest <- cbind(
# #     obsv_change %>% slice(1) %>% dplyr::select(year, area),                        # select the 1st row of year and area data, but need the next decline stat
# #     obsv_change %>% slice(2) %>% dplyr::select(absolute_rate_change, proportional_rate_change) # # select the  1st set of decline stats
# #   )
# #   recent <- obsv_change %>% slice(n())                                      # choose the most recent row
# #   nYearsPast <- (horizon - (max(obsv_change$year) - min(obsv_change$year))) # oldest data
# #   if (nYearsPast < 0) nYearsPast <- 0                                       # safeguard if there happens to be early estimates?
# #   
# #   # projection function
# #   # this is used separately for future and past projections
# #   # and is applied to future projections using imap (purrr package)
# #   # project_seq <- function(start_area, rate, type, years, direction = "future") {
# #     project_seq <- function(start_area, rate, type, years, direction = c("future", "past")) {
# #       direction <- match.arg(direction)
# #       
# #     seq_df <- data.frame(i = 0:years) %>%
# #       mutate(
# #         area = case_when(
# #           type == "PRC" & direction == "future" ~ start_area * (1 + rate/100)^i,
# #           type == "PRC" & direction == "past"   ~ start_area * (1 + rate/100)^(-i),
# #           type == "ARC" & direction == "future" ~ start_area + rate * i,
# #           type == "ARC" & direction == "past"   ~ start_area - rate * i,
# #           TRUE ~ NA_real_
# #         ),
# #       area = pmax(area, 0),   # this prevents negative values
# #       #  area = area,
# #         year = if (direction == "future") recent$year + i else oldest$year - i,
# #       ptype = "projected",
# #       method = type
# #       ) 
# #     return(seq_df)
# #   }
# #   
# #   # Past extrapolation (using oldest rate)
# #   past_proj <- bind_rows(
# #     # project_seq(oldest$area, -oldest$annual_rate_change, "ARC", nYearsPast, "past"),
# #     project_seq(oldest$area, oldest$proportional_rate_change, "PRC", nYearsPast, "past"),
# #     project_seq(oldest$area, oldest$absolute_rate_change, "ARC", nYearsPast, "past")
# #   ) %>% mutate(scenario = "past") %>%
# #     dplyr::select(year, area,  type = ptype, method, scenario) %>%
# #     dplyr::arrange(year)
# #   
# #   # Future projections
# #   scenarios <- list(
# #     median = list(
# #       PRC = median(obsv_change$proportional_rate_change, na.rm = TRUE),
# #       ARC = median(obsv_change$absolute_rate_change, na.rm = TRUE)
# #     ),
# #     recent = list(
# #       PRC = recent$proportional_rate_change,
# #       ARC = recent$absolute_rate_change
# #     ),
# #     worst = list(
# #       PRC = min(0, min(obsv_change$proportional_rate_change, na.rm = TRUE)), # clamped to 0 to prevent increases
# #       ARC = min(0, min(obsv_change$absolute_rate_change, na.rm = TRUE))
# #     )
# #   )
# #   
# #   # If recent and worst are the same, merge them into "recent/worst"
# #   equal_check <- all.equal(scenarios$recent, scenarios$worst, tolerance = 1e-8)
# #   if (isTRUE(equal_check)) {
# #     scenarios$`recent/worst` <- scenarios$worst
# #     scenarios$recent <- NULL
# #     scenarios$worst <- NULL
# #   }
# #   
# #   future_proj <- imap_dfr(scenarios, function(rates, scen) { # this iterates over 2 arguments at a time (a bit like seq_along...)
# #     bind_rows(
# #       project_seq(recent$area, rates$PRC, "PRC", horizon, "future"),
# #       project_seq(recent$area, rates$ARC, "ARC", horizon, "future")
# #     ) %>% mutate(scenario = scen) %>%
# #       dplyr::select(year, area,  type = ptype, method, scenario) %>%
# #       arrange(year)
# #   })
# #   
# #   # Combine observed + projections
# #   observed <- obsv_change %>% 
# #     dplyr::select(year, area) %>%                                                # select the data required for combining
# #     mutate(type = "observed", method = "observed", scenario = "observed") # force names to comply with other data
# #   
# #   out <- bind_rows(past_proj, observed,  future_proj)                     # combine the 3 sets of data
# #   
# #   return(out)
# # }
# 
# predict_area_change <- function(obsv_change, horizon = 50) {
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
#         area   = pmax(area, 0),
#         year   = if (direction == "future") recent$year + i else oldest$year - i,
#         method = type
#       )
#   }
#   
#   med_PRC <- median(obsv_change$proportional_rate_change, na.rm = TRUE)
#   med_ARC <- median(obsv_change$absolute_rate_change,     na.rm = TRUE)
#   
#   proj <- imap_dfr(
#     list(recent = NULL, median = NULL),   # just iterating over names
#     function(., scen) {
#       
#       # past uses oldest-period rates for "recent", median otherwise
#       past_PRC <- if (scen == "recent") oldest_rates$proportional_rate_change else med_PRC
#       past_ARC <- if (scen == "recent") oldest_rates$absolute_rate_change     else med_ARC
#       
#       # future uses most-recent-period rates for "recent", median otherwise  
#       fut_PRC  <- if (scen == "recent") recent$proportional_rate_change else med_PRC
#       fut_ARC  <- if (scen == "recent") recent$absolute_rate_change     else med_ARC
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
