# analysis functions

# -----------------------------------------------------------------------------
# Criterion A functions
# -----------------------------------------------------------------------------


# calculate area change
calc_area_change <- function(area.data) {
  
  # calculate stats for observed changes 
  area.data <- area.data %>%
    dplyr::select(Year, Area_km2) %>%
    dplyr::mutate(year = round(as.numeric(Year),0), area = Area_km2, Year = NULL, Area_km2 = NULL) %>%
    dplyr::arrange(year)
  
  obsv_change <- area.data %>%
    dplyr::mutate(
      area_change = lag(area) - area,
      proportional_rate_change = (1 - (lag(area)/area)^(1/(lag(year)-year))) * 100,
      absolute_rate_change = (lag(area) - area) / (lag(year)-year)#,
      #annual_rate_change = (1 / (lag(year)-year)) * log(lag(area)/area) * 100
    )
  return(obsv_change)
}



# Function to calculate decline statistics + projections
predict_area_change <- function(obsv_change, horizon = 50) {
  
  # helper data for projections
  oldest <- cbind((obsv_change %>% slice(1))[c(1:2)],                       # select the 1st row of year and area data, but need the next decline stat
                  (obsv_change %>% slice(2))[c(3:5)])                       # select the  1st set of decline stats
  recent <- obsv_change %>% slice(n())                                      # choose the most recent row
  nYearsPast <- (horizon - (max(obsv_change$year) - min(obsv_change$year))) # oldest data
  if (nYearsPast < 0) nYearsPast <- 0                                       # safeguard if there happens to be early estimates?
  
  # projection function
  # this is used separately for future and past projections
  # and is applied to future projections using imap (purrr package)
  project_seq <- function(start_area, rate, type, years, direction = "future") {
    seq_df <- data.frame(i = 0:years) %>%
      mutate(
        area = case_when(
          # type == "ARC" ~ start_area * exp((rate/100) * i),
          type == "PRC" ~ start_area * (1 - (rate/100))^i,
          type == "ARC" ~ start_area + rate * i,
          TRUE ~ NA_real_
        ),
        area = pmax(area, 0),   # this prevents negative values
        year = if (direction == "future") recent$year + i else oldest$year - i,
        ptype = "projected",
        method = type
      ) 
    return(seq_df)
  }
  
  # Past extrapolation (using oldest rate)
  past_proj <- bind_rows(
    # project_seq(oldest$area, -oldest$annual_rate_change, "ARC", nYearsPast, "past"),
    project_seq(oldest$area, -oldest$proportional_rate_change, "PRC", nYearsPast, "past"),
    project_seq(oldest$area, -oldest$absolute_rate_change, "ARC", nYearsPast, "past")
  ) %>% mutate(scenario = "past") %>%
    select(year, area, type = ptype, method, scenario) %>%
    arrange(year)
  
  # Future projections
  scenarios <- list(
    median = list(
      # ARC = median(area$annual_rate_change, na.rm = TRUE),
      PRC = median(obsv_change$proportional_rate_change, na.rm = TRUE),
      ARC = median(obsv_change$absolute_rate_change, na.rm = TRUE)
    ),
    recent = list(
      # ARC = recent$annual_rate_change,
      PRC = recent$proportional_rate_change,
      ARC = recent$absolute_rate_change
    ),
    worst = list(
      # ARC = min(area$annual_rate_change, na.rm = TRUE),
      PRC = max(obsv_change$proportional_rate_change, na.rm = TRUE),
      ARC = min(obsv_change$absolute_rate_change, na.rm = TRUE)
    )
  )
  
  # If recent and worst are the same, merge them into "recent/worst"
  equal_check <- all.equal(scenarios$recent, scenarios$worst, tolerance = 1e-8)
  if (isTRUE(equal_check)) {
    scenarios$`recent/worst` <- scenarios$worst
    scenarios$recent <- NULL
    scenarios$worst <- NULL
  }
  
  future_proj <- imap_dfr(scenarios, function(rates, scen) { # this iterates over 2 arguements at a time (a bit like seq_along...)
    bind_rows(
      # project_seq(recent$area, rates$ARC, "ARC", horizon),
      project_seq(recent$area, rates$PRC, "PRC", horizon),
      project_seq(recent$area, rates$ARC, "ARC", horizon)
    ) %>% mutate(scenario = scen) %>%
      select(year, area, type = ptype, method, scenario) %>%
      arrange(year)
  })
  
  # Combine observed + projections
  observed <- obsv_change %>% 
    select(year, area) %>%                                                # select the data required for combining
    mutate(type = "observed", method = "observed", scenario = "observed") # force names to comply with other data
  
  out <- bind_rows(past_proj, observed,  future_proj)                     # combine the 3 sets of data
  
  return(out)
}

# -----------------------------------------------------------------------------
# Criterion B functions
# -----------------------------------------------------------------------------

# function needed for MCP calculation
makeEOO.SpatRaster <- function(input.data, year){
  EOO.polygon <- as.polygons(input.data, na.rm=T)
  EOO.poly <- convHull(EOO.polygon)
  # add attributes directly
  EOO.poly$year <- year
  EOO.poly$area_km2 <- terra::expanse(EOO.poly, unit = "km")
  
  return(EOO.poly)
}
