# helpers/rle_assessment.R

# =============================================================================
# Thresholds and assessment for RLE Criteria A and B
#
# Functions used by A and B modules
# these were orgianlly in teh A and B modules code, but now moved to separate 
# file so can easily be exported as stand-alone part of the script 
# this ensures that app and downloaded scipts stay the same
# =============================================================================

# 1. Criterion A thresholds -----------------------------------------------
rle_status_decline <- function(perc_change) {
  dplyr::case_when(
    perc_change < -0.80 ~ "CR",
    perc_change < -0.50 ~ "EN",
    perc_change < -0.30 ~ "VU",
    perc_change < -0.27 ~ "NT",
    TRUE                ~ "LC"
  )
}


# 2. Criterion B1: EOO thresholds -----------------------------------------
rle_status_eoo <- function(area_km2) {
  dplyr::case_when(
    area_km2 <= 2000  ~ "CR",
    area_km2 <= 20000 ~ "EN",
    area_km2 <= 50000 ~ "VU",
    area_km2 <= 55000 ~ "NT",
    TRUE             ~ "LC"
  )
}


# 3. Criterion B2: AOO thresholds -----------------------------------------
# number of occupied 10 x 10 km cells (after the 1% rule)
rle_status_aoo <- function(n_cells) {
  dplyr::case_when(
    n_cells < 2  ~ "CR",
    n_cells < 20 ~ "EN",
    n_cells < 50 ~ "VU",
    n_cells < 55 ~ "NT",
    TRUE         ~ "LC"
  )
}


# 4. worst case RLE status ------------------------------------------------
# CR > EN > VU > NT > LC
worst_rle_status <- function(...) {
  rank <- c(CR = 1, EN = 2, VU = 3, NT = 4, LC = 5)
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[!is.na(vals) & vals %in% names(rank)]
  if (length(vals) == 0) return(NA_character_)
  names(which.min(rank[vals]))
}



# 5. Criterion A assessment -----------------------------------------------
# Assess A1 (past 50 yrs), A2a (next 50 yrs) and A2b (any 50-yr window).
#   area_prediction_data : output of predict_area_change()
#   area_change_data     : output of calc_area_change() (for the near-term
#                          rate warning; optional)
#   headline_method      : "PRC" or "ARC" - method used for the overall status
#
# For bound rasters only the "opt" (central) estimate is assessed.
# Returns NULL when there are too few observations, otherwise a list with
# a1, a2a, a2b (data frames), status, obs_years, near_term_prc.
assess_criterion_a <- function(area_prediction_data, area_change_data = NULL,
                               headline_method = "PRC", window = 50) {

  results <- if ("Bound" %in% names(area_prediction_data)) {
    dplyr::filter(area_prediction_data, Bound == "opt")
  } else {
    area_prediction_data
  }

  needed <- c("year", "area", "type", "method", "segment", "scenario")
  if (is.null(results) || nrow(results) == 0 || !all(needed %in% names(results))) return(NULL)

  obs_years <- results$year[results$type == "observed"]
  if (length(obs_years) < 2) return(NULL)

  # Earliest past projection, plus the last row of every other series
  # (i.e. latest observation and furthest future projection)
  past.obsv.fut <- dplyr::bind_rows(
    results |>
      dplyr::group_by(type, method, segment, scenario) |>
      dplyr::filter(segment == "past") |>
      dplyr::slice(1),
    results |>
      dplyr::group_by(type, method, segment, scenario) |>
      dplyr::filter(segment != "past") |>
      dplyr::slice(dplyr::n())
  ) |>
    dplyr::ungroup() |>
    dplyr::arrange(year)

  latest_obs_area <- past.obsv.fut$area[past.obsv.fut$scenario == "observed"]
  latest_obs_year <- max(past.obsv.fut$year[past.obsv.fut$scenario == "observed"])

  # ---- A1: change from projected area 50 years ago to latest observation ----
  # Past projections use the median rate in both scenarios, so keep one.
  a1 <- past.obsv.fut |>
    dplyr::filter(segment == "past", method %in% c("ARC", "PRC"), scenario == "median") |>
    dplyr::group_by(method) |>
    dplyr::slice_min(year, n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      latest_observed_area = latest_obs_area,
      perc_change          = (latest_observed_area - area) / area,
      year_start           = year,
      year_end             = latest_obs_year
    ) |>
    dplyr::select(method, scenario, area, latest_observed_area, perc_change,
                  year_start, year_end) |>
    dplyr::mutate(status = rle_status_decline(perc_change))

  # ---- A2a: change from latest observation to projected area in 50 years ----
  a2a <- past.obsv.fut |>
    dplyr::filter(segment != "past", method %in% c("ARC", "PRC")) |>
    dplyr::group_by(method) |>
    dplyr::slice_max(year, n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      latest_observed_area = latest_obs_area,
      perc_change          = (area - latest_observed_area) / latest_observed_area,
      year_start           = latest_obs_year,
      year_end             = year
    ) |>
    dplyr::select(method, scenario, area, latest_observed_area, perc_change,
                  year_start, year_end) |>
    dplyr::mutate(status = rle_status_decline(perc_change))

  # ---- A2b: worst change over any 50-year window spanning past and future ---
  scenarios <- setdiff(unique(results$scenario), c("past", "observed"))
  a2b_all <- purrr::map_dfr(c("ARC", "PRC"), function(m) {
    purrr::map_dfr(scenarios, function(s) {
      df <- results |>
        dplyr::filter(method %in% c(m, "observed"),
                      scenario %in% c(s, "past", "observed")) |>
        dplyr::arrange(year, dplyr::desc(type == "observed")) |>
        dplyr::distinct(year, .keep_all = TRUE)

      start_years <- df |>
        dplyr::filter(segment %in% c("past", "observed")) |>
        dplyr::filter(!(year %in% c(min(year), max(year)))) |>
        dplyr::pull(year)

      purrr::map_dfr(start_years, function(y0) {
        y1 <- y0 + window
        a0 <- df$area[df$year == y0]
        a1_ <- df$area[df$year == y1]
        if (length(a0) == 0 || length(a1_) == 0) return(NULL)
        data.frame(year_start = y0, year_end = y1,
                   perc_change = (a1_ - a0) / a0)
      }) |>
        dplyr::mutate(method = m, scenario = s)
    })
  })

  a2b <- a2b_all |>
    dplyr::mutate(status = rle_status_decline(perc_change)) |>
    dplyr::group_by(method, scenario) |>
    dplyr::slice_min(perc_change, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(method, scenario, perc_change, status, year_start, year_end)

  # ---- Near-term rate (for the "unrealistic rate" warning) ------------------
  near_term_prc <- NA_real_
  if (!is.null(area_change_data)) {
    acd <- if ("Bound" %in% names(area_change_data)) {
      dplyr::filter(area_change_data, Bound == "opt")
    } else {
      area_change_data
    }
    acd <- dplyr::arrange(dplyr::ungroup(acd), year)
    near_term_prc <- abs(utils::tail(acd$proportional_rate_change, 1))
  }

  list(
    a1            = a1,
    a2a           = a2a,
    a2b           = a2b,
    status        = worst_rle_status(a1$status[a1$method   == headline_method],
                                     a2a$status[a2a$method == headline_method],
                                     a2b$status[a2b$method == headline_method]),
    obs_years     = obs_years,
    near_term_prc = near_term_prc
  )
}

# 6. Criterion B1 (EOO) Assessment ----------------------------------------
# Returns list(poly = SpatVector MCP, table = data.frame(year, area_km2, assessment))
assess_criterion_b1 <- function(eco_raster, year) {
  poly  <- makeEOO.SpatRaster(eco_raster, year)
  table <- as.data.frame(poly)[, c("year", "area_km2")]
  table$assessment <- rle_status_eoo(table$area_km2)
  list(poly = poly, table = table)
}


# 7. Criterion B2 (AOO) Assessment ----------------------------------------
#   method = "fast"  : rasterize onto the 10 km grid (what the app uses)
#   method = "exact" : area-weighted extraction with exactextractr (slower)
# Returns list(dat_all, dat_sel, table_df) or NULL if no ecosystem cells.
assess_criterion_b2 <- function(eco_raster, method = c("fast", "exact")) {
  method <- match.arg(method)
  if (method == "fast") compute_aoo_fast(eco_raster) else compute_aoo_slow(eco_raster)
}


# 8. Criterion B RLE Status -----------------------------------------------
# Criterion B status from the B1 and B2 tables (latest year of each).
criterion_b_status <- function(b1_table = NULL, b2_table = NULL) {
  latest <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(character())
    df$assessment[which.max(as.numeric(df$year))]
  }
  worst_rle_status(latest(b1_table), latest(b2_table))
}
