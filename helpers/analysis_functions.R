# helpers/analysis_functions.R
# =============================================================================
# Core EcoCAT analysis functions: raster loading, area, Criterion A rates and
# projections, EOO and AOO geometry.
#
# Rules for this file (it is copied verbatim into the exported R script):
#   * plain functions only - no Shiny, no reactives, no top-level side effects
#   * call packages with pkg:: so behaviour doesn't depend on library() order
#     (e.g. stats::lag vs dplyr::lag silently give different answers)
# =============================================================================


# -----------------------------------------------------------------------------
# Raster loading and area
# -----------------------------------------------------------------------------

# Area (m2) of the non-NA cells of a single-layer ecosystem raster.
raster_area_m2 <- function(r) {
  if (!is.na(terra::crs(r)) && !terra::is.lonlat(r)) {
    terra::global(r, "sum", na.rm = TRUE)[1, 1] * prod(terra::res(r))
  } else {
    terra::expanse(r, unit = "m")[, 2]
  }
}

# Treat 0 as "not ecosystem" when the raster has no NA cells at all.
zero_to_na <- function(r) {
  if (terra::global(r, "isNA")[1, 1] == 0) {
    r <- terra::classify(r, matrix(c(0, NA), ncol = 2))
  }
  r
}

# Pick the ecosystem layer / class from one raster file (same rules as the
# upload module).
#   selected_layer = NULL        -> first layer
#   selected_layer = "name"      -> that layer
#   selected_layer = "3" / 3     -> cells of class 3 in a single-layer class map
#   selected_layer = "Thicket"   -> cells with that category label
select_ecosystem_layer <- function(r, selected_layer = NULL) {
  if (is.null(selected_layer) || !nzchar(selected_layer)) return(r[[1]])

  if (selected_layer %in% names(r)) return(r[[selected_layer]])

  if (terra::nlyr(r) > 1) return(r[[1]])        # multi-layer, no matching name

  r1    <- r[[1]]
  uvals <- terra::unique(r1)[, 1]
  uvals <- uvals[!is.na(uvals)]
  if (length(uvals) == 1) return(r1)            # already a single-class map

  sel_num <- suppressWarnings(as.numeric(selected_layer))
  if (!is.na(sel_num)) return(terra::ifel(r1 == sel_num, 1, NA))

  lev <- terra::levels(r1)
  if (!is.null(lev) && length(lev) >= 1 && is.data.frame(lev[[1]]) && nrow(lev[[1]]) > 0) {
    lev_tbl <- lev[[1]]
    lbl_col <- intersect(c("label", "levels", "Labels", "Level"), names(lev_tbl))
    val_col <- intersect(c("value", "ID", "Values", "Class"), names(lev_tbl))
    if (length(lbl_col) >= 1 && length(val_col) >= 1) {
      i <- match(selected_layer, lev_tbl[[lbl_col[1]]])
      if (!is.na(i)) return(terra::ifel(r1 == lev_tbl[[val_col[1]]][i], 1, NA))
    }
  }

  terra::ifel(r1 == selected_layer, 1, NA)
}

# Crop and mask a raster to a polygon (as the app's "refine area" does).
# Returns the raster unchanged if crop_to is NULL or doesn't overlap.
crop_to_area <- function(r, crop_to = NULL) {
  if (is.null(crop_to)) return(r)
  v <- terra::project(crop_to, terra::crs(r))
  if (!terra::relate(terra::ext(r), terra::ext(v), "intersects")) {
    warning("The subset area doesn't overlap the raster - using the full extent.")
    return(r)
  }
  r <- terra::crop(r, v)
  r <- terra::mask(r, terra::rasterize(v, r, field = 1, background = NA))
  terra::trim(r)
}

# Load one raster per year, select the ecosystem, and calculate area.
# Bound rasters (layers named min / opt / max) get one area row per bound and
# the "opt" layer is used as the ecosystem map for Criterion B.
#
# crop_to: optional SpatVector - crop and mask to this area first (the app's
# "refine area" polygon).
#
# Returns list(rasters = list of SpatRaster (chronological),
#              areas   = data.frame, is_bound = logical, years = sorted years)
load_ecosystem_rasters <- function(raster_files, years, selected_layer = NULL,
                                   crop_to = NULL) {
  if (length(raster_files) != length(years)) {
    stop("Got ", length(raster_files), " raster file(s) but ", length(years),
         " year(s) - each raster needs exactly one year.", call. = FALSE)
  }
  missing_files <- raster_files[!file.exists(raster_files)]
  if (length(missing_files) > 0) {
    stop("Raster file(s) not found:\n  ", paste(missing_files, collapse = "\n  "),
         call. = FALSE)
  }
  raster_files <- normalizePath(raster_files, winslash = "/")

  ord          <- order(as.numeric(years))
  raster_files <- raster_files[ord]
  years        <- years[ord]

  is_bound <- identical(sort(names(terra::rast(raster_files[1]))),
                        c("max", "min", "opt"))

  if (is_bound) {
    areas <- do.call(rbind, lapply(seq_along(raster_files), function(i) {
      r_all <- crop_to_area(terra::rast(raster_files[i]), crop_to)
      do.call(rbind, lapply(c("min", "opt", "max"), function(b) {
        area_m2 <- raster_area_m2(zero_to_na(r_all[[b]]))
        data.frame(Year = years[i], Bound = b,
                   Area_ha = area_m2 / 1e4, Area_km2 = area_m2 / 1e6)
      }))
    }))
    rasters <- lapply(seq_along(raster_files), function(i) {
      r <- zero_to_na(crop_to_area(terra::rast(raster_files[i]), crop_to)[["opt"]])
      names(r) <- years[i]
      r
    })
  } else {
    rasters <- lapply(seq_along(raster_files), function(i) {
      r_all <- crop_to_area(terra::rast(raster_files[i]), crop_to)
      r     <- zero_to_na(select_ecosystem_layer(r_all, selected_layer))
      names(r) <- years[i]
      r
    })
    areas <- do.call(rbind, lapply(seq_along(rasters), function(i) {
      area_m2 <- raster_area_m2(rasters[[i]])
      data.frame(Year = years[i], Area_ha = area_m2 / 1e4, Area_km2 = area_m2 / 1e6)
    }))
  }

  list(rasters = rasters, areas = areas, is_bound = is_bound, years = years)
}


# -----------------------------------------------------------------------------
# Criterion A: observed change and projections
# -----------------------------------------------------------------------------

# Observed area change between consecutive years.
# Accepts area data with columns Year, Area_km2 (and optionally Bound).
calc_area_change <- function(area.data) {

  add_rates <- function(d) {
    dplyr::mutate(
      d,
      dt                       = year - dplyr::lag(year),
      area_change              = area - dplyr::lag(area),
      proportional_rate_change = ((area / dplyr::lag(area))^(1 / dt) - 1) * 100,
      absolute_rate_change     = area_change / dt
    )
  }

  if ("Bound" %in% names(area.data)) {
    area.data |>
      dplyr::select(Bound, Year, Area_km2) |>
      dplyr::mutate(year = round(as.numeric(Year), 0), area = Area_km2,
                    Year = NULL, Area_km2 = NULL) |>
      dplyr::group_by(Bound) |>
      dplyr::arrange(year, .by_group = TRUE) |>
      add_rates()
  } else {
    area.data |>
      dplyr::select(Year, Area_km2) |>
      dplyr::mutate(year = round(as.numeric(Year), 0), area = Area_km2,
                    Year = NULL, Area_km2 = NULL) |>
      dplyr::arrange(year) |>
      add_rates()
  }
}


# Project area backwards and forwards from one set of observations (no Bound).
#   horizon  : years to project (RLE uses 50)
#   n_recent : number of most recent periods averaged for the near-term rate
# Past projections use the median rate; future uses either the near-term
# ("recent") or the median rate.
predict_area_change_single <- function(obsv_change, horizon = 50, n_recent = 1) {

  obsv_change <- dplyr::ungroup(obsv_change) |> dplyr::arrange(year)
  if (nrow(obsv_change) < 2) return(data.frame())

  oldest <- dplyr::slice(obsv_change, 1)
  recent <- dplyr::slice(obsv_change, dplyr::n())

  nYearsPast <- max(0, horizon - (max(obsv_change$year) - min(obsv_change$year)))

  project_seq <- function(start_area, rate, type, years,
                          direction = c("future", "past")) {
    direction <- match.arg(direction)
    data.frame(i = 0:years) |>
      dplyr::mutate(
        area = dplyr::case_when(
          type == "PRC" & direction == "future" ~ start_area * (1 + rate / 100)^i,
          type == "PRC" & direction == "past"   ~ start_area * (1 + rate / 100)^(-i),
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

  med_PRC <- stats::median(obsv_change$proportional_rate_change, na.rm = TRUE)
  med_ARC <- stats::median(obsv_change$absolute_rate_change,     na.rm = TRUE)
  recent_PRC <- mean(utils::tail(stats::na.omit(obsv_change$proportional_rate_change), n_recent), na.rm = TRUE)
  recent_ARC <- mean(utils::tail(stats::na.omit(obsv_change$absolute_rate_change),     n_recent), na.rm = TRUE)

  proj <- purrr::imap_dfr(
    list(recent = NULL, median = NULL),
    function(., scen) {
      fut_PRC <- if (scen == "recent") recent_PRC else med_PRC
      fut_ARC <- if (scen == "recent") recent_ARC else med_ARC

      dplyr::bind_rows(
        project_seq(oldest$area, med_PRC, "PRC", nYearsPast, "past")   |> dplyr::mutate(segment = "past"),
        project_seq(oldest$area, med_ARC, "ARC", nYearsPast, "past")   |> dplyr::mutate(segment = "past"),
        project_seq(recent$area, fut_PRC, "PRC", horizon,    "future") |> dplyr::mutate(segment = "future"),
        project_seq(recent$area, fut_ARC, "ARC", horizon,    "future") |> dplyr::mutate(segment = "future")
      ) |>
        dplyr::mutate(scenario = scen, type = "projected") |>
        dplyr::select(year, area, type, method, scenario, segment) |>
        dplyr::arrange(method, year)
    }
  )

  observed <- obsv_change |>
    dplyr::select(year, area) |>
    dplyr::mutate(type = "observed", method = "observed",
                  scenario = "observed", segment = "observed")

  dplyr::bind_rows(observed, proj)
}


# Projections for standard or bound (min/opt/max) area-change data.
predict_area_change <- function(obsv_change, horizon = 50, n_recent = 1) {
  if ("Bound" %in% names(obsv_change)) {
    purrr::map_dfr(unique(dplyr::pull(obsv_change, Bound)), function(b) {
      obsv_change |>
        dplyr::filter(Bound == b) |>
        dplyr::ungroup() |>
        predict_area_change_single(horizon, n_recent) |>
        dplyr::mutate(Bound = b)
    })
  } else {
    predict_area_change_single(dplyr::ungroup(obsv_change), horizon, n_recent)
  }
}


# -----------------------------------------------------------------------------
# Criterion B1: Extent of Occurrence (minimum convex polygon)
# -----------------------------------------------------------------------------
makeEOO.SpatRaster <- function(input.data, year) {
  EOO.poly <- terra::as.polygons(input.data, na.rm = TRUE) |>
    terra::hull(type = "convex")
  EOO.poly$year     <- year
  EOO.poly$area_km2 <- terra::expanse(EOO.poly, unit = "km")
  EOO.poly
}


# -----------------------------------------------------------------------------
# Criterion B2: Area of Occupancy (10 x 10 km grid, 1% rule)
# -----------------------------------------------------------------------------

# Fast version: rasterize ecosystem cells onto a 10 km grid in a data-centred
# Lambert azimuthal equal-area projection.
compute_aoo_fast <- function(eau) {

  # ensure 1 and NA raster
  eau <- terra::ifel(eau > 0, 1, NA)
  
   res_xy <- terra::res(eau)
  # cell_area_km2 <- if (terra::is.lonlat(eau)) {
  #   ctr <- mean(c(terra::ext(eau)$ymin, terra::ext(eau)$ymax))
  #   prod(res_xy) * 111.32^2 * cos(ctr * pi / 180)
  # } else {
  #   prod(res_xy) / 1e6
  # }
  cell_area_km2 <- if (terra::is.lonlat(eau)) {
    prod(res(terra::mask(terra::cellSize(eau, unit = "km"), eau)))/1e6
  } else {
    prod(terra::res(eau)) / 1e6      # presence (1/NA) x pixel area
  }
  
  # Centre: midpoint of the geographic (WGS84) bounding box
  # same as slow version 
  ext_ll <- if (terra::is.lonlat(eau)) terra::ext(eau) else
    terra::ext(terra::project(
      terra::as.polygons(terra::ext(eau), crs = terra::crs(eau)), "EPSG:4326"))
  laea <- sprintf(
    "+proj=laea +lat_0=%.4f +lon_0=%.4f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    mean(c(ext_ll$ymin, ext_ll$ymax)), mean(c(ext_ll$xmin, ext_ll$xmax)))
  
  # # Data extent in LAEA, snapped outwards to the grid (plus one cell of padding)
  # pe      <- as.vector(terra::ext(terra::project(eau, crs)))
  # snap_lo <- function(v, o) o + (floor((v - o) / res) - 1) * res
  # snap_hi <- function(v, o) o + (ceiling((v - o) / res) + 1) * res

  pts <- terra::as.points(eau, na.rm = TRUE)
  if (nrow(pts) == 0) return(NULL)
  pts_laea <- terra::project(pts, laea)

  # grid   <- terra::rast(terra::ext(pts_laea), res = 10000, crs = laea)
  # 10 km grid snapped to whole multiples of 10 km from the projection centre,
  # padded by one cell so no edge points fall outside it
  pe   <- terra::ext(pts_laea)
  grid <- terra::rast(
    xmin = (floor(pe$xmin / 1e4) - 1) * 1e4, xmax = (ceiling(pe$xmax / 1e4) + 1) * 1e4,
    ymin = (floor(pe$ymin / 1e4) - 1) * 1e4, ymax = (ceiling(pe$ymax / 1e4) + 1) * 1e4,
    resolution = 1e4, crs = laea)
  counts <- terra::rasterize(pts_laea, grid, fun = "sum")

  # 1% rule: drop the smallest-occupancy cells that together hold <= 1% of area
  vals      <- terra::values(counts, na.rm = TRUE)
  areas     <- sort(vals * cell_area_km2)
  cum_props <- cumsum(areas) / sum(areas)
  thresh_n  <- if (any(cum_props > 0.01))
    areas[which(cum_props > 0.01)[1]] / cell_area_km2 else 0

  # aggregate = FLASE to ensure counts are on distinct count values not grid cells
  polys_all <- terra::as.polygons(counts, aggregate = FALSE) |>
    sf::st_as_sf() |> sf::st_transform(4326) |> dplyr::mutate(type = "all")
  polys_sel <- terra::as.polygons(terra::ifel(counts >= thresh_n, counts, NA), aggregate = FALSE) |>
    sf::st_as_sf() |> sf::st_transform(4326) |> dplyr::mutate(type = "assessment")

  list(
    dat_all  = polys_all,
    dat_sel  = polys_sel,
    table_df = data.frame(
      year       = names(eau)[1],
      n          = nrow(polys_all),
      n_exceed   = nrow(polys_sel),
      assessment = rle_status_aoo(nrow(polys_sel)),
      stringsAsFactors = FALSE
    )
  )
}

# Exact version: area-weighted extraction with exactextractr.
compute_aoo_slow <- function(eau) {

  ext_4326 <- terra::ext(terra::project(eau, "EPSG:4326"))
  grid.prj <- sprintf(
    "+proj=laea +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    (ext_4326$ymin + ext_4326$ymax) / 2, (ext_4326$xmin + ext_4326$xmax) / 2)

  eau.wcea <- terra::project(eau, grid.prj)

  grid <- terra::rast(terra::ext(eau.wcea), res = 10000, crs = terra::crs(eau.wcea))
  grid <- terra::extend(grid, c(2, 2))
  grid[] <- 0

  grid_sf <- sf::st_as_sf(terra::as.polygons(grid, aggregate = FALSE))
  grid_sf$cell_id <- seq_len(nrow(grid_sf))

  cell_area <- exactextractr::exact_extract(eau.wcea, grid_sf, "sum",
                                            coverage_area = TRUE, force_df = TRUE,
                                            progress = FALSE)
  names(cell_area) <- names(eau.wcea)

  cumulative <- cbind(cell_id = grid_sf$cell_id, cell_area) |>
    tidyr::pivot_longer(cols = -cell_id, names_to = "year", values_to = "area") |>
    dplyr::group_by(year) |>
    dplyr::arrange(area, .by_group = TRUE) |>
    dplyr::mutate(cum_prop = cumsum(area) / sum(area, na.rm = TRUE)) |>
    dplyr::ungroup()

  table_df <- cumulative |>
    dplyr::group_by(year) |>
    dplyr::summarise(n        = sum(area > 0, na.rm = TRUE),
                     n_exceed = sum(cum_prop > 0.01, na.rm = TRUE)) |>
    as.data.frame()
  table_df$assessment <- rle_status_aoo(table_df$n_exceed)

  grid_4326 <- sf::st_transform(grid_sf, 4326)
  all_ids   <- cumulative$cell_id[cumulative$cum_prop > 0]
  sel_ids   <- cumulative$cell_id[cumulative$cum_prop > 0.01]

  dat_all <- grid_4326[grid_4326$cell_id %in% all_ids, ]
  dat_sel <- grid_4326[grid_4326$cell_id %in% sel_ids, ]
  dat_all$type <- "all"
  dat_sel$type <- "assessment"

  list(dat_all = dat_all, dat_sel = dat_sel, table_df = table_df)
}

# compute_aoo_fast(
# rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/Itigi Sumbu Thicket/Classifications/Sumbu/Classifications/classification_temporal_filtered_sumbu_2024_2025.tif")
# )

