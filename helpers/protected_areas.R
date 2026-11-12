# helpers/protected_areas.R

# =============================================================================
# Protected area overlap
# =============================================================================
# Queries the live UNEP-WCMC WDPCA feature service for the raster's extent
# only (no local copy of the database), then computes:
#   - % of the ecosystem inside protected areas (overlaps counted once),
#     estimated from a regular grid of sample points
#   - breakdown by IUCN management category (strictest category wins
#     where sites overlap)
#   - polygons (EPSG:4326) for the leaflet map
#
# Requires: httr2, sf, terra
# =============================================================================

wdpca_layer_url <- "https://data-gis.unep-wcmc.org/server/rest/services/ProtectedPlanet/WDPCA/FeatureServer/1"

# IUCN categories in order of strictness; OECMs last
pa_cat_levels <- c("Ia", "Ib", "II", "III", "IV", "V", "VI",
                   "Not Reported", "Not Assigned", "Not Applicable", "OECM")

# -----------------------------------------------------------------------------
# Citation (fetched from the service so the month stays current)
# -----------------------------------------------------------------------------
wdpca_citation_fallback <- function() {
  paste0(
    "UNEP-WCMC and IUCN (", format(Sys.Date(), "%Y"), "), Protected Planet: ",
    "The World Database on Protected and Conserved Areas (WDPCA) [On-line], ",
    format(Sys.Date(), "%B %Y"), ", Cambridge, UK: UNEP-WCMC and IUCN. ",
    "Available at: https://www.protectedplanet.net/"
  )
}

get_wdpca_citation <- function() {
  tryCatch({
    meta <- httr2::request(wdpca_layer_url) |>
      httr2::req_url_query(f = "json") |>
      httr2::req_timeout(10) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    cit <- meta$copyrightText
    if (is.null(cit) || !nzchar(cit)) wdpca_citation_fallback() else cit
  }, error = function(e) wdpca_citation_fallback())
}

# -----------------------------------------------------------------------------
# Query the service for a bounding box (EPSG:4326)
# bb: named numeric with xmin, xmax, ymin, ymax
# Returns an sf object (EPSG:4326), or NULL if nothing in the extent.
# -----------------------------------------------------------------------------
fetch_wdpca <- function(bb, include_oecm = FALSE,
                        fields = c("site_id", "site_pid", "name_eng", "desig_eng",
                                   "iucn_cat", "status", "status_yr",
                                   "site_type", "gov_type"),
                        max_features = 20000, page_size = 2000,
                        simplify_deg = 0.0005) {
  
  where <- if (include_oecm) {
    "status <> 'Proposed'"
  } else {
    "status <> 'Proposed' AND site_type = 'PA'"
  }
  
  base <- httr2::request(paste0(wdpca_layer_url, "/query")) |>
    httr2::req_url_query(
      where        = where,
      geometry     = paste(bb[c("xmin", "ymin", "xmax", "ymax")], collapse = ","),
      geometryType = "esriGeometryEnvelope",
      inSR         = 4326,
      outSR        = 4326,
      spatialRel   = "esriSpatialRelIntersects"
    ) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3)
  
  # 1. Count first: separates "no sites here" from "request failed"
  cnt <- base |>
    httr2::req_url_query(returnCountOnly = "true", f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  if (!is.null(cnt$error)) stop("WDPCA service error: ", cnt$error$message)
  n <- cnt$count
  if (is.null(n) || n == 0) return(NULL)
  if (n > max_features) {
    stop(sprintf("%s protected areas in extent (limit %s); extent too large",
                 format(n, big.mark = ","), format(max_features, big.mark = ",")))
  }
  
  # 2. Page through the results
  pages <- lapply(seq(0, n - 1, by = page_size), function(offset) {
    txt <- base |>
      httr2::req_url_query(
        outFields         = paste(fields, collapse = ","),
        returnGeometry    = "true",
        geometryPrecision = 5,           # ~1 m; keeps payload small
        maxAllowableOffset = simplify_deg,  # server-side generalisation (~50 m)
        orderByFields     = "objectid",  # stable paging
        resultOffset      = offset,
        resultRecordCount = page_size,
        f                 = "geojson"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_string()
    sf::read_sf(txt, quiet = TRUE)
  })
  
  pa <- do.call(rbind, pages)
  pa <- pa[!sf::st_is_empty(pa), ]
  suppressWarnings(sf::st_make_valid(pa))
}

# -----------------------------------------------------------------------------
# Main function (point-based)
# -----------------------------------------------------------------------------
# The ecosystem raster is never polygonised or rasterised against the PAs.
# Instead a regular grid of points is sampled across the raster extent; each
# point carries the area it represents (latitude-weighted for lon/lat rasters).
# Ecosystem points are then tested against the PA polygons (point-in-polygon),
# and every statistic is a sum of point areas.
#
# x:        SpatRaster of the ecosystem (non-NA cells = ecosystem). If several
#           layers, the last is used.
# pa:       optional sf of sites already fetched (e.g. the cached full-extent
#           result); if supplied, no web request is made.
# n_points: number of sample points across the extent. 2e5 gives a standard
#           error well under 0.5 percentage points for most ecosystems.
# -----------------------------------------------------------------------------
compute_protected <- function(x, include_oecm = FALSE, pa = NULL,
                              n_points = 2e5, n_top = 10) {
  if (is.null(x)) stop("Raster not loaded")
  if (!inherits(x, "SpatRaster")) stop("x must be a SpatRaster")
  if (terra::nlyr(x) > 1) x <- x[[terra::nlyr(x)]]
  if (!terra::is.lonlat(x)) x <- terra::project(x, "EPSG:4326", method = "near")
  
  # 1. Extent in lon/lat for the web query
  e  <- terra::project(terra::ext(x), from = terra::crs(x), to = "EPSG:4326")
  bb <- c(xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax)
  bb <- setNames(as.numeric(bb), c("xmin", "xmax", "ymin", "ymax"))
  
  # 2. Regular sample of points across the whole extent (NA cells kept, so
  #    each point represents an equal share of the extent's grid)
  pts <- terra::spatSample(x, n_points, method = "regular",
                           as.points = TRUE, na.rm = FALSE)
  is_eco <- !is.na(terra::values(pts)[, 1])
  if (!any(is_eco)) stop("No ecosystem pixels found")
  
  # Area each point represents (kmB2): extent area shared across points,
  # weighted by cos(latitude) when the raster is in lon/lat
  ext_km2 <- terra::expanse(terra::as.polygons(terra::ext(x), crs = terra::crs(x)),
                            unit = "km")
  w <- if (terra::is.lonlat(x)) cos(terra::crds(pts)[, 2] * pi / 180) else rep(1, nrow(pts))
  pt_km2 <- ext_km2 * w / sum(w)
  
  pts    <- pts[is_eco]
  pt_km2 <- pt_km2[is_eco]
  eco_km2 <- sum(pt_km2)
  
  # total area output now not based on the sample but exact area
  eco_exact <- terra::expanse(x, unit = "km")$area
  pt_km2    <- pt_km2 * eco_exact / eco_km2
  eco_km2   <- eco_exact
  
  # 3. Fetch sites (unless supplied)
  if (is.null(pa)) pa <- fetch_wdpca(bb, include_oecm = include_oecm)
  citation <- get_wdpca_citation()
  
  empty <- list(
    summary = data.frame(eco_km2 = eco_km2, prot_km2 = 0, pct_protected = 0,
                         pct_strict = 0),
    by_category = NULL, by_site = NULL, pa = NULL, pa_all = NULL,
    n_points = length(pt_km2), citation = citation
  )
  if (is.null(pa) || nrow(pa) == 0) return(empty)
  
  # 4. Category rank per site (strictest wins where sites overlap)
  cat <- ifelse(pa$site_type == "OECM", "OECM", pa$iucn_cat)
  cat[is.na(cat) | !cat %in% pa_cat_levels] <- "Not Reported"
  pa$category <- factor(cat, levels = pa_cat_levels)
  pa$cat_rank <- as.integer(pa$category)
  
  # 5. Point-in-polygon: points go to the PA CRS (4326), not the reverse
  pts_ll <- terra::project(pts, "EPSG:4326")
  pa_v   <- terra::vect(pa)
  hits   <- terra::relate(pts_ll, pa_v, "intersects", pairs = TRUE)  # cols: point, site
  if (nrow(hits) == 0) return(modifyList(empty, list(pa_all = pa)))
  hits <- data.frame(pt = hits[, 1], site = hits[, 2])
  hits$rank <- pa$cat_rank[hits$site]
  hits$km2  <- pt_km2[hits$pt]
  
  # Protected total and categories: one entry per point (strictest category)
  per_pt <- aggregate(rank ~ pt, data = hits, FUN = min)
  per_pt$km2 <- pt_km2[per_pt$pt]
  prot_km2 <- sum(per_pt$km2)
  
  by_cat <- aggregate(km2 ~ rank, data = per_pt, FUN = sum)
  by_cat$Category <- factor(pa_cat_levels[by_cat$rank], levels = pa_cat_levels)
  by_cat$area_km2 <- by_cat$km2
  by_cat$pct_eco  <- 100 * by_cat$area_km2 / eco_km2
  by_cat <- by_cat[order(by_cat$rank), c("Category", "area_km2", "pct_eco")]
  
  strict_km2 <- sum(by_cat$area_km2[by_cat$Category %in% c("Ia", "Ib", "II")])
  
  # 6. Ecosystem area per site (sites can overlap, so these do not sum to the
  #    protected total)
  site_km2 <- tapply(hits$km2, hits$site, sum)
  pa$eco_km2 <- 0
  pa$eco_km2[as.integer(names(site_km2))] <- as.numeric(site_km2)
  
  sites <- sf::st_drop_geometry(pa)
  sites$name_eng[is.na(sites$name_eng)]   <- "Unnamed site"
  sites$desig_eng[is.na(sites$desig_eng)] <- "Not reported"
  sites <- aggregate(eco_km2 ~ site_id + name_eng + desig_eng + category,
                     data = sites, FUN = sum)   # merge parcels of one site
  sites <- sites[sites$eco_km2 > 0, ]
  sites$pct_eco <- 100 * sites$eco_km2 / eco_km2
  sites <- sites[order(-sites$eco_km2), ]
  
  by_site <- head(sites, n_top)
  by_site <- data.frame(
    by_site$name_eng,
    by_site$desig_eng,
    as.character(by_site$category),
    format(round(by_site$eco_km2, 0), big.mark = ","),
    format(round(by_site$pct_eco, 1))
  )
  # Plain-ASCII column names: special characters (e.g. superscript 2) in
  # names break DT on some Windows locales
  names(by_site) <- c("Site", "Designation", "IUCN", "Area (km2)", "% of Ecosystem")
  
  list(
    summary = data.frame(
      eco_km2        = eco_km2,
      prot_km2       = prot_km2,
      pct_protected  = 100 * prot_km2 / eco_km2,
      pct_strict     = 100 * strict_km2 / eco_km2
    ),
    by_category = by_cat,
    by_site     = by_site,
    pa          = pa[pa$eco_km2 > 0, c("site_id", "name_eng", "desig_eng",
                                       "category", "status", "status_yr",
                                       "site_type", "eco_km2")],
    pa_all      = pa,          # everything in the extent; reused for subsets
    n_points    = length(pt_km2),
    citation    = citation
  )
}

# # test function
# eau <- terra::rast("path/to/ecosystem.tif")
# res <- compute_protected(eau)
# res$summary; res$by_category; res$by_site
# plot(sf::st_geometry(res$pa))

# # test function
# eau <- terra::rast("path/to/ecosystem.tif")
# res <- compute_protected(eau)
# res$summary; res$by_category; res$by_site
# plot(sf::st_geometry(res$pa))
