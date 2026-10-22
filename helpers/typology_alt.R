# helpers/typology.R
# =============================================================================
# Global Ecosystem Typology (GET) overlap
# =============================================================================
# Speed-ups compared with the previous version:
#  1. Values are read once per unique GET cell, not once per sample point.
#     GET maps are much coarser than the ecosystem raster, so the sample points
#     collapse to a small number of GET cells; each cell's values are weighted
#     by how many points fall in it. Results are identical to extracting at
#     every point.
#  2. Fewer points by default (2e5). At GET resolution this is still far more
#     points than cells, so it adds nothing to accuracy to go higher.
#  3. Optional single multi-band file (data/get_stack.tif) instead of 107
#     separate GeoTIFFs: one file open instead of 107. Build it once with
#     build_get_stack() below; the function uses it automatically if present.
#
# Same arguments and same output table as before.
# =============================================================================

get_typology_rast <- function(get_dir = "data/get_geotiff",
                              stack_path = "data/get_stack.tif",
                              exclude = c(27, 71)) {
  if (file.exists(stack_path)) return(terra::rast(stack_path))
  files <- list.files(get_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(exclude)) files <- files[-exclude]
  terra::rast(files)
}

compute_typology <- function(eau, efg_lookup,
                             n_sample    = 2e5,
                             cache_path  = NULL,   # dir to save the point/cell cache
                             subset_vect = NULL) { # SpatVector mask for subsetting
  
  if (is.null(eau)) stop("Ecosystem map not loaded.")
  if (terra::nlyr(eau) > 1) eau <- eau[[terra::nlyr(eau)]]
  
  typ     <- get_typology_rast()
  typ_crs <- terra::crs(typ)
  
  cache_file <- if (!is.null(cache_path)) file.path(cache_path, "typ_cache.rds") else NULL
  
  # ----- Points -> GET cell numbers, and values for each unique cell ---------
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cache <- readRDS(cache_file)
  } else {
    pts <- terra::spatSample(eau, n_sample, method = "regular",
                             as.points = TRUE, na.rm = TRUE)
    if (nrow(pts) == 0) return(data.frame())
    if (!terra::same.crs(pts, typ_crs)) pts <- terra::project(pts, typ_crs)
    
    xy    <- terra::crds(pts)
    cells <- terra::cellFromXY(typ, xy)
    ok    <- !is.na(cells)
    xy    <- xy[ok, , drop = FALSE]
    cells <- cells[ok]
    
    ucell <- sort(unique(cells))
    vals  <- typ[ucell]               # one row per unique GET cell, one col per layer
    if (!is.data.frame(vals)) vals <- as.data.frame(vals)
    
    cache <- list(xy = xy, cells = cells, ucell = ucell, vals = vals)
    if (!is.null(cache_file)) saveRDS(cache, cache_file)
  }
  
  # ----- Apply subset if provided --------------------------------------------
  cells <- cache$cells
  if (!is.null(subset_vect)) {
    mask_proj <- if (!terra::same.crs(subset_vect, typ_crs))
      terra::project(subset_vect, typ_crs) else subset_vect
    pts_sv <- terra::vect(cache$xy, crs = typ_crs)
    inside <- as.logical(terra::relate(pts_sv, mask_proj, "intersects"))
    cells  <- cells[inside]
  }
  if (length(cells) == 0) return(data.frame())
  
  # ----- Weighted counts: points per unique cell ------------------------------
  w <- tabulate(match(cells, cache$ucell), nbins = length(cache$ucell))
  vals <- cache$vals
  
  counts_per_layer <- data.frame(
    layer       = names(vals),
    count_major = vapply(vals, function(col) sum(w[col %in% 1]), numeric(1)),
    count_minor = vapply(vals, function(col) sum(w[col %in% 2]), numeric(1)),
    count_total = vapply(vals, function(col) sum(w[!is.na(col)]), numeric(1))
  )
  grand_total <- sum(counts_per_layer$count_total)
  if (grand_total == 0) return(data.frame())
  
  # ----- Summarise proportions (unchanged) ------------------------------------
  counts_per_layer |>
    dplyr::mutate(
      proportion_major_global = count_major / grand_total,
      proportion_minor_global = count_minor / grand_total,
      EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+")
    ) |>
    dplyr::filter(dplyr::if_any(c(proportion_major_global, proportion_minor_global),
                                ~ dplyr::coalesce(., 0) > 0.01)) |>
    dplyr::arrange(dplyr::desc(proportion_major_global)) |>
    dplyr::left_join(
      efg_lookup |>
        dplyr::select(EFG_CODE = EFG_ID,
                      `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
                      Realm = REALM, Biome = BIOME),
      by = "EFG_CODE"
    ) |>
    dplyr::mutate(`Major occurrence (%)` = round(proportion_major_global * 100, 2),
                  `Minor occurrence (%)` = round(proportion_minor_global * 100, 2)) |>
    dplyr::select(Realm, Biome, `Ecosystem Functional Group`, EFG_CODE,
                  `Major occurrence (%)`, `Minor occurrence (%)`)
}

# -----------------------------------------------------------------------------
# One-off: combine the GET GeoTIFFs into a single compressed multi-band file.
# Run once locally, then commit data/get_stack.tif. Check the file size first:
# GitHub rejects files over 100 MB.
# -----------------------------------------------------------------------------
build_get_stack <- function(get_dir = "data/get_geotiff",
                            out = "data/get_stack.tif",
                            exclude = c(27, 71)) {
  files <- list.files(get_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(exclude)) files <- files[-exclude]
  r <- terra::rast(files)
  terra::writeRaster(r, out, datatype = "INT1U", overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES",
                              "BIGTIFF=IF_SAFER"))
  message(out, ": ", round(file.size(out) / 1e6, 1), " MB, ", terra::nlyr(r), " layers")
  invisible(out)
}

# # test function
# eau <- terra::rast("path/to/ecosystem.tif")
# system.time(tt <- compute_typology(eau, efg_lookup))
# tt