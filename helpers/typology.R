# helpers/typology.R

# -------------------------------------------------------------------------
# Compute Global Ecosystem Typology
compute_typology <- function(eau, efg_lookup, n_sample=100000) {
  if (is.null(eau )) stop("Ecosystem map not loaded.")
  
  get_files <- list.files("data/get_geotiff", pattern = "\\.tif$", full.names = TRUE)[c(1:26, 28:70, 72:109)]
  
  # if(crs(rast(get_files[1])) != crs(eau)){
  #   eau <- project(eau, crs(rast(get_files[1])), method="mode")
  # }
  # 
  # # Load typology rasters ----------------------------------------------------
  # get_data <- rast(get_files, win = ext(eau))
  # 
  # # if (length(get_data) == 0) return(data.frame())
  # # validate(need(!is.null(get_data), "Could not load typology rasters."))
  # 
  # # reample eau and mask to get the GET data
  # eau2 <- resample(eau, get_data[[1]], method = "average")
  # get_maps <- mask(get_data, eau2)
  # 
  # # count number of cells for each layer/value
  # cellCount <- freq(get_maps, usenames=T) %>% arrange(desc(count), desc(value))
  # 
  # # grand total of all counts (all layers, both values)
  # grand_total <- sum(cellCount$count, na.rm = TRUE)
  
  # 1. Sample points in native CRS first
  pts <- terra::spatSample(eau, n_sample, method = "regular",
                           as.points = TRUE, na.rm = TRUE)
  if (nrow(pts) == 0) return(data.frame())
  
  # 2. Project points to typology CRS (fast)
  typ_crs <- terra::crs(terra::rast(get_files[1]))
  if (!terra::same.crs(pts, typ_crs)) {
    pts <- terra::project(pts, typ_crs)
  }
  
  # 3. Load typology rasters cropped to projected points' extent
  get_data <- terra::rast(get_files, win = terra::ext(pts))
  
  # 4. Extract typology values at points (one column per layer)
  vals <- terra::extract(get_data, pts)[ , -1]
  
  # grand_total <- sum(!is.na(unlist(vals)))
  # 
  # # Per-layer counts of value=1 (major) and value=2 (minor)
  # cellCount <- purrr::imap_dfr(vals, function(col, lyr) {
  #   data.frame(
  #     layer       = lyr,
  #     count_major = sum(col == 1, na.rm = TRUE),
  #     count_minor = sum(col == 2, na.rm = TRUE),
  #     count_total = sum(!is.na(col))
  #   )
  # })
  # 
  # # Summarise proportion per EFG --------------------------------------------
  # typ_tbl <- cellCount |>
  #   dplyr::group_by(layer) |>
  #   dplyr::summarise(
  #     count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
  #     count_minor = sum(ifelse(value == 2, count, 0), na.rm = TRUE),
  #     count_total = sum(count, na.rm = TRUE),
  #     .groups = "drop"
  #   ) |>
  #   dplyr::mutate(
  #     proportion_major_global = count_major / grand_total,
  #     proportion_minor_global = count_minor / grand_total,
  #     EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract EFG_CODE = text up to the second dot
  #   ) |>
  #   dplyr::filter(if_any(c(proportion_major_global, proportion_minor_global), ~ coalesce(., 0) > 0.01)) |> # Keep rows where either proportion > 0.01 (treat NAs as 0)
  #   dplyr::arrange(desc(proportion_major_global)) |>
  #   dplyr::left_join(
  #     efg_lookup %>%
  #       dplyr::select(
  #         EFG_CODE = EFG_ID,
  #         `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
  #         Realm = REALM,
  #         Biome = BIOME
  #       ),
  #     by = "EFG_CODE"
  #   ) |>
  #   # dplyr::mutate(`% of Ecosystem (Major)` = scales::percent(proportion_major_global, accuracy = 0.1)) |>
  #   # dplyr::mutate(`% of Ecosystem (Minor)` = scales::percent(proportion_minor_global, accuracy = 0.1)) |>
  #   dplyr::mutate(`Major occurrence (%)` = round(proportion_major_global*100,2)) |>
  #   dplyr::mutate(`Minor occurrence (%)` = round(proportion_minor_global*100,2)) |>
  #   dplyr::select(Realm, Biome, `Ecosystem Functional Group`, EFG_CODE, `Major occurrence (%)`, `Minor occurrence (%)`)
  
  # Per-layer counts directly from vals (each column is one layer)
  counts_per_layer <- data.frame(
    layer       = names(vals),
    count_major = sapply(vals, function(col) sum(col == 1, na.rm = TRUE)),
    count_minor = sapply(vals, function(col) sum(col == 2, na.rm = TRUE)),
    count_total = sapply(vals, function(col) sum(!is.na(col)))
  )
  
  grand_total <- sum(counts_per_layer$count_total)
  
  typ_tbl <- counts_per_layer |>
    dplyr::mutate(
      proportion_major_global = count_major / grand_total,
      proportion_minor_global = count_minor / grand_total,
      EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+")
    ) |>
    dplyr::filter(if_any(c(proportion_major_global, proportion_minor_global),
                         ~ coalesce(., 0) > 0.01)) |>
    dplyr::arrange(desc(proportion_major_global)) |>
    dplyr::left_join(
      efg_lookup |>
        dplyr::select(
          EFG_CODE = EFG_ID,
          `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
          Realm = REALM,
          Biome = BIOME
        ),
      by = "EFG_CODE"
    ) |>
    dplyr::mutate(`Major occurrence (%)` = round(proportion_major_global * 100, 2),
                  `Minor occurrence (%)` = round(proportion_minor_global * 100, 2)) |>
    dplyr::select(Realm, Biome, `Ecosystem Functional Group`, EFG_CODE,
                  `Major occurrence (%)`, `Minor occurrence (%)`)
  
  typ_tbl
}

