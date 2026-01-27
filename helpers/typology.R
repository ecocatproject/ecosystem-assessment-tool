# helpers/typology.R

# -------------------------------------------------------------------------
# Compute Global Ecosystem Typology
compute_typology <- function(eau, efg_lookup) {
  validate(need(!is.null(eau), "Ecosystem map not loaded."))
  
  # Load typology rasters ----------------------------------------------------
  get_data <- rast(list.files("data/get_geotiff",
                              pattern = "\\.tif$", full.names = TRUE)[c(1:26, 28:70, 72:109)],
                   win = ext(eau))
  
  # if (length(get_data) == 0) return(data.frame())
  validate(need(!is.null(get_data), "Could not load typology rasters."))
  
  # reample eau and mask to get the GET data
  eau2 <- resample(eau, get_data[[1]], method = "average")
  get_maps <- mask(get_data, eau2)
  
  # count number of cells for each layer/value
  cellCount <- freq(get_maps, usenames=T) %>% arrange(desc(count), desc(value))
  
  # grand total of all counts (all layers, both values)
  grand_total <- sum(cellCount$count, na.rm = TRUE)
  
  # Summarise proportion per EFG --------------------------------------------
  typ_tbl <- cellCount |>
    dplyr::group_by(layer) |>
    dplyr::summarise(
      count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
      count_minor = sum(ifelse(value == 2, count, 0), na.rm = TRUE),
      count_total = sum(count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      proportion_major_global = count_major / grand_total,
      proportion_minor_global = count_minor / grand_total,
      EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract EFG_CODE = text up to the second dot
    ) |>
    dplyr::filter(if_any(c(proportion_major_global, proportion_minor_global), ~ coalesce(., 0) > 0.01)) |> # Keep rows where either proportion > 0.01 (treat NAs as 0)
    dplyr::arrange(desc(proportion_major_global)) |>
    dplyr::left_join(
      efg_lookup %>%
        dplyr::select(
          EFG_CODE = EFG_ID,
          `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
          Realm = REALM,
          Biome = BIOME
        ),
      by = "EFG_CODE"
    ) |>
    dplyr::mutate(`major % of Ecosystem` = scales::percent(proportion_major_global, accuracy = 0.1)) |>
    dplyr::mutate(`minor % of Ecosystem` = scales::percent(proportion_minor_global, accuracy = 0.1)) |>
    dplyr::select(EFG_CODE, `Ecosystem Functional Group`, Realm, Biome, `major % of Ecosystem`, `minor % of Ecosystem`)
  
  typ_tbl
}

