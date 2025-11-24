# helpers/typology.R

# library(terra)
# eau <- rast("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiThicketRF3_L8_15_17_mosaic_240924.tif")
# roi <- vect("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/Tarciso Leao's files - EcoCAT team folder/R_scripts/ItigiSumbuThicket/ItigiSumbuThicketEcoregion20kmBuffer.shp")
# efg_lookup <- read.csv("C:/Users/rcr11kg/OneDrive - The Royal Botanic Gardens, Kew/EcoCat/Code/R/modularised/data/ecosystem_typology_lookup_table.csv")

# -------------------------------------------------------------------------
# Compute Global Ecosystem Typology
compute_typology <- function(eau, efg_lookup) {
  validate(need(!is.null(eau), "ROI not loaded."))
  
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
      # proportion_within_layer = dplyr::if_else(count_total > 0,
      #                                          count_major / count_total, NA_real_),
      EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract EFG_CODE = text up to the second dot
    ) |>
    dplyr::arrange(desc(proportion_major_global)) |>
    dplyr::filter(proportion_major_global >= 0.01) |>
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
    dplyr::mutate(`% of EAU` = scales::percent(proportion_major_global, accuracy = 0.1)) |>
    dplyr::select(EFG_CODE, `Ecosystem Functional Group`, Realm, Biome, `% of EAU`)
  
  typ_tbl
}

# orignal based on the roi 
# compute_typology <- function(roi, efg_lookup) {
#   validate(need(!is.null(roi), "ROI not loaded."))
#   
#   # Load typology rasters ----------------------------------------------------
#   get_data <- rast(list.files("data/get_geotiff",
#                               pattern = "\\.tif$", full.names = TRUE)[c(1:26, 28:70, 72:109)],
#                    win = ext(roi))
#   # if (length(get_data) == 0) return(data.frame())
#   validate(need(!is.null(get_data), "Could not load typology rasters."))
#   
#   # Crop + mask to ROI
#   r_roi <- terra::crop(get_data, roi) |> terra::mask(roi)
#   
#   # count number of cells for each layer/value
#   cellCount <- freq(r_roi, usenames=T) %>% arrange(desc(count), desc(value))
#   
#   # grand total of all counts (all layers, both values)
#   grand_total <- sum(cellCount$count, na.rm = TRUE)
#   
#   # Summarise proportion per EFG --------------------------------------------
#   typ_tbl <- cellCount |>
#     dplyr::group_by(layer) |>
#     dplyr::summarise(
#       count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
#       count_total = sum(count, na.rm = TRUE),
#       .groups = "drop"
#     ) |>
#     dplyr::mutate(
#       proportion_global = count_major / grand_total,
#       proportion_within_layer = dplyr::if_else(count_total > 0,
#                                                count_major / count_total, NA_real_),
#       EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract EFG_CODE = text up to the second dot
#     ) |>
#     dplyr::arrange(desc(proportion_global)) |>
#     dplyr::filter(proportion_global >= 0.05) |>
#     dplyr::left_join(
#       efg_lookup %>%
#         dplyr::select(
#           EFG_CODE = EFG_ID,
#           `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
#           Realm = REALM,
#           Biome = BIOME
#         ),
#       by = "EFG_CODE"
#     ) |>
#     dplyr::mutate(`% of ROI` = scales::percent(proportion_global, accuracy = 0.1)) |>
#     dplyr::select(EFG_CODE, `Ecosystem Functional Group`, Realm, Biome, `% of ROI`)
#   
#   typ_tbl
# }




# ##
# # -------------------------------------------------------------------------
# # Compute Global Ecosystem Typology
# #   - Based on the most recent uploaded EAU raster instead of ROI polygon
# # -------------------------------------------------------------------------
# compute_typology <- function(eau_rasters, efg_lookup) {
#   validate(need(!is.null(eau_rasters) && length(eau_rasters) > 0,
#                 "No EAU rasters available."))
# 
#   # Use the most recent EAU raster (assuming names are years or sorted chronologically)
#   latest_raster <- eau_rasters[[length(eau_rasters)]]
#   validate(need(!is.null(latest_raster), "Latest EAU raster not found."))
# 
#   # Load typology rasters ----------------------------------------------------
#   typology_files <- list.files(
#     "data/get_geotiff",
#     pattern = "\\.tif$",
#     full.names = TRUE
#   )[c(1:26, 28:70, 72:109)]
# 
#   get_data <- rast(typology_files, win = ext(latest_raster))
#   validate(need(!is.null(get_data), "Could not load typology rasters."))
# 
#   # Crop + mask to the extent of the latest EAU raster
#   r_masked <- terra::crop(get_data, latest_raster) |>
#     terra::mask(latest_raster)
# 
#   # Count number of cells for each layer/value
#   cellCount <- freq(r_masked, usenames = TRUE) |>
#     dplyr::arrange(desc(count), desc(value))
# 
#   # Grand total of all counts
#   grand_total <- sum(cellCount$count, na.rm = TRUE)
# 
#   # Summarise proportion per EFG --------------------------------------------
#   typ_tbl <- cellCount |>
#     dplyr::group_by(layer) |>
#     dplyr::summarise(
#       count_major = sum(ifelse(value == 1, count, 0), na.rm = TRUE),
#       count_total = sum(count, na.rm = TRUE),
#       .groups = "drop"
#     ) |>
#     dplyr::mutate(
#       proportion_global = count_major / grand_total,
#       proportion_within_layer = dplyr::if_else(count_total > 0,
#                                                count_major / count_total, NA_real_),
#       EFG_CODE = stringr::str_extract(layer, "^[^.]+\\.[0-9]+") # Extract code before 2nd dot
#     ) |>
#     dplyr::arrange(desc(proportion_global)) |>
#     dplyr::filter(proportion_global >= 0.05) |>
#     dplyr::left_join(
#       efg_lookup |>
#         dplyr::select(
#           EFG_CODE = EFG_ID,
#           `Ecosystem Functional Group` = ECOSYSTEM_FUNCTIONAL_GROUP,
#           Realm = REALM,
#           Biome = BIOME
#         ),
#       by = "EFG_CODE"
#     ) |>
#     dplyr::mutate(`% of Area` = scales::percent(proportion_global, accuracy = 0.1)) |>
#     dplyr::select(EFG_CODE, `Ecosystem Functional Group`, Realm, Biome, `% of Area`)
# 
#   typ_tbl
# }
# 
# observeEvent(input$compute_typology, {
#   req(data_module$rasters())
#   typology_tbl <- compute_typology(data_module$rasters(), efg_lookup)
#   output$typology_table <- renderTable(typology_tbl)
#   # typology table ---------------------------------------------------------
#   output$get_table <- DT::renderDataTable({
#     typology_tbl
#   }, options = list(
#     paging = FALSE,        # no pagination
#     searching = FALSE,     # no search box
#     info = FALSE,          # no "Showing X of Y entries"
#     dom = 't'              # only show table
#   ), rownames = FALSE)
# 
# })

