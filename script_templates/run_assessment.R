# =============================================================================
# EcoCAT - IUCN Red List of Ecosystems assessment
# Reproducible script exported from EcoCAT Assessent Tool
# {{app_version}} on {{export_date}}
# =============================================================================
#
# This script repeats the assessment you ran in the EcoCAT Assessment tool:
#   1. loads ecosystem rasters and calculates area for each year
#   2. Criterion A  - observed change, projections, A1 / A2a / A2b
#   3. Criterion B1 - Extent of Occurrence (EOO)
#   4. Criterion B2 - Area of Occupancy (AOO)
#   5. saves tables and figures to outputs/
#
# To run (either):
#   * double-click EcoCAT_assessment.Rproj, then  source("run_assessment.R")
#   * or  e.g. source("path/to/EcoCAT_assessment/run_assessment.R", chdir = TRUE)
# =============================================================================


# SETTINGS ---------------------------------------------------------------------
# Values used in the app. Change them to re-run with different inputs.

# Folder containing the raster files
data_dir <- {{data_dir}}

# One row per raster: the file (in data_dir) and the year it represents.
# Add, remove or edit rows to change the assessment.
rasters <- {{rasters}}

# Ecosystem layer or class to assess (NULL = first layer)
selected_layer <- {{selected_layer}}

# Area the app was cropped to with "refine area" (NULL = full extent)
subset_area_file <- {{subset_area_file}}

# Which criteria to run (set to the criteria you calculated in the app)
run_criterion_a  <- {{run_a}}
run_criterion_b1 <- {{run_b1}}
run_criterion_b2 <- {{run_b2}}

# AOO method: "fast" (as used in the app) or "exact" (area-weighted, slower)
aoo_method <- "fast"

# Also save interactive HTML figures (needs the plotly and leaflet packages)
save_interactive_figures <- TRUE

output_dir <- "outputs"


# Check we are running from the export folder -----------------------------------
if (!file.exists("R/ecocat_functions.R")) {
  stop("Can't find R/ecocat_functions.R - the working directory needs to be the\n",
       "folder containing run_assessment.R. Either open EcoCAT_assessment.Rproj, or run:\n",
       "  source(\"path/to/EcoCAT_assessment/run_assessment.R\", chdir = TRUE)",
       call. = FALSE)
}


# PACKAGES ---------------------------------------------------------------------
required <- c("terra", "sf", "dplyr", "purrr", "tidyr", "ggplot2", "ggspatial", "exactextractr")
optional <- c("plotly", "leaflet", "htmlwidgets")   # interactive HTML figures only

is_missing <- function(pkgs) pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

# Offer to install anything missing (asks first - nothing is installed silently)
to_install <- c(is_missing(required), if (save_interactive_figures) is_missing(optional))
if (length(to_install) > 0) {
  cat("These R packages are needed but not installed:\n  ",
      paste(to_install, collapse = ", "), "\n", sep = "")
  answer <- if (interactive()) readline("Install them now from CRAN? [y/n]: ") else "n"
  if (tolower(substr(trimws(answer), 1, 1)) == "y") {
    install.packages(to_install)
  }
}

still_missing <- is_missing(required)
if (length(still_missing) > 0) {
  stop("Please install the missing packages, then run the script again:\n  install.packages(c(",
       paste0('"', still_missing, '"', collapse = ", "), "))", call. = FALSE)
}
if (save_interactive_figures && length(is_missing(optional)) > 0) {
  message("plotly / leaflet / htmlwidgets not installed - skipping interactive figures.")
  save_interactive_figures <- FALSE
}

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(ggplot2)
})

source("R/ecocat_functions.R")

# Use this R session's temp folder for terra's intermediate files
terra::terraOptions(tempdir = tempdir())

dir.create(file.path(output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)

save_table <- function(x, name) {
  utils::write.csv(as.data.frame(x), file.path(output_dir, paste0(name, ".csv")),
                   row.names = FALSE)
}
save_figure <- function(plot, name, width = 8, height = 6) {
  ggplot2::ggsave(file.path(output_dir, "figures", paste0(name, ".png")),
                  plot, width = width, height = height, dpi = 300)
}
save_widget <- function(widget, name) {
  htmlwidgets::saveWidget(widget, file.path(output_dir, "figures", paste0(name, ".html")),
                          selfcontained = FALSE)
}

# Map backgrounds used by the app's static Criterion B maps (cropped to this area)
read_if_exists <- function(path, reader) if (file.exists(path)) reader(path) else NULL
world  <- read_if_exists("data/countries.rds", readRDS)
relief <- read_if_exists("data/relief.tif",    terra::rast)
lakes  <- read_if_exists("data/lakes.rds",     readRDS)
rivers <- read_if_exists("data/rivers.rds",    readRDS)


# 1. LOAD RASTERS AND CALCULATE AREA --------------------------------------
subset_area <- if (!is.null(subset_area_file)) terra::vect(subset_area_file) else NULL

eco <- load_ecosystem_rasters(file.path(data_dir, rasters$file), rasters$year,
                              selected_layer, crop_to = subset_area)

areas         <- eco$areas
years         <- eco$years                      # sorted chronologically
latest_year   <- max(years)
latest_raster <- eco$rasters[[length(eco$rasters)]]

cat("\nEcosystem area by year", if (eco$is_bound) "(min / opt / max bounds)", ":\n")
print(areas)
save_table(areas, "areas")

# 2. CRITERION A - DECLINE IN DISTRIBUTION --------------------------------


crit_a <- NULL
if (run_criterion_a) {
  area_change_data     <- calc_area_change(areas)
  area_prediction_data <- predict_area_change(area_change_data) |> dplyr::ungroup()
  crit_a               <- assess_criterion_a(area_prediction_data, area_change_data)

  if (is.null(crit_a)) {
    message("Criterion A needs at least two years of data - skipped.")
  } else {
    cat("\nCriterion A1 (past 50 years):\n");     print(crit_a$a1)
    cat("\nCriterion A2a (next 50 years):\n");    print(crit_a$a2a)
    cat("\nCriterion A2b (worst 50-year window):\n"); print(crit_a$a2b)
    cat("\nCriterion A status (PRC):", crit_a$status, "\n")

    if (isTRUE(crit_a$near_term_prc > 10)) {
      message("Note: the near-term rate (", round(crit_a$near_term_prc, 1),
              " %/yr) is likely to be ecologically unrealistic.")
    }

    save_table(area_change_data,     "criterion_a_area_change")
    save_table(area_prediction_data, "criterion_a_projections")
    save_table(crit_a$a1,  "criterion_a1")
    save_table(crit_a$a2a, "criterion_a2a")
    save_table(crit_a$a2b, "criterion_a2b")

    save_figure(make_fig_a(area_prediction_data, is_bound = eco$is_bound),
                "criterion_a_area_change", width = 8, height = 5)
    if (save_interactive_figures) {
      save_widget(make_fig_a_plotly(area_prediction_data, is_bound = eco$is_bound),
                  "criterion_a_area_change")
    }
  }
}


# 3. CRITERION B1 - EXTENT OF OCCURRENCE ----------------------------------
crit_b1 <- NULL
if (run_criterion_b1) {
  crit_b1 <- assess_criterion_b1(latest_raster, latest_year)

  cat("\nCriterion B1 (EOO):\n")
  print(crit_b1$table)
  save_table(crit_b1$table, "criterion_b1_eoo")

  if (!is.null(world)) {
    save_figure(make_fig_b1(list(crit_b1$poly), eco$rasters, years, world = world,
                            basemap = relief, lakes = lakes, rivers = rivers),
                "criterion_b1_eoo_map")
  }
  if (save_interactive_figures) {
    save_widget(make_fig_b1_leaflet(crit_b1$poly, latest_raster, latest_year,
                                    base_provider = "Esri.WorldGrayCanvas",
                                    base_label    = "Basemap",
                                    draw_tools    = FALSE),
                "criterion_b1_eoo_map")
  }
}

# 4. CRITERION B2 - AREA OF OCCUPANCY -------------------------------------
crit_b2 <- NULL
if (run_criterion_b2) {
  crit_b2 <- assess_criterion_b2(latest_raster, method = aoo_method)
  if (is.null(crit_b2)) stop("No ecosystem cells found in the latest raster.")

  cat("\nCriterion B2 (AOO):\n")
  print(crit_b2$table_df)
  save_table(crit_b2$table_df, "criterion_b2_aoo")

  if (!is.null(world)) {
    aoo_cells <- terra::vect(rbind(crit_b2$dat_all, crit_b2$dat_sel))
    save_figure(make_fig_b2(aoo_cells, eco$rasters, years, world = world,
                            basemap = relief, lakes = lakes, rivers = rivers),
                "criterion_b2_aoo_map")
  }
  if (save_interactive_figures) {
    save_widget(make_fig_b2_leaflet(crit_b2$dat_all, crit_b2$dat_sel, latest_raster, latest_year,
                                    base_provider = "Esri.WorldGrayCanvas",
                                    base_label    = "Basemap",
                                    draw_tools    = FALSE),
                "criterion_b2_aoo_map")
  }
}


# 5. SUMMARY AND CHECK AGAINST THE APP ------------------------------------
status_a <- if (!is.null(crit_a)) crit_a$status else NA_character_
status_b <- criterion_b_status(crit_b1$table, crit_b2$table_df)


cat("\n================ Summary ================\n")
cat("Criterion A :", status_a, "\n")
cat("Criterion B :", status_b, "\n")
cat("Overall     :", worst_rle_status(status_a, status_b), "\n")
cat("(Criterion B also requires continuing decline or few threat-defined\n",
    " locations - see the RLE guidelines.)\n", sep = "")


cat("\nTables and figures saved to", normalizePath(output_dir), "\n")

# R and package versions used for this run
writeLines(utils::capture.output(utils::sessionInfo()),
           file.path(output_dir, "session_info.txt"))
