# modules/script_export.R
# -----------------------------------------------------------------------------
# Adds a "Download R Script" button that exports a self-contained, reproducible
# R script capturing all user inputs, analysis steps, figures, and maps
# performed in the GUI.
#
# The user can optionally type/paste the folder path where their raster files
# are stored; if provided, real paths are embedded in the script instead of
# PATH/TO placeholders.
#
# Add to app.R: source("modules/script_export.R")
# Add to UI:    script_export_ui("script_export")      (e.g. inside report tab)
# Add to server: script_export_server("script_export",
#                  upload_module, criterion_a_module, criterion_b_module)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
script_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$p(
      tags$b("Optional: "),
      "Enter the folder path where your raster files are saved on your machine. ",
      "If provided, exact file paths will be written into the script.",
      style = "color: #555; font-size: 0.9em; margin-bottom: 4px;"
    ),
    div(
      style = "display: flex; gap: 8px; align-items: center; margin-bottom: 12px;",
      textInput(
        ns("folder_path"),
        label = NULL,
        placeholder = "e.g.  C:/Users/you/data   or   /home/you/data",
        width = "420px"
      ),
      actionButton(
        ns("clear_path"), "Clear"#,
      #  style = "height: 36px; margin-top: 0;"
      ),downloadButton(
      ns("download_script"), "Download R Script",
      style = "background-color: #2c7bb6; color: white; border: none;margin-top: 0;"
    ),
    uiOutput(ns("path_preview"))
    )
    
   
    
   # tags$br(),
    
  )
}


# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
script_export_server <- function(id, upload_module, criterion_a_module, criterion_b_module) {
  moduleServer(id, function(input, output, session) {
    
    # ------------------------------------------------------------------
    # Clear button
    # ------------------------------------------------------------------
    observeEvent(input$clear_path, {
      updateTextInput(session, "folder_path", value = "")
    })
    
    # ------------------------------------------------------------------
    # Reactive: cleaned folder path (NULL if empty)
    # ------------------------------------------------------------------
    folder_path <- reactive({
      p <- trimws(input$folder_path)
      p <- gsub("\\\\", "/", p)   # normalise Windows backslashes
      p <- sub("/$", "", p)        # strip trailing slash
      if (nzchar(p)) p else NULL
    })
    
    # ------------------------------------------------------------------
    # Live preview of what the first path will look like
    # ------------------------------------------------------------------
    output$path_preview <- renderUI({
      tif_info      <- tryCatch(upload_module$tifs(), error = function(e) NULL)
      using_example <- isTRUE(tryCatch(upload_module$example(), error = function(e) FALSE))
      if (using_example || is.null(tif_info)) return(NULL)
      
      first_name <- tif_info$name[1]
      fp <- folder_path()
      
      if (is.null(fp)) {
        example_path <- paste0("PATH/TO/", first_name)
        colour <- "#888"; icon <- "\u2139"
        note   <- "No folder entered \u2014 placeholders will be used"
      } else {
        example_path <- paste0(fp, "/", first_name)
        colour <- "#2e7d32"; icon <- "\u2713"
        note   <- "Paths will be embedded in the script"
      }
      
      tags$p(
        style = paste0("font-size: 0.85em; color: ", colour, "; margin-bottom: 4px;"),
        icon, " ", note, ":",
       # tags$br(),
        tags$code(example_path)
      )
    })
    
    
    # ------------------------------------------------------------------
    # Collect all user parameters into one reactive snapshot
    # ------------------------------------------------------------------
    params <- reactive({
      
      tif_info      <- tryCatch(upload_module$tifs(), error = function(e) NULL)
      using_example <- isTRUE(tryCatch(upload_module$example(), error = function(e) FALSE))
      tif_names     <- if (!is.null(tif_info)) tif_info$name else NULL
      
      fp <- folder_path()
      resolved_paths <- if (using_example || is.null(tif_names)) {
        NULL
      } else if (!is.null(fp)) {
        paste0(fp, "/", tif_names)
      } else {
        paste0("PATH/TO/", tif_names)
      }
      
      list(
        using_example        = using_example,
        tif_names            = if (using_example) "itigithicket_zambia_example_2022.tif" else tif_names,
        resolved_paths       = resolved_paths,
        folder_provided      = !is.null(fp),
        years                = tryCatch(upload_module$years(),          error = function(e) NULL),
        selected_layer       = tryCatch(upload_module$selected_layer(), error = function(e) NULL),
        is_bound_raster      = isTRUE(tryCatch(upload_module$is_bound_raster(), error = function(e) FALSE)),
        area_change_data     = tryCatch(criterion_a_module$area_change_data(),     error = function(e) NULL),
        area_prediction_data = tryCatch(criterion_a_module$area_prediction_data(), error = function(e) NULL),
        mcp_table            = tryCatch(criterion_b_module$mcp_table(), error = function(e) NULL),
        b2_table             = tryCatch(criterion_b_module$b2_table(),  error = function(e) NULL)
      )
    })
    
    
    # ------------------------------------------------------------------
    # Build the script as a character string
    # ------------------------------------------------------------------
    generate_script <- function(p) {
      
      run_a  <- !is.null(p$area_change_data)
      run_b1 <- !is.null(p$mcp_table)
      run_b2 <- !is.null(p$b2_table)
      
      # ---- Header ----------------------------------------------------------------
      header <- glue::glue(
        '# =============================================================================
# EcoCAT RLE Assessment Tool - Auto-generated reproducible script
# Generated: <<Sys.Date()>>
# =============================================================================
# Reproduces your assessment, figures, and maps outside the Shiny interface.
#
# BEFORE RUNNING:
#   1. Check / update the file path(s) in the INPUT FILES section
#   2. Ensure helpers/analysis_functions.R is accessible (see source() call)
#   3. Figures are saved to a "figures/" subfolder in your working directory
# =============================================================================


# =============================================================================
# STEP 0.1 - check and load packages
# =============================================================================

packages.required <- c("terra", "dplyr", "purrr", "sf", "exactextractr",
              "tidyr", "ggplot2", "plotly", "leaflet", "htmlwidgets", "mgcv")

for (pkg in packages.required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Create output folder for figures
dir.create("figures", showWarnings = FALSE)
    
    ',
.open = "<<", .close = ">>"
)

      # ---- functions ----------------------------------------------------------------
      # Read figures.R from disk and inline it verbatim so the exported script
      # is fully self-contained (no source() calls to app files).
      figures_src <- paste(readLines("modules/figures.R"), collapse = "\n")

functions_block <- paste0(
  '
# =============================================================================
# STEP 0.2 - Load functions
# =============================================================================

# -----------------------------------------------------------------------------
# 1) calculate area change
#    Handles standard data (Year, Area_km2) and bound data (Year, Bound, Area_km2)
calc_area_change <- function(area.data) {
  
  has_bound <- "Bound" %in% names(area.data)
  
  if (has_bound) {
    area.data %>%
      dplyr::select(Bound, Year, Area_km2) %>%
      dplyr::mutate(year = round(as.numeric(Year), 0), area = Area_km2,
                    Year = NULL, Area_km2 = NULL) %>%
      dplyr::group_by(Bound) %>%
      dplyr::arrange(year, .by_group = TRUE) %>%
      dplyr::mutate(
        dt                       = year - lag(year),
        area_change              = area - lag(area),
        proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
        absolute_rate_change     = area_change / dt
      )
  } else {
    area.data %>%
      dplyr::select(Year, Area_km2) %>%
      dplyr::mutate(year = round(as.numeric(Year), 0), area = Area_km2,
                    Year = NULL, Area_km2 = NULL) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(
        dt                       = year - lag(year),
        area_change              = area - lag(area),
        proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
        absolute_rate_change     = area_change / dt
      )
  }
}

# -----------------------------------------------------------------------------
# 2) Internal workhorse — single bound / ungrouped data
predict_area_change_single <- function(obsv_change, horizon = 50) {
  
  obsv_change <- dplyr::ungroup(obsv_change) %>% dplyr::arrange(year)
  if (nrow(obsv_change) < 2) return(data.frame())
  
  oldest       <- obsv_change %>% slice(1)
  oldest_rates <- obsv_change %>% slice(2)
  if (nrow(oldest_rates) == 0 || is.na(oldest_rates$proportional_rate_change) ||
      is.na(oldest_rates$absolute_rate_change)) oldest_rates <- obsv_change %>% slice(n())
  recent      <- obsv_change %>% slice(n())
  nYearsPast  <- max(0, horizon - (max(obsv_change$year) - min(obsv_change$year)))
  
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
  
  proj <- imap_dfr(list(recent = NULL, median = NULL), function(., scen) {
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
  })
  
  observed <- obsv_change %>%
    dplyr::select(year, area) %>%
    mutate(type = "observed", method = "observed", scenario = "observed", segment = "observed")
  
  bind_rows(observed, proj)
}

# 3) Public dispatcher — routes to single or per-bound
predict_area_change <- function(obsv_change, horizon = 50) {
  if ("Bound" %in% names(obsv_change)) {
    purrr::map_dfr(unique(dplyr::pull(obsv_change, Bound)), function(b) {
      predict_area_change_single(
        obsv_change %>% dplyr::filter(Bound == b) %>% dplyr::ungroup(),
        horizon
      ) %>% dplyr::mutate(Bound = b)
    })
  } else {
    predict_area_change_single(dplyr::ungroup(obsv_change), horizon)
  }
}

# -----------------------------------------------------------------------------
# 4) function needed for MCP calculation
makeEOO.SpatRaster <- function(input.data, year){
  EOO.polygon <- as.polygons(input.data, na.rm=T)
  EOO.poly <- convHull(EOO.polygon)
  EOO.poly$year <- year
  EOO.poly$area_km2 <- terra::expanse(EOO.poly, unit = "km")
  return(EOO.poly)
}

# -----------------------------------------------------------------------------
# 5) Shared figure functions (ggplot2 static + plotly interactive)
#    Inlined from modules/figures.R
',
  figures_src,
  "\n"
)
            
      # ---- Input files -----------------------------------------------------------
      if (isTRUE(p$using_example)) {
        file_block <-
          '# --- INPUT FILES (example dataset) ---
# Using the built-in Itigi-Sumbu thicket example dataset.
raster_files <- list.files("www/", pattern = "itigithicket_zambia_example_2022.tif",
                            full.names = TRUE)
'
      } else if (!is.null(p$resolved_paths) && length(p$resolved_paths) > 0) {
        path_lines <- paste(
          sapply(seq_along(p$resolved_paths), function(i) {
            glue::glue('  "{p$resolved_paths[i]}",  # time point {i}')
          }),
          collapse = "\n"
        )
        path_note <- if (p$folder_provided) {
          "# Paths set from folder entered in the app."
        } else {
          "# Replace each PATH/TO/... with the actual location of your file."
        }
        file_block <- glue::glue(
          '# --- INPUT FILES ---
{path_note}
raster_files <- c(
{path_lines}
)
')
      } else {
        file_block <-
          '# --- INPUT FILES ---
# Add your raster file paths here:
raster_files <- c(
  "PATH/TO/your_raster_t1.tif",  # time point 1
  "PATH/TO/your_raster_t2.tif"   # time point 2
)
'
      }
      
      # ---- Year inputs -----------------------------------------------------------
      if (!is.null(p$years) && length(p$years) > 0) {
        years_vec <- paste0("c(", paste(p$years, collapse = ", "), ")")
        year_note <- "# Years as entered in the app sidebar."
      } else {
        years_vec <- 'c(2000, 2020)  # replace with your actual survey years'
        year_note <- "# One year per raster file, in the same order as raster_files."
      }
      year_block <- glue::glue(
        '# --- YEAR INPUTS ---
{year_note}
years <- {years_vec}
')
      
      # ---- Selected layer --------------------------------------------------------
      if (!is.null(p$selected_layer) && nzchar(p$selected_layer)) {
        layer_val  <- glue::glue('"{p$selected_layer}"')
        layer_note <- "# Layer/class selected in the app sidebar."
      } else {
        layer_val  <- "NULL  # or set to a layer name / numeric value"
        layer_note <- "# NULL uses the first layer; set to a name or numeric value to select a class."
      }
      layer_block <- glue::glue(
        '# --- ECOSYSTEM LAYER SELECTION ---
{layer_note}
selected_layer <- {layer_val}
')
      
      # ---- Processing ------------------------------------------------------------
      processing_block <-
        '# =============================================================================
# STEP 1 - Load and process rasters
# =============================================================================
order_idx    <- order(as.numeric(years))
raster_files <- raster_files[order_idx]
years        <- years[order_idx]

# Detect whether rasters have min/opt/max bound layers
is_bound_raster <- identical(
  sort(names(terra::rast(raster_files[1]))),
  c("max", "min", "opt")
)

if (is_bound_raster) {
  # ---- Bound raster path: compute area for all three bounds -----------------
  areas <- do.call(rbind, lapply(seq_along(raster_files), function(i) {
    r_all <- terra::rast(raster_files[i])
    do.call(rbind, lapply(c("min", "opt", "max"), function(b) {
      r_b <- r_all[[b]]
      if (terra::global(r_b, "isNA")[1, 1] == 0)
        r_b <- terra::classify(r_b, matrix(c(0, NA), ncol = 2))
      if (!is.na(terra::crs(r_b)) && !terra::is.lonlat(r_b)) {
        area_m2 <- terra::global(r_b, "sum", na.rm = TRUE)[1, 1] * prod(terra::res(r_b))
      } else {
        area_m2 <- terra::expanse(r_b, unit = "m")[, 2]
      }
      data.frame(Year = years[i], Bound = b,
                 Area_ha = area_m2 / 10000, Area_km2 = area_m2 / 1e6)
    }))
  }))
  
} else {
  # ---- Standard path: single representative layer per file -----------------
  raster_list <- lapply(seq_along(raster_files), function(i) {
    r <- terra::rast(raster_files[i])

    if (!is.null(selected_layer) && nzchar(selected_layer)) {
      if (selected_layer %in% names(r)) {
        r <- r[[selected_layer]]
      } else if (terra::nlyr(r) == 1) {
        r_single <- r[[1]]
        uvals    <- terra::unique(r_single)[, 1]
        uvals    <- uvals[!is.na(uvals)]
        if (length(uvals) > 1) {
          sel_num <- suppressWarnings(as.numeric(selected_layer))
          if (!is.na(sel_num)) r <- terra::ifel(r_single == sel_num, 1, NA)
        }
      }
    } else {
      r <- r[[1]]
    }

    if (terra::global(r, "isNA")[1, 1] == 0)
      r <- terra::classify(r, matrix(c(0, NA), ncol = 2))

    names(r) <- years[i]
    r
  })

  areas <- do.call(rbind, lapply(seq_along(raster_list), function(i) {
    r <- raster_list[[i]]
    if (!is.na(terra::crs(r)) && !terra::is.lonlat(r)) {
      area_m2 <- terra::global(r, "sum", na.rm = TRUE)[1, 1] * prod(terra::res(r))
    } else {
      area_m2 <- terra::expanse(r, unit = "m")[, 2]
    }
    data.frame(Year = years[i], Area_ha = area_m2 / 10000, Area_km2 = area_m2 / 1e6)
  }))
}

cat("\nArea summary:\n")
print(areas)
'
      
      # ---- Criterion A -----------------------------------------------------------
      if (run_a) {
        
        if (isTRUE(p$is_bound_raster)) {
          # ------------------------------------------------------------------
          # BOUND RASTER PATH: min / opt / max layers
          # ------------------------------------------------------------------
          crit_a_block <- '# =============================================================================
# STEP 2 - Criterion A (decline in ecosystem distribution) — bound raster
# =============================================================================
area_change_data     <- calc_area_change(areas)          # returns Bound column
area_prediction_data <- predict_area_change(area_change_data) |> dplyr::ungroup()

# Assessment on \'opt\' bound only (central estimate)
results_opt <- area_prediction_data |> dplyr::filter(Bound == "opt")
obs_years   <- results_opt$year[results_opt$type == "observed"]

past.obsv.fut <- dplyr::bind_rows(
  results_opt |> dplyr::group_by(type, method, scenario, segment) |>
    dplyr::filter(segment == "future") |> dplyr::slice(dplyr::n()),
  results_opt |> dplyr::group_by(type, method, scenario, segment) |>
    dplyr::filter(segment == "past") |> dplyr::slice(1)
) |> dplyr::arrange(year)

latest_obs_area <- results_opt$area[results_opt$type == "observed" &
                                    results_opt$year == max(obs_years)][1]

a1_result <- past.obsv.fut |>
  dplyr::filter(segment == "past", method %in% c("ARC", "PRC")) |>
  dplyr::group_by(method) |> dplyr::slice_min(year, n = 1) |> dplyr::ungroup() |>
  dplyr::mutate(
    latest_observed_area = latest_obs_area,
    perc_change          = (latest_observed_area - area) / area,
    A1 = dplyr::case_when(
      perc_change < -0.8  ~ "CR", perc_change < -0.5  ~ "EN",
      perc_change < -0.3  ~ "VU", perc_change < -0.27 ~ "NT", TRUE ~ "LC"
    )
  )

cat("\nCriterion A1 result (opt bound):\n")
print(a1_result[, c("method", "perc_change", "A1")])

# ---------------------------------------------------------------------------
# FIGURE 1 - Criterion A: opt line + min/max ribbons (observed + projected)
# ---------------------------------------------------------------------------
# Interactive plotly (show_ribbon = TRUE — toggle to FALSE to hide ribbons)
p_plotly <- make_fig_a_plotly(area_prediction_data, is_bound = TRUE, show_ribbon = TRUE)
print(p_plotly)
htmlwidgets::saveWidget(p_plotly, "figures/criterion_a_area_change.html", selfcontained = TRUE)
cat("Saved: figures/criterion_a_area_change.html\n")

# Static ggplot2 (for reports / publication)
p_gg <- make_fig_a(area_prediction_data, is_bound = TRUE)
ggplot2::ggsave("figures/criterion_a_area_change.png", p_gg,
                width = 8, height = 5, dpi = 300)
cat("Saved: figures/criterion_a_area_change.png\n")
'

        } else {
          # ------------------------------------------------------------------
          # STANDARD PATH: single representative layer
          # ------------------------------------------------------------------
          crit_a_block <- '# =============================================================================
# STEP 2 - Criterion A (decline in ecosystem distribution)
# =============================================================================
area_change_data     <- calc_area_change(areas)
area_prediction_data <- predict_area_change(area_change_data) |> dplyr::ungroup()

obs_years <- area_prediction_data$year[area_prediction_data$type == "observed"]

# A1 / A2a assessment
past.obsv.fut <- dplyr::bind_rows(
  area_prediction_data |>
    dplyr::group_by(type, method, scenario, segment) |>
    dplyr::filter(segment == "future") |> dplyr::slice(dplyr::n()),
  area_prediction_data |>
    dplyr::group_by(type, method, scenario, segment) |>
    dplyr::filter(segment == "past") |> dplyr::slice(1)
) |> dplyr::arrange(year)

latest_obs_area <- area_prediction_data$area[
  area_prediction_data$type == "observed" &
  area_prediction_data$year == max(obs_years)][1]

a1_result <- past.obsv.fut |>
  dplyr::filter(segment == "past", method %in% c("ARC", "PRC")) |>
  dplyr::group_by(method) |> dplyr::slice_min(year, n = 1) |> dplyr::ungroup() |>
  dplyr::mutate(
    latest_observed_area = latest_obs_area,
    perc_change          = (latest_observed_area - area) / area,
    A1 = dplyr::case_when(
      perc_change < -0.8  ~ "CR", perc_change < -0.5  ~ "EN",
      perc_change < -0.3  ~ "VU", perc_change < -0.27 ~ "NT", TRUE ~ "LC"
    )
  )

cat("\nCriterion A1 result:\n")
print(a1_result[, c("method", "perc_change", "A1")])

# ---------------------------------------------------------------------------
# FIGURE 1 - Criterion A: area change and projections
# ---------------------------------------------------------------------------
# Interactive plotly
p_plotly <- make_fig_a_plotly(area_prediction_data, is_bound = FALSE)
print(p_plotly)
htmlwidgets::saveWidget(p_plotly, "figures/criterion_a_area_change.html", selfcontained = TRUE)
cat("Saved: figures/criterion_a_area_change.html\n")

# Static ggplot2
p_gg <- make_fig_a(area_prediction_data, is_bound = FALSE)
ggplot2::ggsave("figures/criterion_a_area_change.png", p_gg,
                width = 8, height = 5, dpi = 300)
cat("Saved: figures/criterion_a_area_change.png\n")
'
        }

      } else {
        crit_a_block <-
          '# =============================================================================
# STEP 2 - Criterion A
# =============================================================================
# Criterion A was not run during this session.
# To run it once areas is created in Step 1:
#   area_change_data     <- calc_area_change(areas)
#   area_prediction_data <- predict_area_change(area_change_data)
'
      }
    
    # ---- Criterion B1 (EOO) ----------------------------------------------------
    if (run_b1) {
      crit_b1_block <-
        '# =============================================================================
# STEP 3a - Criterion B1 (EOO - Extent of Occurrence)
# =============================================================================
latest_raster <- raster_list[[length(raster_list)]]
latest_year   <- max(years)

eoo_poly <- makeEOO.SpatRaster(latest_raster, latest_year)

eoo_df <- as.data.frame(eoo_poly)[, c("year", "area_km2")]
eoo_df$assessment <- dplyr::case_when(
  eoo_df$area_km2 < 2000  ~ "CR", eoo_df$area_km2 < 20000 ~ "EN",
  eoo_df$area_km2 < 50000 ~ "VU", eoo_df$area_km2 < 55000 ~ "NT",
  TRUE ~ "LC"
)

cat("\nCriterion B1 (EOO) result:\n")
print(eoo_df)

# ---------------------------------------------------------------------------
# FIGURE 2 - Criterion B1: EOO map
# ---------------------------------------------------------------------------
# Interactive leaflet map
map_b1 <- make_fig_b1_leaflet(eoo_poly, latest_raster, latest_year)
print(map_b1)
htmlwidgets::saveWidget(map_b1, "figures/criterion_b1_eoo_map.html", selfcontained = TRUE)
cat("Saved: figures/criterion_b1_eoo_map.html\n")

# Static ggplot2 map
p_b1 <- make_fig_b1(list(eoo_poly), list(latest_raster), years)
ggplot2::ggsave("figures/criterion_b1_eoo_map.png", p_b1,
                width = 8, height = 6, dpi = 300)
cat("Saved: figures/criterion_b1_eoo_map.png\n")
'
    } else {
      crit_b1_block <-
        '# =============================================================================
# STEP 3a - Criterion B1 (EOO)
# =============================================================================
# EOO was not calculated during this session.
# To run it:
#   eoo_poly <- makeEOO.SpatRaster(raster_list[[length(raster_list)]], max(years))
'
    }
    
    # ---- Criterion B2 (AOO) ----------------------------------------------------
    if (run_b2) {
      crit_b2_block <-
        '# =============================================================================
# STEP 3b - Criterion B2 (AOO - Area of Occupancy)
# =============================================================================
# Requires data/World_Cylindrical_Equal_Area_WKT.txt from the app data/ folder.

latest_raster <- raster_list[[length(raster_list)]]

grid_prj  <- readChar("data/World_Cylindrical_Equal_Area_WKT.txt",
                       file.info("data/World_Cylindrical_Equal_Area_WKT.txt")$size)
eau_wcea  <- terra::project(latest_raster, grid_prj)

grid      <- terra::rast(terra::ext(eau_wcea), res = c(10000, 10000), crs = terra::crs(eau_wcea))
grid_exp  <- terra::extend(grid, c(2, 2))
grid_exp[]<- 0

grid_poly    <- terra::as.polygons(grid_exp, aggregate = FALSE)
grid_poly_sf <- sf::st_as_sf(grid_poly)
grid_poly_sf$cell_id <- seq_len(nrow(grid_poly_sf))

aoo_area <- exactextractr::exact_extract(
  eau_wcea, grid_poly_sf, "sum", coverage_area = TRUE, force_df = TRUE
)
names(aoo_area) <- names(eau_wcea)

aoo_counts <- cbind(cell_id = grid_poly_sf$cell_id, aoo_area) |>
  tidyr::pivot_longer(cols = -cell_id, names_to = "year", values_to = "area")

aoo_cumulative <- aoo_counts |>
  dplyr::group_by(year) |>
  dplyr::arrange(area, .by_group = TRUE) |>
  dplyr::mutate(cum_area = cumsum(area),
                cum_prop = cum_area / sum(area, na.rm = TRUE)) |>
  dplyr::ungroup()

n_cells <- aoo_cumulative |>
  dplyr::group_by(year) |>
  dplyr::summarise(n        = sum(area > 0, na.rm = TRUE),
                   n_exceed = sum(cum_prop > 0.01, na.rm = TRUE))

n_cells$assessment <- dplyr::case_when(
  n_cells$n_exceed < 2  ~ "CR", n_cells$n_exceed < 20 ~ "EN",
  n_cells$n_exceed < 50 ~ "VU", n_cells$n_exceed < 55 ~ "NT",
  TRUE ~ "LC"
)

cat("\nCriterion B2 (AOO) result:\n")
print(n_cells)

# ---------------------------------------------------------------------------
# FIGURE 3 - Criterion B2: AOO map
# ---------------------------------------------------------------------------
grid_sf_4326 <- sf::st_transform(grid_poly_sf, crs = 4326)

dat_all <- grid_sf_4326[grid_sf_4326$cell_id %in% all_ids$cell_ids[[1]], ]
dat_sel <- grid_sf_4326[grid_sf_4326$cell_id %in% sel_ids$cell_ids[[1]], ]

# Interactive leaflet map
map_b2 <- make_fig_b2_leaflet(dat_all, dat_sel, latest_raster, max(years))
print(map_b2)
htmlwidgets::saveWidget(map_b2, "figures/criterion_b2_aoo_map.html", selfcontained = TRUE)
cat("Saved: figures/criterion_b2_aoo_map.html\n")

# Static ggplot2 map — build a SpatVector with $type column as make_fig_b2 expects
b2_poly_export <- rbind(
  cbind(sf::st_drop_geometry(dat_all), type = "all") |>
    sf::st_set_geometry(sf::st_geometry(dat_all)),
  cbind(sf::st_drop_geometry(dat_sel), type = "assessment") |>
    sf::st_set_geometry(sf::st_geometry(dat_sel))
)
p_b2 <- make_fig_b2(terra::vect(b2_poly_export), list(latest_raster), years)
ggplot2::ggsave("figures/criterion_b2_aoo_map.png", p_b2,
                width = 8, height = 6, dpi = 300)
cat("Saved: figures/criterion_b2_aoo_map.png\n")
'
    } else {
      crit_b2_block <-
        '# =============================================================================
# STEP 3b - Criterion B2 (AOO)
# =============================================================================
# AOO was not calculated during this session.
# Ensure data/World_Cylindrical_Equal_Area_WKT.txt is accessible, then
# follow the AOO workflow above.
'
    }
    
    # ---- Footer ----------------------------------------------------------------
    footer <-
      '# =============================================================================
# END OF SCRIPT
# Outputs saved to figures/
# For questions or issues, refer to the EcoCAT documentation or GitHub repository.
# =============================================================================
'
    
    paste(header, functions_block, file_block, year_block, layer_block,
          processing_block, crit_a_block, crit_b1_block, crit_b2_block,
          footer, sep = "\n")
    }


# ------------------------------------------------------------------
# Download handler
# ------------------------------------------------------------------
output$download_script <- downloadHandler(
  filename = function() {
    paste0("EcoCAT_RLE_assessment_", format(Sys.Date(), "%Y%m%d"), ".R")
  },
  content = function(file) {
    writeLines(generate_script(params()), file)
  }
)

  })
}