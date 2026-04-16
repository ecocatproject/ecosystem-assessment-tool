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
# Generated: <<Sys.time()>>
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
      
functions_block <- 
  '
# =============================================================================
# STEP 0.2 - Load functions
# =============================================================================

# -----------------------------------------------------------------------------
# 1) calculate area change
calc_area_change <- function(area.data) {
  # calculate stats for observed changes 
  area.data <- area.data %>%
    dplyr::select(Year, Area_km2) %>%
    dplyr::mutate(year = round(as.numeric(Year),0), area = Area_km2, Year = NULL, Area_km2 = NULL) %>%
    dplyr::arrange(year)
  
  obsv_change <- area.data %>%
    dplyr::mutate(
      dt = year - lag(year),
      area_change = area - lag(area),   # later - earlier (signed correctly)
      proportional_rate_change = ((area / lag(area))^(1 / dt) - 1) * 100,
      absolute_rate_change = area_change / dt,
    )
    
  return(obsv_change)
}

# -----------------------------------------------------------------------------
# 2) Function to calculate decline statistics + projections
predict_area_change <- function(obsv_change, horizon = 50) {
  
  # helper data for projections
  oldest <- cbind(
    obsv_change %>% slice(1) %>% dplyr::select(year, area),                        # select the 1st row of year and area data, but need the next decline stat
    obsv_change %>% slice(2) %>% dplyr::select(absolute_rate_change, proportional_rate_change) # # select the  1st set of decline stats
  )
  recent <- obsv_change %>% slice(n())                                      # choose the most recent row
  nYearsPast <- (horizon - (max(obsv_change$year) - min(obsv_change$year))) # oldest data
  if (nYearsPast < 0) nYearsPast <- 0                                       # safeguard if there happens to be early estimates?
  
  # projection function
  # this is used separately for future and past projections
  # and is applied to future projections using imap (purrr package)
  # project_seq <- function(start_area, rate, type, years, direction = "future") {
    project_seq <- function(start_area, rate, type, years, direction = c("future", "past")) {
      direction <- match.arg(direction)
      
    seq_df <- data.frame(i = 0:years) %>%
      mutate(
        area = case_when(
          type == "PRC" & direction == "future" ~ start_area * (1 + rate/100)^i,
          type == "PRC" & direction == "past"   ~ start_area * (1 + rate/100)^(-i),
          type == "ARC" & direction == "future" ~ start_area + rate * i,
          type == "ARC" & direction == "past"   ~ start_area - rate * i,
          TRUE ~ NA_real_
        ),
      area = pmax(area, 0),   # this prevents negative values
      #  area = area,
        year = if (direction == "future") recent$year + i else oldest$year - i,
      ptype = "projected",
      method = type
      ) 
    return(seq_df)
  }
  
  # Past extrapolation (using oldest rate)
  past_proj <- bind_rows(
    # project_seq(oldest$area, -oldest$annual_rate_change, "ARC", nYearsPast, "past"),
    project_seq(oldest$area, oldest$proportional_rate_change, "PRC", nYearsPast, "past"),
    project_seq(oldest$area, oldest$absolute_rate_change, "ARC", nYearsPast, "past")
  ) %>% mutate(scenario = "past") %>%
    dplyr::select(year, area,  type = ptype, method, scenario) %>%
    dplyr::arrange(year)
  
  # Future projections
  scenarios <- list(
    median = list(
      PRC = median(obsv_change$proportional_rate_change, na.rm = TRUE),
      ARC = median(obsv_change$absolute_rate_change, na.rm = TRUE)
    ),
    recent = list(
      PRC = recent$proportional_rate_change,
      ARC = recent$absolute_rate_change
    ),
    worst = list(
      PRC = min(0, min(obsv_change$proportional_rate_change, na.rm = TRUE)), # clamped to 0 to prevent increases
      ARC = min(0, min(obsv_change$absolute_rate_change, na.rm = TRUE))
    )
  )
  
  # If recent and worst are the same, merge them into "recent/worst"
  equal_check <- all.equal(scenarios$recent, scenarios$worst, tolerance = 1e-8)
  if (isTRUE(equal_check)) {
    scenarios$`recent/worst` <- scenarios$worst
    scenarios$recent <- NULL
    scenarios$worst <- NULL
  }
  
  future_proj <- imap_dfr(scenarios, function(rates, scen) { # this iterates over 2 arguments at a time (a bit like seq_along...)
    bind_rows(
      project_seq(recent$area, rates$PRC, "PRC", horizon, "future"),
      project_seq(recent$area, rates$ARC, "ARC", horizon, "future")
    ) %>% mutate(scenario = scen) %>%
      dplyr::select(year, area,  type = ptype, method, scenario) %>%
      arrange(year)
  })
  
  # Combine observed + projections
  observed <- obsv_change %>% 
    dplyr::select(year, area) %>%                                                # select the data required for combining
    mutate(type = "observed", method = "observed", scenario = "observed") # force names to comply with other data
  
  out <- bind_rows(past_proj, observed,  future_proj)                     # combine the 3 sets of data
  
  return(out)
}

# -----------------------------------------------------------------------------
# 3) function needed for MCP calculation
makeEOO.SpatRaster <- function(input.data, year){
  EOO.polygon <- as.polygons(input.data, na.rm=T)
  EOO.poly <- convHull(EOO.polygon)
  # add attributes directly
  EOO.poly$year <- year
  EOO.poly$area_km2 <- terra::expanse(EOO.poly, unit = "km")
  
  return(EOO.poly)
}

      '
            
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

cat("\nArea summary:\n")
print(areas)
'
      
      # ---- Criterion A -----------------------------------------------------------
      if (run_a) {
        crit_a_block <-
          '# =============================================================================
# STEP 2 - Criterion A (decline in ecosystem distribution)
# =============================================================================
area_change_data     <- calc_area_change(areas)
area_prediction_data <- predict_area_change(area_change_data) |> dplyr::ungroup()

# A1 assessment
past.obsv.fut <- dplyr::bind_rows(
  area_prediction_data |>
    dplyr::group_by(type, method, scenario) |>
    dplyr::filter(scenario != "past") |> dplyr::slice(dplyr::n()),
  area_prediction_data |>
    dplyr::group_by(type, method, scenario) |>
    dplyr::filter(scenario == "past") |> dplyr::slice(1)
) |> dplyr::arrange(year)

a1_result <- past.obsv.fut |>
  dplyr::filter(scenario == "past", method %in% c("ARC", "PRC")) |>
  dplyr::group_by(method) |> dplyr::slice_min(year, n = 1) |> dplyr::ungroup() |>
  dplyr::mutate(
    latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
    perc_change          = (latest_observed_area - area) / area,
    A1 = dplyr::case_when(
      perc_change < -0.8  ~ "CR", perc_change < -0.5  ~ "EN",
      perc_change < -0.3  ~ "VU", perc_change < -0.27 ~ "NT", TRUE ~ "LC"
    )
  )

cat("\nCriterion A1 result:\n")
print(a1_result[, c("method", "perc_change", "A1")])

# ---------------------------------------------------------------------------
# FIGURE 1 - Criterion A: area change and projections (interactive + static)
# ---------------------------------------------------------------------------
colors    <- c("PRC" = "#D55E00", "ARC" = "#56B4E9")
linetypes <- c("median" = "dot", "recent" = "solid", "worst" = "dash",
               "recent/worst" = "dash", "past" = "longdashdot")

results <- area_prediction_data |>
  dplyr::mutate(hover_text = paste0("Year: ", year, "<br>Area: ", round(area, 0),
                                    "<br>Scenario: ", scenario, "<br>Method: ", method))

# -- Interactive plotly (mirrors the app) --
p_plotly <- plotly::plot_ly()

p_plotly <- p_plotly |>
  plotly::add_trace(
    data = dplyr::filter(results, type == "observed"),
    x = ~year, y = ~area, type = "scatter", mode = "lines+markers",
    line   = list(color = "grey20", width = 3),
    marker = list(color = "grey10", size = 6),
    name   = "Observed"
  )

proj_data <- dplyr::filter(results, type == "projected")
for (scen in unique(proj_data$scenario)) {
  for (meth in unique(proj_data$method)) {
    sub <- dplyr::filter(proj_data, scenario == scen, method == meth)
    p_plotly <- plotly::add_trace(
      p_plotly, data = sub,
      x = ~year, y = ~area, type = "scatter", mode = "lines",
      line = list(color = colors[meth], dash = linetypes[scen], width = 3),
      name = paste(scen, meth, sep = "/")
    )
  }
}

# Optional GAM smoother (only fitted with >3 observed points)
n_points <- nrow(area_change_data)
if (n_points > 3) {
  gam_model <- tryCatch(
    mgcv::gam(area ~ s(year, k = max(3, n_points - 2)), data = area_change_data),
    error = function(e) NULL
  )
  if (!is.null(gam_model)) {
    new_years <- data.frame(year = seq(min(area_prediction_data$year),
                                       max(area_prediction_data$year)))
    pred_gam  <- predict(gam_model, newdata = new_years, se.fit = TRUE)
    pred_gam  <- data.frame(
      year = new_years$year,
      fit  = pred_gam$fit,
      lwr  = pmax(pred_gam$fit - 2 * pred_gam$se.fit, 0),
      upr  = pred_gam$fit + 2 * pred_gam$se.fit
    )
    p_plotly <- p_plotly |>
      plotly::add_trace(
        data = dplyr::filter(pred_gam, year < min(area_change_data$year)),
        x = ~year, y = ~fit, type = "scatter", mode = "lines",
        line = list(color = "blue", width = 3, dash = "dash"),
        name = "GAM", legendgroup = "GAM", visible = "legendonly"
      ) |>
      plotly::add_trace(
        data = dplyr::filter(pred_gam, year > max(area_change_data$year)),
        x = ~year, y = ~fit, type = "scatter", mode = "lines",
        line = list(color = "blue", width = 3, dash = "dash"),
        name = "GAM", legendgroup = "GAM", showlegend = FALSE
      ) |>
      plotly::add_ribbons(
        data = pred_gam, x = ~year, ymin = ~lwr, ymax = ~upr,
        fillcolor = "rgba(0,0,255,0.2)", line = list(color = "transparent"),
        name = "95% CI", legendgroup = "GAM", showlegend = FALSE, visible = "legendonly"
      )
  }
}

p_plotly <- p_plotly |>
  plotly::layout(
    xaxis  = list(title = "Year"),
    yaxis  = list(title = "Area (km\u00b2)"),
    legend = list(title = list(text = "<b>Scenario / Method</b>")),
    font   = list(family = "Roboto", size = 14, color = "#333333")
  )

print(p_plotly)
htmlwidgets::saveWidget(p_plotly, "figures/criterion_a_area_change.html", selfcontained = TRUE)
cat("Saved: figures/criterion_a_area_change.html\n")

# -- Static ggplot2 version (suitable for publication / report) --
p_gg <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(results, type == "observed"),
    ggplot2::aes(x = year, y = area), colour = "grey20", linewidth = 1.2
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(results, type == "observed"),
    ggplot2::aes(x = year, y = area), colour = "grey10", size = 3
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(results, type == "projected"),
    ggplot2::aes(x = year, y = area, colour = method, linetype = scenario), linewidth = 1
  ) +
  ggplot2::scale_colour_manual(values = colors) +
  ggplot2::scale_linetype_manual(values = c(
    "median" = "dotted", "recent" = "solid", "worst" = "dashed",
    "recent/worst" = "dashed", "past" = "longdash"
  )) +
  ggplot2::labs(
    x = "Year", y = expression("Area (km"^2*")"),
    colour = "Method", linetype = "Scenario"
  ) +
  ggplot2::theme_bw(base_size = 13)

ggplot2::ggsave("figures/criterion_a_area_change.png", p_gg,
                width = 8, height = 5, dpi = 300)
cat("Saved: figures/criterion_a_area_change.png\n")
'
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
# FIGURE 2 - Criterion B1: EOO map (interactive leaflet + static ggplot2)
# ---------------------------------------------------------------------------
eoo_sf      <- sf::st_as_sf(terra::project(eoo_poly, "EPSG:4326"))
raster_4326 <- terra::project(latest_raster, "EPSG:4326")

# -- Interactive leaflet map (mirrors the app) --
ext_r <- as.vector(terra::ext(raster_4326))

map_b1 <- leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::fitBounds(ext_r["xmin"], ext_r["ymin"], ext_r["xmax"], ext_r["ymax"]) |>
  leaflet::addRasterImage(
    raster_4326, opacity = 0.7, colors = "#000000",
    group = paste0(latest_year, " classification")
  ) |>
  leaflet::addPolygons(
    data = eoo_sf, fillOpacity = 0.3, color = "black", weight = 2,
    group = paste0(latest_year, " EOO (MCP)"),
    label = ~paste0("Year: ", year, " | Area: ", round(area_km2, 2), " km\u00b2")
  ) |>
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellite") |>
  leaflet::addLayersControl(
    baseGroups   = c("OSM", "ESRI Satellite"),
    overlayGroups = c(paste0(latest_year, " EOO (MCP)"),
                      paste0(latest_year, " classification")),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

print(map_b1)
htmlwidgets::saveWidget(map_b1, "figures/criterion_b1_eoo_map.html", selfcontained = TRUE)
cat("Saved: figures/criterion_b1_eoo_map.html\n")

# -- Static ggplot2 map --
raster_df <- as.data.frame(raster_4326, xy = TRUE) |> stats::na.omit()

p_b1 <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = raster_df,
                       ggplot2::aes(x = x, y = y), fill = "grey30", alpha = 0.7) +
  ggplot2::geom_sf(data = eoo_sf, fill = NA, colour = "black", linewidth = 0.8) +
  ggplot2::coord_sf() +
  ggplot2::labs(
    title = paste0("Criterion B1 - EOO (", latest_year, ")"),
    subtitle = paste0("Area: ", round(eoo_df$area_km2, 1), " km\u00b2  |  Assessment: ", eoo_df$assessment),
    x = "Longitude", y = "Latitude"
  ) +
  ggplot2::theme_bw(base_size = 13)

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
# FIGURE 3 - Criterion B2: AOO map (interactive leaflet + static ggplot2)
# ---------------------------------------------------------------------------
grid_sf_4326 <- sf::st_transform(grid_poly_sf, crs = 4326)
raster_4326  <- terra::project(latest_raster, "EPSG:4326")

# Identify all occupied cells and cells exceeding the 1% AOO threshold
all_ids <- aoo_cumulative |>
  dplyr::filter(cum_prop > 0) |>
  dplyr::group_by(year) |>
  dplyr::summarise(cell_ids = list(cell_id))

sel_ids <- aoo_cumulative |>
  dplyr::filter(cum_prop > 0.01) |>
  dplyr::group_by(year) |>
  dplyr::summarise(cell_ids = list(cell_id))

dat_all <- grid_sf_4326[grid_sf_4326$cell_id %in% all_ids$cell_ids[[1]], ]
dat_sel <- grid_sf_4326[grid_sf_4326$cell_id %in% sel_ids$cell_ids[[1]], ]

# -- Interactive leaflet map (mirrors the app) --
map_b2 <- leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addRasterImage(raster_4326, opacity = 0.7, colors = "#000000",
                          group = "Ecosystem classification") |>
  leaflet::addPolygons(
    data = dat_all, group = "10x10 km grid",
    fillColor = "black", fillOpacity = 0.2, color = "black", weight = 0.5
  ) |>
  leaflet::addPolygons(
    data = dat_sel, group = "AOO (>1% occupancy)",
    fillColor = "orange", fillOpacity = 0.5, color = "black", weight = 0.5
  ) |>
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellite") |>
  leaflet::addLayersControl(
    baseGroups    = c("OSM", "ESRI Satellite"),
    overlayGroups = c("Ecosystem classification", "10x10 km grid", "AOO (>1% occupancy)"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    colors   = c("black", "orange"),
    labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
    opacity  = 0.7, title = "AOO layers"
  )

print(map_b2)
htmlwidgets::saveWidget(map_b2, "figures/criterion_b2_aoo_map.html", selfcontained = TRUE)
cat("Saved: figures/criterion_b2_aoo_map.html\n")

# -- Static ggplot2 map --
raster_df <- as.data.frame(raster_4326, xy = TRUE) |> stats::na.omit()

p_b2 <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = raster_df,
                       ggplot2::aes(x = x, y = y), fill = "grey30", alpha = 0.6) +
  ggplot2::geom_sf(data = dat_all,
                   fill = "black", alpha = 0.15, colour = "black", linewidth = 0.3) +
  ggplot2::geom_sf(data = dat_sel,
                   fill = "orange", alpha = 0.5, colour = "black", linewidth = 0.3) +
  ggplot2::coord_sf() +
  ggplot2::labs(
    title    = paste0("Criterion B2 - AOO (", max(years), ")"),
    subtitle = paste0("Total cells: ", n_cells$n, "  |  Cells >1% occupancy: ",
                      n_cells$n_exceed, "  |  Assessment: ", n_cells$assessment),
    x = "Longitude", y = "Latitude"
  ) +
  ggplot2::theme_bw(base_size = 13)

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