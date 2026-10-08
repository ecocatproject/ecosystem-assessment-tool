# modules/script_export.R
# ==============================================================================
# "Download R script" - exports a zip that reproduces the assessment outside
# the app:
#
#   run_assessment.R        script_templates/run_assessment.R with the user's
#                           settings filled in
#   R/ecocat_functions.R    the app's own function files, copied whole
#   data/                   the user's rasters (optional) + country outlines
#   README.txt
#
# The exported script contains no copies of analysis code: it calls the same
# functions the modules call, so it can't go out of date. To change what the
# script does, edit script_templates/run_assessment.R (a normal R file).
# ==============================================================================


# Files copied (whole) into R/ecocat_functions.R. They must contain only
# function definitions and constants - no Shiny code or top-level side effects.
export_function_files <- c(
  "helpers/analysis_functions.R",
  "helpers/rle_assessment.R",
  "helpers/figures.R"
)


# Map background layers: names of the global objects (from global.R) the app
# passes to the static maps. Change the right-hand side if yours are named
# differently. Missing objects are skipped.
export_map_layers <- c(
  world  = "countries.sf",  # sf country polygons (needed for the static B maps)
  relief = "relief",        # SpatRaster shaded relief, EPSG:4326
  lakes  = "lakes",         # sf polygons
  rivers = "rivers"         # sf lines
)


# ==============================================================================
# Pure helpers (no Shiny) - tested in tests/testthat/test-script-export.R
# ==============================================================================

# R source for a value, e.g. c(2000, 2020) or "opt" or NULL
r_literal <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n")

# Replace {{name}} placeholders; error if the template lacks one we fill
# (e.g. an exported script was saved over the template) or any are left over.
fill_template <- function(text, values) {
  for (nm in names(values)) {
    token <- paste0("{{", nm, "}}")
    if (!grepl(token, text, fixed = TRUE)) {
      stop("Template is missing placeholder ", token,
           " - is script_templates/ holding an exported script instead of the template?")
    }
    text <- gsub(token, values[[nm]], text, fixed = TRUE)
  }
  left <- unique(regmatches(text, gregexpr("\\{\\{[A-Za-z_]+\\}\\}", text))[[1]])
  if (length(left) > 0) stop("Unfilled template placeholders: ", paste(left, collapse = ", "))
  text
}

# R source for the file/year table - one row per raster so files and years
# can't get out of step when someone edits the script, e.g.
#   rbind(
#     data.frame(file = "eco_2000.tif", year = 2000),
#     data.frame(file = "eco_2020.tif", year = 2020)
#   )
r_raster_table <- function(files, years) {
  rows <- sprintf("  data.frame(file = %s, year = %s)",
                  vapply(files, r_literal, character(1)), years)
  paste0("rbind(\n", paste(rows, collapse = ",\n"), "\n)")
}

# Every raster needs exactly one valid year.
check_raster_years <- function(raster_names, years) {
  years <- suppressWarnings(as.numeric(years))
  if (length(raster_names) == 0) stop("No raster files are loaded.")
  if (length(raster_names) != length(years) || anyNA(years)) {
    stop("There are ", length(raster_names), " raster file(s) but ",
         sum(!is.na(years)), " valid year(s). Give every raster a year before exporting.")
  }
  invisible(TRUE)
}

# Gather the map background layers, cropped to the area the maps can show
# (the ecosystem plus its host countries, with padding) so the zip stays small.
#   raster_file : one of the processed rasters (defines the area)
#   layers      : named list of objects, e.g. list(world = countries.sf, ...)
crop_map_layers <- function(raster_file, layers, pad_deg = 1) {
  layers <- Filter(Negate(is.null), layers)
  if (length(layers) == 0) return(list())

  r    <- terra::rast(raster_file)
  e_ll <- terra::ext(terra::project(terra::as.polygons(terra::ext(r), crs = terra::crs(r)),
                                    "EPSG:4326"))
  e_ll <- as.vector(e_ll)                     # xmin, xmax, ymin, ymax
  bb <- sf::st_bbox(c(xmin = e_ll[[1]], ymin = e_ll[[3]],
                      xmax = e_ll[[2]], ymax = e_ll[[4]]), crs = 4326)

  # widen to the host countries, as the maps frame on them
  if (!is.null(layers$world)) {
    host <- suppressWarnings(sf::st_filter(sf::st_make_valid(layers$world), sf::st_as_sfc(bb)))
    if (nrow(host) > 0) {
      hb <- sf::st_bbox(host)
      bb[c("xmin", "ymin")] <- pmin(bb[c("xmin", "ymin")], hb[c("xmin", "ymin")])
      bb[c("xmax", "ymax")] <- pmax(bb[c("xmax", "ymax")], hb[c("xmax", "ymax")])
    }
  }
  pad <- pad_deg + 0.25 * max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
  bb  <- bb + c(-pad, -pad, pad, pad)

  crop_one <- function(x) {
    if (inherits(x, "SpatRaster")) {
      terra::crop(x, terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]))
    } else if (inherits(x, "SpatVector")) {
      sf::st_as_sf(terra::crop(x, terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]])))
    } else if (inherits(x, "sf")) {
      old <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)))
      suppressWarnings(sf::st_crop(sf::st_make_valid(x), bb))
    } else {
      x
    }
  }

  # world stays whole - the maps use it to find the host country
  out <- lapply(names(layers), function(nm) if (nm == "world") layers[[nm]] else crop_one(layers[[nm]]))
  names(out) <- names(layers)
  out
}

# Make free text safe for file and folder names on Windows / macOS / Linux:
# "Itigi-Sumbu thicket (Zambia)" -> "Itigi_Sumbu_thicket_Zambia".
# Returns NULL for empty / unusable input.
safe_file_name <- function(x, max_chars = 50) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  x <- iconv(enc2utf8(trimws(x[1])), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  if (is.na(x)) return(NULL)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", substr(x, 1, max_chars))
  if (nzchar(x)) x else NULL
}

read_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Write the export folder.
#   settings       : list(raster_names, years, selected_layer, subset_poly,
#                         data_dir, run_a, run_b1, run_b2, app_version)
#                    subset_poly is the app's crop polygon (SpatVector) or NULL
#   raster_sources : paths of raster files to copy into data/ (same order as
#                    raster_names), or NULL to leave data out
#   map_layers     : list(world, relief, lakes, rivers) for the static maps
#                    (see crop_map_layers()); any can be missing
build_export_bundle <- function(out_dir, settings,
                                raster_sources = NULL, map_layers = list(),
                                app_dir = ".") {

  check_raster_years(settings$raster_names, settings$years)
  settings$raster_names <- basename(settings$raster_names)

  dir.create(file.path(out_dir, "R"),    recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "data"), recursive = TRUE, showWarnings = FALSE)
  export_date <- format(Sys.Date())

  # ---- R/ecocat_functions.R: the app's own function files, unchanged -------
  fn_files <- file.path(app_dir, export_function_files)
  fn_text  <- vapply(seq_along(fn_files), function(i) {
    paste0("\n# ", strrep("=", 77), "\n# From ", export_function_files[i],
           "\n# ", strrep("=", 77), "\n\n", read_text(fn_files[i]))
  }, character(1))
  writeLines(c(
    paste0("# EcoCAT functions - exported from EcoCAT ", settings$app_version,
           " on ", export_date, "."),
    "# This is the code the app runs. It is sourced by run_assessment.R.",
    fn_text
  ), file.path(out_dir, "R", "ecocat_functions.R"))

  # ---- data/ ---------------------------------------------------------------
  data_dir <- settings$data_dir
  if (!is.null(raster_sources)) {
    if (length(raster_sources) != length(settings$raster_names)) {
      stop("Got ", length(raster_sources), " raster file(s) to copy but ",
           length(settings$raster_names), " file name(s).")
    }
    missing_src <- raster_sources[!file.exists(raster_sources)]
    if (length(missing_src) > 0) {
      stop("Raster file(s) to include in the export were not found:\n  ",
           paste(missing_src, collapse = "\n  "))
    }
    ok <- file.copy(raster_sources, file.path(out_dir, "data", settings$raster_names),
                    overwrite = TRUE)
    if (!all(ok)) stop("Could not copy raster files into the export.")
    data_dir <- "data"
  }
  # Map backgrounds
  if (!is.null(map_layers$world))  saveRDS(map_layers$world,  file.path(out_dir, "data", "countries.rds"))
  if (!is.null(map_layers$lakes))  saveRDS(map_layers$lakes,  file.path(out_dir, "data", "lakes.rds"))
  if (!is.null(map_layers$rivers)) saveRDS(map_layers$rivers, file.path(out_dir, "data", "rivers.rds"))
  if (!is.null(map_layers$relief)) {
    terra::writeRaster(map_layers$relief, file.path(out_dir, "data", "relief.tif"), overwrite = TRUE)
  }

  subset_file <- NULL
  if (!is.null(settings$subset_poly)) {
    subset_file <- "data/subset_area.gpkg"
    terra::writeVector(settings$subset_poly, file.path(out_dir, subset_file), overwrite = TRUE)
  }

  # ---- run_assessment.R and README -----------------------------------------
  tpl_dir <- file.path(app_dir, "script_templates")

  script <- fill_template(read_text(file.path(tpl_dir, "run_assessment.R")), list(
    app_version    = settings$app_version,
    export_date    = export_date,
    data_dir       = r_literal(data_dir),
    rasters        = r_raster_table(settings$raster_names, as.numeric(settings$years)),
    selected_layer = r_literal(settings$selected_layer),
    subset_area_file = r_literal(subset_file),
    run_a          = r_literal(isTRUE(settings$run_a)),
    run_b1         = r_literal(isTRUE(settings$run_b1)),
    run_b2         = r_literal(isTRUE(settings$run_b2))
  ))
  writeLines(script, file.path(out_dir, "run_assessment.R"))

  readme <- fill_template(read_text(file.path(tpl_dir, "README.txt")), list(
    app_version = settings$app_version,
    export_date = export_date,
    data_note   = if (!is.null(raster_sources)) {
      "Your raster files, as uploaded to the app."
    } else {
      paste0("Not included - set data_dir in run_assessment.R to the folder\n",
             "                          containing your raster files.")
    }
  ))
  writeLines(readme, file.path(out_dir, "README.txt"))

  writeLines(c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No"),
             file.path(out_dir, "EcoCAT_assessment.Rproj"))

  invisible(out_dir)
}


# ==============================================================================
# UI
# ==============================================================================
script_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p(
      "Download a zip containing an R script, the EcoCAT functions and the app's ",
      "results, to reproduce this assessment on your own computer.",
      style = "color: #555; font-size: 0.9em; margin-bottom: 8px;"
    ),
    tags$p("Unzip the file adn instructions can be found in the ", style = "color: #555; font-size: 0.9em; margin-bottom: 8px;"),
    uiOutput(ns("data_options")),
    uiOutput(ns("download_ui"))
  )
}


# ==============================================================================
# SERVER
# ==============================================================================
script_export_server <- function(id, upload_module, criterion_a_module, criterion_b_module,
                                 ecosystem_name = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    safely <- function(expr) tryCatch(expr, error = function(e) NULL)

    using_example <- reactive(isTRUE(safely(upload_module$example())))

    # Only offer the include/folder options for the user's own data
    output$data_options <- renderUI({
      pi <- processed()
      if (using_example() || is.null(pi)) return(NULL)

      size_mb <- sum(file.size(pi$rasters$datapath), na.rm = TRUE) / 1024^2
      tagList(
        checkboxInput(
          ns("include_data"),
          label = paste0("Include my raster files in the download",
                         if (!is.na(size_mb)) sprintf(" (%.0f MB)", size_mb)),
          value = is.na(size_mb) || size_mb < 200
        ),
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("include_data")),
          textInput(
            ns("data_dir"),
            label = "Folder containing your raster files (optional - can be edited in the script)",
            placeholder = "e.g. C:/Users/you/data",
            width = "420px"
          )
        )
      )
    })

    # ---- Only offer the download when files and years pair up -----------------
    # NULL when OK, otherwise the problem as text
    # What produced the current outputs: only the files ticked "Include", with
    # their years, layer and crop area, as recorded when processing finished.
    processed <- reactive(safely(upload_module$processed_inputs()))

    raster_year_problem <- reactive({
      pi <- processed()
      if (is.null(pi)) return("Process your maps first - the script reproduces the processed outputs.")
      tryCatch({
        check_raster_years(pi$rasters$name, pi$rasters$year)
        NULL
      }, error = function(e) conditionMessage(e))
    })

    output$download_ui <- renderUI({
      problem <- raster_year_problem()
      if (!is.null(problem)) {
        return(tags$p(problem, style = "color: #b35806; font-size: 0.9em;"))
      }
      downloadButton(
        ns("download_script"), "Download R script (.zip)",
        style = "background-color: #2c7bb6; color: white; border: none;"
      )
    })

    # ---- Everything the export needs, as plain values ------------------------
    export_inputs <- reactive({
      pi      <- processed()
      example <- using_example()
      req(pi)

      # Only the files that produced the outputs, already paired with their years
      raster_names   <- basename(pi$rasters$name)
      raster_sources <- if (example || isTRUE(input$include_data)) pi$rasters$datapath else NULL

      data_dir <- if (is.null(input$data_dir)) "" else input$data_dir
      data_dir <- trimws(gsub("\\\\", "/", data_dir))
      data_dir <- sub("/$", "", data_dir)
      if (!nzchar(data_dir)) data_dir <- "PATH/TO/your/raster/folder"


      a1 <- safely(criterion_a_module$a1_result())
      b1 <- safely(criterion_b_module$mcp_table())
      b2 <- safely(criterion_b_module$b2_table())

      list(
        settings = list(
          raster_names   = raster_names,
          years          = pi$rasters$year,
          selected_layer = pi$selected_layer,
          subset_poly    = pi$subset_poly,
          data_dir       = data_dir,
          run_a          = !is.null(a1),
          run_b1         = !is.null(b1),
          run_b2         = !is.null(b2),
          app_version    = get0("ECOCAT_VERSION", envir = globalenv(), ifnotfound = "(dev)")
        ),
        raster_sources = raster_sources,
        map_area_file  = pi$rasters$datapath[nrow(pi$rasters)]
      )
    })

    # ---- Download ------------------------------------------------------------
    # e.g. EcoCAT_Itigi_Sumbu_thicket_20261005.zip, unzipping to a folder of the
    # same name (so exports for different ecosystems don't overwrite each other)
    export_name <- reactive({
      eco <- safe_file_name(safely(ecosystem_name()))
      paste0("EcoCAT_", if (is.null(eco)) "assessment" else eco,
             "_", format(Sys.Date(), "%Y%m%d"))
    })

    output$download_script <- downloadHandler(
      filename = function() paste0(export_name(), ".zip"),
      content  = function(file) {
        x <- export_inputs()

        out_dir <- file.path(tempfile("ecocat_export_"), export_name())
        on.exit(unlink(dirname(out_dir), recursive = TRUE), add = TRUE)

        build_export_bundle(
          out_dir,
          settings       = x$settings,
          raster_sources = x$raster_sources,
          map_layers     = crop_map_layers(
            x$map_area_file,
            lapply(export_map_layers, get0, envir = globalenv())
          )
        )

        zip::zip(file,
                 files = list.files(dirname(out_dir), recursive = TRUE),
                 root  = dirname(out_dir))
      },
      contentType = "application/zip"
    )
  })
}
