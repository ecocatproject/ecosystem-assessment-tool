# modules/report.R

# =============================================================================
# Report Module
# =============================================================================

# This module sets up report and download options for:
# .html / .docx report
# data files
# stand alone r analysis script
# thumbnails for Criterion A and B figures (graphs and maps)


# ==============================================================================
# UI
# ==============================================================================

report_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Report & Data",
    
    page_fluid(navset_card_pill(
      # -----------------------------------------------------------------------
      # report
      nav_panel(
        tags$span("Report", tooltip(bs_icon("info-circle"),
                                    "Optional - Information provided here is used solely for creating the report and is not retained.",
                                    tags$p("It is recommended to include the name of the ecosystem that has been assessed"),
                                    style = "color: #555; font-size: 0.9em;"
        )),
        card_body(
          fluidRow(
            column(
              width = 6,
              textInput(ns("ecosystem_name"),
                        tags$span(tags$b("Ecosystem Name:"),tooltip(bs_icon("info-circle"), "Name of ecosystem inlcuded in this assessment"), style = "color: #555;"),
                        placeholder = "Ecosystem name for report - Optional"),
              textInput(ns("organisation"),
                        tags$span(tags$b("Organisation(s):"),tooltip(bs_icon("info-circle"), "Optionally include the organisation name(s) in the subtitle"), style = "color: #555;"),
                        placeholder = "Organisation name(s) for report - Optional")),
            column(
              width = 6,
              textInput(ns("author"),
                        tags$span(tags$b("Author:"),tooltip(bs_icon("info-circle"), "Optionally include the assessor name(s) in the subtitle"), style = "color: #555;"),
                        placeholder = "Assessor name(s) for report - Optional"),
              selectInput(
                ns("report_format"),
                tags$span(tags$b("Choose report format:"),tooltip(bs_icon("info-circle"), "HTML format will include interactive maps and figures."),
                                                                  #PDF format will include static map and figures."),
                          style = "color: #555;"),
                choices = c("HTML" = "html")#, "PDF" = "pdf") # "PDF" = "pdf"
              )),
            downloadButton(ns("generate_report"), "Generate RLE Assessment Report")
          )
        ),
      ),
      # -----------------------------------------------------------------------
      # data 
      nav_panel(
        "Data",
        card_body(
          checkboxGroupInput(ns("dataset_choice"), tags$b("Choose datasets:"), choices = NULL),
          downloadButton(ns("download_file"), "Download data")
        )
      ),

      
      # -----------------------------------------------------------------------
      # scripts
      nav_panel(
        tags$span("Script", tooltip(bs_icon("info-circle"),
                                    "Download a standalone R script that reproduces your assessment outside the app,",
                                    "including all figures and maps.",
                                    style = "color: #555; font-size: 0.9em;"
        )),
        card_body(
          script_export_ui(("script_export")),
          # height=50
        )
      ),

      
      # -----------------------------------------------------------------------
      # Thumbnails 
      nav_panel(
        tags$span("Figures", tooltip(bs_icon("info-circle"),
                                     "Static publication-ready figures generated from your assessment.",
                                     tags$p("Select figures to preview and download as PNG."),
                                     style = "color: #555; font-size: 0.9em;"
        )),
        card_body(
          # tags$p(
          #   "Static publication-ready figures generated from your assessment.",
          #   "Select figures to preview and download as PNG.",
          #   style = "color: #555; font-size: 0.9em;"
          # ),
          fluidRow(
            # current distribution and change (if available)
            column(
              width = 4,
              tags$b("Distribution"),
              withSpinner(uiOutput(ns("fig_change_preview")),
                          type = 6, color = "#669900", proxy.height = "160px"),
              tags$br(),
              downloadButton(ns("dl_fig_change"), "Download PNG", disabled = length(years()) <1,
                             style = "width:100%; margin-top: 4px;")
            ),
            # Criterion A trend figure
            column(
              width = 4,
              tags$b("Criterion A - Area change trend"),
              withSpinner(uiOutput(ns("fig_a_preview")),
                          type = 6, color = "#669900", proxy.height = "160px"),
              tags$br(),
              downloadButton(ns("dl_fig_a"), "Download PNG",
                             style = "width:100%; margin-top: 4px;")
            ),
            # Criterion B1 EOO map
            column(
              width = 4,
              tags$b("Subcriterion B1 - EOO map"),
              withSpinner(uiOutput(ns("fig_b1_preview")),
                          type = 6, color = "#669900", proxy.height = "160px"),
              tags$br(),
              downloadButton(ns("dl_fig_b1"), "Download PNG",
                             style = "width:100%; margin-top: 4px;")
            ),
            # Criterion B2 AOO map
            column(
              width = 4,
              tags$b("Subcriterion B2 - AOO map"),
              withSpinner(uiOutput(ns("fig_b2_preview")),
                          type = 6, color = "#669900", proxy.height = "160px"),
              tags$br(),
              downloadButton(ns("dl_fig_b2"), "Download PNG",
                             style = "width:100%; margin-top: 4px;")
            )
          )
        )
      )
    ))
  )
  
}


# ==============================================================================
# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
# ==============================================================================
report_module_server <- function(datasets,
                                 id,
                                 ecosystem_name, author, organisation,# email,
                                 rasters,
                                 rasters_change,
                                 areas,
                                 years,
                                 countries,
                                 rainfall,
                                 temperature,
                                 elevation,
                                 typology,
                                 area_change_data,
                                 area_prediction_data,
                                 a1_result,
                                 a2a_result,
                                 a2b_result,
                                 criterion_a_status,
                                 criterion_b_status,
                                 mcp_data,
                                 mcp_poly,
                                 b2_data,
                                 aoo_sel_ids,
                                 aoo_all_ids,
                                 b2_poly
) {
  moduleServer(id, function(input, output, session) {
    
    
    
    # --------------------------------------------------------------------------
    # conditionally build file name
    output$generate_report <- downloadHandler(
      filename = function() {
        ext <- input$report_format
        name_part <- if (!is.null(input$ecosystem_name) && nzchar(trimws(input$ecosystem_name))) {
          paste0(gsub(" ", "_", trimws(input$ecosystem_name)), "_")
        } else {
          ""
        }
        paste0("RLE_Assessment_", name_part, format(Sys.Date(), "%d-%b-%y"), ".", ext)
      },
      content = function(file) {
        
        # Create a fresh temp Rmd on EVERY click ------------------------------
        report_path <- tempfile(fileext = ".Rmd")
        # template_path <- file.path("www", "rle_report_template_html_testing.Rmd")
        template_path <- file.path("report", "report_main.Rmd")
        file.copy(template_path, report_path, overwrite = TRUE)
        
        # Show modal popup while rendering ------------------------------------
        # showModal(report_modal)
        showModal(modalDialog(
          title = "Generating report...",
          tagList(
            div(
              style = "text-align:center;",
              tags$h4("Please wait while your RLE assessment report is being generated."),
              # tags$p("This process usually takes approximately 10 seconds."),
              tags$p(tags$em(paste0("Rendering the ", input$report_format, " report..."))),
              withSpinner(uiOutput(session$ns("dummy_spinner")), type = 5, color = "#669900")
            )
          ),
          easyClose = TRUE,
          footer = NULL
        ))
        
        # Create temporary directory for large objects ------------------------
        tmpdir <- tempdir()
        
        # Save rasters safely
        raster_list <- rasters()
        raster_paths <- NULL
        if (!is.null(raster_list)) {
          raster_paths <- file.path(tmpdir, paste0("raster_", seq_along(raster_list), ".tif"))
          for (i in seq_along(raster_list)) {
            terra::writeRaster(raster_list[[i]], raster_paths[i], overwrite = TRUE)
          }
        }
        
        # Save rasters_change (the loss SpatRaster) so the "Mapped distribution"
        # section in the report can show the red loss overlay.  NULL when only a
        # single assessment year was uploaded, in which case the map falls back
        # to showing just the recent raster.
        rasters_change_obj   <- rasters_change()
        rasters_change_paths <- NULL
        if (!is.null(rasters_change_obj)) {
          rasters_change_paths <- file.path(tmpdir, "rasters_change.tif")
          terra::writeRaster(rasters_change_obj, rasters_change_paths, overwrite = TRUE)
        }
        
        # Build params list for R Markdown ------------------------------------
        params_list <- list(
          ecosystem_name       = input$ecosystem_name,
          author               = input$author,
          organisation         = input$organisation,
          areas                = (areas()),
          years                = (years()),
          raster_files         = raster_paths,
          rasters_change_files = rasters_change_paths,
          # roi_vec_file        = roi_vec_path,
          countries            = (countries()),
          rainfall             = (rainfall()),
          temperature          = (temperature()),
          elevation            = (elevation()),
          typology             = (typology()),
          area_change_data     = (area_change_data()),
          area_prediction_data = (area_prediction_data()),
          a1_result            = (a1_result()),
          a2a_result           = (a2a_result()),
          a2b_result           = (a2b_result()),
          criterion_a_status   = (criterion_a_status()),
          criterion_b_status   = (criterion_b_status()),
          mcp_data             = (mcp_data()),
          mcp_poly             = (mcp_poly()),
          b2_data              = (b2_data()),
          aoo_sel_ids          = (aoo_sel_ids()),
          aoo_all_ids          = (aoo_all_ids()),
          b2_poly              = (b2_poly())
        )
        
        str(params_list)
        
        # Choose output format  ----------------------------------------------
        fmt <- switch(input$report_format,
                      html = NULL,
                      pdf = "pdf_document")
        
        # --- Render the report ----------------------------------------------
        report_dir <- normalizePath("report")
        tryCatch({
          rmarkdown::render(
            # input = "rle_report_template_html_testing.rmd", # for local app testing
            input = report_path, # safe for deployed app # for deployed app
            output_format = fmt,
            output_file = file,
            params = params_list,
            envir = new.env(parent = globalenv()),
            # knit_root_dir = getwd()
            knit_root_dir = report_dir
          )
          
        },
        error = function(e) {
          removeModal()
          showModal(modalDialog(
            title = "Error",
            tags$p("An error occurred while generating the report:"),
            tags$pre(conditionMessage(e)),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          stop(e)
        }
        )
        
        # # Remove modal after successful render -------------------------------
        removeModal()
        
      }
    )
    
    
    # =========================================================================
    # Thumbnails
    # =========================================================================
    
    # =========================================================================
    # Distribution change
    # =========================================================================
    fig_change <- reactive({
      req(rasters())
      # req(rasters_change())
      # make_fig_change(rasters_change(), years)
      # make_fig_loss(r_map = rasters(), r_loss = rasters_change(), years())
      make_fig_distribution(r_map = rasters(), r_loss = rasters_change(), years(),
                            world = countries.sf, 
                            lakes = lakes.sf,
                            rivers = rivers.sf,
                            basemap = relief)
      
      
    })
    
    # =========================================================================
    # Criterion A: area change trend
    # =========================================================================
    fig_a <- reactive({
      req(area_prediction_data())
      make_fig_a(area_prediction_data())
    })
    
    # =========================================================================
    # Criterion B1: EOO
    # =========================================================================
    #   make_fig_b1 from figures_ggplot.R
    fig_b1 <- reactive({
      req(mcp_poly(), rasters())
      make_fig_b1(mcp_poly(), rasters(), years(),
                  world = countries.sf, 
                  lakes = lakes.sf,
                  rivers = rivers.sf,
                 basemap = relief)
    })
    
    # =========================================================================
    # Criterion B2: AOO
    # =========================================================================-
    fig_b2 <- reactive({
      req(b2_poly(), rasters())
      make_fig_b2(b2_poly(), rasters(), years(),
                  world = countries.sf, 
                  lakes = lakes.sf,
                  rivers = rivers.sf,
                  basemap = relief)
    })
    
    # =========================================================================
    # Figure preview thumbnails (PNG inline)
    # =========================================================================
    
    # render a ggplot to a temp PNG and show it, or show a placeholder
    make_preview <- function(fig_reactive, ns_id) {
      output[[ns_id]] <- renderUI({
        p <- tryCatch(fig_reactive(), error = function(e) NULL)
        if (is.null(p)) {
          tags$div(
            style = "height:160px; background:#f5f5f5; display:flex;
                     align-items:center; justify-content:center;
                     border:1px dashed #ccc; border-radius:4px; color:#aaa;
                     font-size:0.85em;",
            "Run assessment to preview figure"
          )
        } else {
          tmp <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp, p, width = 5, height = 3.5, dpi = 110)
          b64 <- base64enc::dataURI(file = tmp, mime = "image/png")
          tags$img(src = b64, style = "width:100%; border:1px solid #ddd;
                                        border-radius:4px; margin-top:6px;")
        }
      })
    }
    
    make_preview(fig_change,  "fig_change_preview")
    make_preview(fig_a,  "fig_a_preview")
    make_preview(fig_b1, "fig_b1_preview")
    make_preview(fig_b2, "fig_b2_preview")
    
    # =========================================================================
    # Figure download handlers
    # =========================================================================
    
    # create a downloadHandler for a ggplot reactive
    make_dl <- function(ns_id, fig_reactive, filename) {
      output[[ns_id]] <- downloadHandler(
        filename = function() {
          name_part <- if (!is.null(input$ecosystem_name) && nzchar(trimws(input$ecosystem_name))) {
            paste0(gsub(" ", "_", trimws(input$ecosystem_name)), "_")
          } else ""
          paste0(name_part, filename, "_", format(Sys.Date(), "%d-%b-%y"), ".png")
        },
        content = function(file) {
          p <- fig_reactive()
          ggplot2::ggsave(file, p, width = 8, height = 6, dpi = 300, bg = "white")
        }
      )
    }
    
    make_dl("dl_fig_change",  fig_change,  "distribution_change")
    make_dl("dl_fig_a",  fig_a,  "criterion_a_trend")
    make_dl("dl_fig_b1", fig_b1, "criterion_b1_eoo_map")
    make_dl("dl_fig_b2", fig_b2, "criterion_b2_aoo_map")
    
    
    # =========================================================================
    # Data download selection
    # =========================================================================
    
    # Update checkboxes whenever available datasets change
    observe({
      ds <- datasets()
      updateCheckboxGroupInput(
        session,
        "dataset_choice",
        choices = names(ds),
        selected = names(ds)
      )
    })
    
    # =========================================================================
    # Data download handlers
    # =========================================================================
    
    output$download_file <- downloadHandler(
      filename = function() {
        paste0(gsub(" ", "_", input$ecosystem_name),"_datasets", "_",format(Sys.Date(), "%d-%b-%y"), ".zip")
      },
      content = function(file) {
        # Get selected datasets
        selected <- input$dataset_choice
        ds <- datasets()  # reactive list of datasets
        
        if (length(selected) == 0) {
          showNotification("No datasets selected!", type = "error")
          return(NULL)
        }
        
        # Temp directory to store CSVs
        tmpdir <- tempdir()
        file_paths <- c()
        
        # Current date string
        date_str <- format(Sys.Date(), "%d%m%Y")
        
        # Helper to create safe filenames
        safe_filename <- function(name) {
          name <- gsub("[:/\\?<>\\*|]", "_", name, perl = TRUE) # Replace invalid characters with underscores
          name <- gsub("[\\s–—]+", "_", name) # Replace spaces or long dashes with underscore
          name <- gsub("[^A-Za-z0-9_\\-]", "", name) # Remove any remaining non-alphanumeric, underscore, dash
          name <- tolower(name)# Make lowercase 
          name
        }
        
        # Save each selected dataset as CSV
        for (nm in selected) {
          dat <- ds[[nm]]()
          if (!is.null(dat)) {
            # Determine type of dataset
            if (inherits(dat, "SpatVector")) {
              # Save SpatVector as GeoPackage (single file)
              gpkg_name <- paste0(
                safe_filename(input$ecosystem_name), "_",
                safe_filename(gsub("Criterion ", "", nm)), "_",
                date_str, ".gpkg"
              )
              gpkg_path <- file.path(tmpdir, gpkg_name)
              terra::writeVector(dat, gpkg_path, overwrite = TRUE)
              file_paths <- c(file_paths, gpkg_path)
              
            } else {
              csv_name <- paste0(
                safe_filename(input$ecosystem_name), "_",
                safe_filename(gsub("Criterion ", "", nm)), "_",   # remove "Criterion" from name
                date_str, ".csv"
              )
              csv_path <- file.path(tmpdir, csv_name)
              write.csv(dat, csv_path, row.names = FALSE)
              file_paths <- c(file_paths, csv_path)
            }
            
          }
        }
        
        # Zip all CSVs together
        zip::zip(zipfile = file, files = file_paths, mode = "cherry-pick")
        
      }
    )
    
    # Values other modules need
    return(list(
      ecosystem_name = reactive(input$ecosystem_name)
    ))
    
  })
  
}
