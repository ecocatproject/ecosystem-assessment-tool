# modules/report.R

# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------
report_module_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Report/Data download",
    page_fillable(uiOutput(ns("status_boxes"))),
    card(
      card_body(
        fluidRow(
          column(
            width = 6,
            textInput(ns("ecosystem_name"),
                      tags$span("Ecosystem Name",tooltip(bs_icon("info-circle"), "Optional - Information provided here is used solely for creating the report and is not retained")),
                      placeholder = "Enter ecosystem name - Optional"),
            textInput(ns("organisation"),
                      tags$span("Organisation(s)",tooltip(bs_icon("info-circle"), "Optional - Information provided here is used solely for creating the report and is not retained")),
                      placeholder = "Enter organisation name(s) - Optional"),
            textInput(ns("author"),
                      tags$span("Author",tooltip(bs_icon("info-circle"), "Optional - Information provided here is used solely for creating the report and is not retained")),
                      placeholder = "Enter assessor name - Optional"),
            # textInput(ns("email"), "Email address"),
            selectInput(
              ns("report_format"),
              tags$span("Choose report format:",tooltip(bs_icon("info-circle"), "HTML format will include interactive maps and figures")),
              choices = c("HTML" = "html", "Word" = "docx") # "PDF" = "pdf"
            ),
            downloadButton(ns("generate_report"), "Generate RLE Assessment Report")
            ),
          column(
            width = 6,
            checkboxGroupInput(ns("dataset_choice"), "Choose datasets:", choices = NULL),
            downloadButton(ns("download_file"), "Download data")
        ),
        column(
          width = 6,
        script_export_ui("script_export")
        )
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------
report_module_server <- function(datasets,
                                 id,
                                 ecosystem_name, author, organisation,# email,
                                 rasters,
                                 areas,
                                 years,
                                # roi_vec,
                                 countries,
                                 rainfall,
                                temperature,
                                elevation,
                                 typology,
                                  area_change_data,
                                  area_prediction_data,
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
        paste0("RLE_Assessment_", name_part, format(Sys.Date(), "%d%m%Y"), ".", ext)
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
              withSpinner(uiOutput(session$ns("dummy_spinner")), type = 5)
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

        # Save evaluated ROI vector safely
        #roi_vec_eval <- req(roi_vec())
        #roi_vec_path <- file.path(tmpdir, "roi_vec.rds")
        #saveRDS(roi_vec_eval, roi_vec_path)

        # Build params list for R Markdown ------------------------------------
        params_list <- list(
          ecosystem_name = input$ecosystem_name,
          author = input$author,
          organisation = input$organisation,
          # email = input$email,
          areas = (areas()),
          years = (years()),
          raster_files = raster_paths,
         # roi_vec_file = roi_vec_path,
          countries = (countries()),
          rainfall = (rainfall()),
          temperature = (temperature()),
          elevation = (elevation()),
          typology = (typology()),
          area_change_data = (area_change_data()),
          area_prediction_data = (area_prediction_data()),
          mcp_data = (mcp_data()),
          mcp_poly = (mcp_poly()),
          b2_data = (b2_data()),
          aoo_sel_ids = (aoo_sel_ids()),
          aoo_all_ids = (aoo_all_ids()),
          b2_poly = (b2_poly())
        )

        str(params_list)

        # Choose output format  ----------------------------------------------
        fmt <- switch(input$report_format,
                      html = "html_document",
                      docx = "word_document")
        
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
        
        # # Remove modal after successful render --------------------------------
         removeModal()

      }
    )
    
    # data download section ---------------------------------------------------
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
    
    # download handler --------------------------------------------------------
    output$download_file <- downloadHandler(
      filename = function() {
        paste0(gsub(" ", "_", input$ecosystem_name),"_datasets", "_",format(Sys.Date(), "%d%m%Y"), ".zip")
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
        
        #unlink(tmpdir, recursive = TRUE)
        
      }
    )

  })
  
  # unlink(report_path, recursive = TRUE)

  
  
}
