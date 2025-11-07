# modules/report.R

# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------
report_module_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Generate Report",
    page_fillable(uiOutput(ns("status_boxes"))),
    card(
      card_body(
        fluidRow(
          column(
            width = 6,
            textInput(ns("ecosystem_name"), "Ecosystem Name"),
            textInput(ns("organisation"), "Organisation(s)"),
            textInput(ns("author"), "Author"),
            textInput(ns("email"), "Email address"),
            downloadButton(ns("generate_report"), "Generate RLE Assessment Report")
          )
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------
report_module_server <- function(id,
                                 ecosystem_name, author, organisation, email,
                                 rasters,
                                 areas,
                                 years,
                                 roi_vec, 
                                 countries, 
                                 rainfall,
                                 area_change_data,
                                 area_prediction_data,
                                 mcp_data,
                                 mcp_poly,
                                 b2_data,
                                 aoo_sel_ids,
                                 aoo_all_ids,
                                 b2_poly) {
  moduleServer(id, function(input, output, session) {
    
    output$generate_report <- downloadHandler(
      filename = function() {
        paste0("RLE_Assessment_", gsub(" ", "_", input$ecosystem_name), ".html")
      },
      content = function(file) {
        
        # --- Show modal popup while rendering --------------------------------
        showModal(modalDialog(
          title = "Generating report...",
          tagList(
            div(
              style = "text-align:center;",
              tags$h4("Please wait while your RLE assessment report is being generated."),
              tags$p("This process usually takes approximately 10 seconds."),
              tags$p(tags$em("Rendering the HTML report...")),
              withSpinner(uiOutput(session$ns("dummy_spinner")), type = 5)
            )
          ),
          easyClose = FALSE,
          footer = NULL
        ))
        
        # --- Create temporary directory for large objects --------------------
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
        roi_vec_eval <- req(roi_vec())
        roi_vec_path <- file.path(tmpdir, "roi_vec.rds")
        saveRDS(roi_vec_eval, roi_vec_path)
        
        # --- Build params list for R Markdown --------------------------------
        params_list <- list(
          ecosystem_name = input$ecosystem_name,
          author = input$author,
          organisation = input$organisation,
          email = input$email,
          areas = req(areas()),
          years = req(years()),
          raster_files = raster_paths,
          roi_vec_file = roi_vec_path,
          countries = req(countries()),
          rainfall = req(rainfall()),
          area_change_data = req(area_change_data()),
          area_prediction_data = req(area_prediction_data()),
          mcp_data = req(mcp_data()),
          mcp_poly = req(mcp_poly()),
          b2_data = req(b2_data()),
          aoo_sel_ids = req(aoo_sel_ids()),
          aoo_all_ids = req(aoo_all_ids()),
          b2_poly = req(b2_poly())
        )
        
        message("Rendering report with parameters:")
        str(params_list)
        
        # --- Render the report ----------------------------------------------
        tryCatch({
          rmarkdown::render(
            input = "rle_report_template_html_testing.rmd",
            output_file = file,
            params = params_list,
            envir = new.env(parent = globalenv())
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
        })
        
        # Remove modal after successful render --------------------------------
        removeModal()
      }
    )
  })
}

 
# report_module_server <- function(id,
#                                  ecosystem_name,
#                                  rasters,
#                                  areas,
#                                  roi_vec,
#                                  roi_wgs)
#   {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     output$generate_report <- downloadHandler(
#       filename = function() {
#         paste0("RLE_Assessment_", gsub(" ", "_", ecosystem_name()), ".html")
#       },
#       content = function(file) {
#         template_file <- "rle_report_template_html_testing.rmd"  # your template file
#         # template_file <- system.file("rmarkdown", "rle_report_template_html.rmd", package = "EcoCAT") # this might be needed in deployed app
#         rmarkdown::render(
#           template_file,
#           output_file = file,
#           params = list(
#             ecosystem_name = ecosystem_name(),
#             rasters = rasters(),
#             years = areas()$Year,
#             area_table = areas(),
#             roi_vec = roi_vec,
#             roi_wgs = roi_wgs
# 
#           ),
#           envir = new.env(parent = globalenv())
#         )
#       }
#     )
#   })
# }
