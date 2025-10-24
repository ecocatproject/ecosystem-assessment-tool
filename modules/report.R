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
            textInput(ns("report_organisation"), "Organisation(s)"),
            textInput(ns("report_author"), "Author"),
            textInput(ns("report_author_email"), "Email address"),
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
                                 ecosystem_name,
                                 rasters,
                                 areas,
                                 roi_countries,
                                 chirps_summary,
                                 rain_data,
                                 temp_data,
                                 elevation_summary,
                                 roi_vec,
                                 roi_wgs,
                                 typology_table,
                                 area_change_data,
                                 area_prediction_data,
                                 mcp_table,
                                 mcp_poly,
                                 b2_table,
                                 aoo_sel_ids,
                                 b2_poly) {
  
  moduleServer(id, function(input, output, session) {
    
    output$generate_report <- downloadHandler(
      filename = function() {
        paste0("RLE_Assessment_", gsub(" ", "_", ecosystem_name()), ".html")
      },
      content = function(file) {
        template_file <- "rle_report_template_html.rmd"  # your template file
        # template_file <- system.file("rmarkdown", "rle_report_template_html.rmd", package = "EcoCAT") # this might be needed in deployed app
        rmarkdown::render(
          template_file,
          output_file = file,
          params = list(
            ecosystem_name = ecosystem_name(),
            rasters = rasters(),
            years = areas()$Year,
            area_table = areas(),
            country_table = roi_countries(),
            chirps_summary = chirps_summary(),
            rain_data = rain_data(),
            temp_data = temp_data(),
            elevation_summary = elevation_summary(),
            roi_vec = roi_vec,
            roi_wgs = roi_wgs,
            typology_data = typology_table(),
            area_change_data = area_change_data(),
            area_prediction_data = area_prediction_data(),
            b1_data = mcp_table(),
            b1_maps = mcp_poly(),
            b2_data = b2_table(),
            b2_aoo_sel_ids = aoo_sel_ids(),
            b2_poly = b2_poly()
          ),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
