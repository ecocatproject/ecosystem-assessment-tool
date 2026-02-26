# app.R 

#########################################################
# -------------------------------------------------------
#               load modules + helpers
# -------------------------------------------------------
#########################################################

# -------------------------------------------------------
# load global
source("global.R")

# -------------------------------------------------------
# Load all modules
source("modules/classification_upload.R")
source("modules/description_eu.R")         # was description.R
source("modules/criterion_a.R")
source("modules/criterion_b.R")
source("modules/report.R")
source("modules/data_download.R")
source("modules/about.R")

# -------------------------------------------------------
# load  helpers
source("helpers/helpers.R")              # info_icon, get_box_status, format_assessment_lines, make_b_value_boxes, make_value_box, criterion_b_card_ui, criterion_b_card_server, empty_assess_table
source("helpers/analysis_functions.R")   # calc_area_change, predict_area_change, makeEOO.SpatRaster   

# -------------------------------------------------------
# description variable helpers currently loaded in description_eu.R

# -------------------------------------------------------
# load modals
source("helpers/modals.R")

# -------------------------------------------------------
# links menu
links_menu <- nav_menu(
  title = "Links",
  align = "right",
  nav_item(get_url),
  nav_item(rle_url)
)


#########################################################
# -------------------------------------------------------
#                       UI
# -------------------------------------------------------
#########################################################

ui <- page_navbar(
  id = "main_tab",
  title = tagList(
    tags$a(
      tags$img(src = "logo.png", height = "30px", style = "margin-right:10px;"),
      tags$img(src = rle_logo, height = "50px", style = "margin-right:10px;")
    ),
    "EcoCAT RLE Assessment Tool"
  ),
  theme = custom_theme,
  
  # data upload sidebar ------------------------------------------------------------------
  sidebar = classification_module_ui("upload"),
  
  # add modules --------------------------------------------------------------
  description_module_ui("description"),
  criterion_a_module_ui("criteria_a"),
  criterion_b_module_ui("criteria_b"),
  report_module_ui("report"),
  about_module_ui("about"),
  
  # Add custom CSS -----------------------------------------------------------
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))
)


#########################################################
# -------------------------------------------------------
#                       SERVER
# -------------------------------------------------------
#########################################################




server <- function(input, output, session) {
  
  # show instruction modal on start -------------------------------------------  
  showModal(start_up_modal)
  
  # classification upload module -----------------------------------------------
  upload_module <- classification_module_server("upload")
  
  # Description module --------------------------------------------------------
  description_module <- description_module_server("description",
                                                  upload_module = upload_module,
                                                  process_btn = upload_module$process,
                                                  tifs_input = upload_module$tifs,
                                                  year_inputs = upload_module$years,
                                                  countries = countries)
  
  # Run Criterion A module ----------------------------------------------------
  main_tab <- reactive(input$main_tab)  # or whatever your navbar id is
  criterion_a_module <- criterion_A_module_server("criteria_a",
                                                  upload_module = upload_module,
                                                  main_tab = main_tab)
  
  
  # Run Criterion B module ----------------------------------------------------
  main_tab <- reactive(input$main_tab)  # or whatever your navbar id is
  criterion_b_module <- criterion_b_module_server(id = "criteria_b",
                                                  rasters = upload_module$rasters,
                                                  year_inputs = upload_module$years, # this becomes upload_module$year_inputs?
                                                  rasters_original = upload_module$rasters_original,
                                                  main_tab = main_tab
  )
  
  # Data download details ------------------------------------------------------
  # Firstly provide list of potential datasets for download 
  available_datasets <- reactive({
    all <- list(
      "Criterion A - area change data" = criterion_a_module$area_change_data,
      "Criterion A - area prediction data" = criterion_a_module$area_prediction_data,
      "Criterion B - EOO data" = criterion_b_module$mcp_table,
      "Criterion B - EOO spatial data" = criterion_b_module$mcp_vectors,
      "Criterion B - AOO data" = criterion_b_module$b2_table,
      "Criterion B - AOO spatial data" = criterion_b_module$b2_poly
    )
    
    # Keep only those whose reactive returns *non-null*
    Filter(function(r) {
      !is.null(r())  # run reactive to check if data exists
    }, all)
  })
  
  # Run report module ---------------------------------------------------------
  # report module server
  report_module_server("report",
                       datasets = available_datasets,
                       ecosystem_name = input$ecosystem_name,
                       author = input$author,
                       organisation = input$organisation,
                       email = input$email,
                       rasters = upload_module$rasters,
                       areas = upload_module$areas,
                       years = upload_module$years,
                       countries = description_module$countries,
                       rainfall = description_module$rainfall,
                       temperature = description_module$temperature,
                       elevation = description_module$elevation,
                       typology = description_module$typology,
                       area_change_data = criterion_a_module$area_change_data,
                       area_prediction_data = criterion_a_module$area_prediction_data,
                       mcp_data = criterion_b_module$mcp_table,
                       mcp_poly = criterion_b_module$mcp_poly,
                       b2_data = criterion_b_module$b2_table,
                       aoo_sel_ids = criterion_b_module$aoo_sel_ids,
                       aoo_all_ids = criterion_b_module$aoo_all_ids,
                       b2_poly = criterion_b_module$b2_poly
  )
  
  
  # Run about module --------------------------------------------------------
  # about_module_server("about")
  
  
  
  
}


# run the app -----------------------------------------------------------------
shinyApp(ui, server)


