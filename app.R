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
source("modules/data.R")
source("modules/data_description.R")
source("modules/description_v2.R") # was description.R
# source("modules/classification_files.R") # ideally have separate for rasters, but can't get the maps to marry
source("modules/criterion_a.R")
source("modules/criterion_b.R")
source("modules/report.R")
source("modules/about.R")

# -------------------------------------------------------
# load all helpers
source("helpers/helpers.R")
source("helpers/analysis_functions.R")

# -------------------------------------------------------
# theme
custom_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#669900",
  bg = "#f5f5f5",
  fg = "#333333",
  base_font = font_google("Roboto")
)

# -------------------------------------------------------
# links menu
links_menu <- nav_menu(
  title = "Links",
  align = "right",
  nav_item(link_get),
  nav_item(link_rle)
)

#########################################################
# -------------------------------------------------------
#                       UI
# -------------------------------------------------------
#########################################################

ui <- page_navbar(
  title = tagList(
    tags$a(
      tags$img(src = "logo.png", height = "30px", style = "margin-right:10px;"),
      tags$img(src = rle_logo, height = "50px", style = "margin-right:10px;")
    ),
    "EcoCAT RLE Assessment"
    ),
  theme = custom_theme,
  
  # sidebar ------------------------------------------------------------------
  sidebar = sidebar(
    width = 300,
    fileInput("roi", "Upload Region of Interest (Shapefile components)",
              multiple = TRUE,
              accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
    actionButton("load_roi", "Load ROI map"),
    hr(),
    fileInput("tifs", "Upload TIF file(s)", multiple = TRUE, accept = ".tif"),
    uiOutput("year_inputs"),
    actionButton("process", "Process ecosystem maps"),
    hr()
  ),
  
  # add modules --------------------------------------------------------------
  description_module_ui("description"),
  criterion_a_module_ui("criteria_a"),
  criterion_b_module_ui("criteria_b"),
  report_module_ui("report"),
  about_module_ui("about"),
  
  # add links ----------------------------------------------------------------
  nav_spacer(),
  links_menu,
  
  # Add custom CSS -----------------------------------------------------------
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))
)


#########################################################
# -------------------------------------------------------
#                       SERVER
# -------------------------------------------------------
#########################################################

server <- function(input, output, session) {
  
  # data module (for raster) --------------------------------------------------
  data_module <- data_module_server("data")

  # Description module --------------------------------------------------------
  description_module_server(
    "description",
    roi_input = reactive({ input$roi }),
    process_roi_btn = reactive({ input$load_roi }),
    data_module = data_module,
    tifs_input = reactive({ input$tifs }),
    year_inputs = reactive({ sapply(seq_along(input$tifs$name), function(i) input[[paste0("year_", i)]]) }),
    process_btn = reactive({ input$process }),  # <- THIS IS REQUIRED for the rasters
    countries = countries
    )

  # Run Criterion A module ----------------------------------------------------
  criterion_A_module_server("criteria_a", data_module = data_module)
  
  # Run Criterion B module ----------------------------------------------------
  criterion_b_module_server(id = "criteria_b",
    rasters = data_module$rasters,
    year_inputs = data_module$years,
    rasters_original = data_module$rasters_original,
  )

 # # # Run report module -------------------------------------------------------
 # report_module_server("report",
 #                      ecosystem_name = reactive(input$ecosystem_name),
 #                      rasters = rasters,
 #                      areas = areas,
 #                      roi_countries = roi_countries,
 #                      chirps_summary = chirps_summary,
 #                      rain_data = rain_data,
 #                      temp_data = temp_data,
 #                      elevation_summary = elevation_summary,
 #                      roi_vec = roi_vec,
 #                      roi_wgs = roi_wgs,
 #                      typology_table = typology_table,
 #                      area_change_data = area_change_data,
 #                      area_prediction_data = area_prediction_data,
 #                      mcp_table = mcp_table,
 #                      mcp_poly = mcp_poly,
 #                      b2_table = b2_table,
 #                      aoo_sel_ids = aoo_sel_ids,
 #                      b2_poly = b2_poly)
  
  # # Run about module --------------------------------------------------------
  # criterion_A_module_server("about")
  
    # Dynamic year inputs -----------------------------------------------------
  output$year_inputs <- renderUI({
    req(input$tifs)
    lapply(seq_along(input$tifs$name), function(i) {
      selectInput(paste0("year_", i), paste("Year for", input$tifs$name[i]), choices = seq(1985, 2025, by = 1), selected = 2025
      )
    })
  })
  
}

shinyApp(ui, server)



