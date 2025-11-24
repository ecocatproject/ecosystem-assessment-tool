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
# source("modules/data_description.R")
source("modules/description_v2.R") # was description.R

# source("modules/classification_processing.R") # this now separates up the raster part from description_v2.R and joins with code in data.R 
# CURRENTLY CAN'T SYNC THE MAPS WELL - IF ONLY HAVE TIFS THE MAP IS NOT PLOTTED

# source("modules/classification_files.R") # ideally have separate for rasters, but can't get the maps to marry
source("modules/criterion_a.R")
source("modules/criterion_b.R")
source("modules/drivers_of_change.R")
source("modules/report.R")
source("modules/data_download.R")
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
  drivers_module_ui("drivers_of_change"),
  report_module_ui("report"),
  # download_module_ui("download"),
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
  
  # # classification processing module ------------------------------------------
  # classification_module <- raster_module_server(
  #   "raster",
  #   tifs_input = tifs_input,     # from fileInput or reactive in app
  #   year_inputs = year_inputs,   # from your UI year input
  #   process_btn = process_btn    # from your "Process" button
  # )

  # Description module --------------------------------------------------------
  description_module <- description_module_server(
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
  criterion_a_module <- criterion_A_module_server("criteria_a",
                                                        data_module = data_module)
  
  # Run Criterion B module ----------------------------------------------------
  main_tab <- reactive(input$main_nav)  # or whatever your navbar id is
  criterion_b_module <- criterion_b_module_server(id = "criteria_b",
    rasters = data_module$rasters,
    year_inputs = data_module$years,
    rasters_original = data_module$rasters_original,
    main_tab = main_tab
  )
  
  # # Run drivers of change module ----------------------------------------------------
  drivers_module <- drivers_module_server(id = "drivers_of_change",
                                                  rasters = data_module$rasters,
                                                  year_inputs = data_module$years,
                                                  rasters_original = data_module$rasters_original
                                          )

  # # Run report module -------------------------------------------------------
  # report_module_server(
  #   "report",
  #                       ecosystem_name = input$ecosystem_name,
  #                       author = input$author,
  #                       organisation = input$organisation,
  #                       email = input$email,
  #   rasters = data_module$rasters,
  #   areas = data_module$areas,
  #   years = data_module$years,
  #   roi_vec = description_module$roi,
  #   # roi_wgs = description_module$roi_wgs,
  #   countries = description_module$countries,
  #   rainfall = description_module$rainfall
  # )
  
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
  
   report_module_server("report",
                        datasets = available_datasets,
                       ecosystem_name = input$ecosystem_name,
                       author = input$author,
                       organisation = input$organisation,
                       email = input$email,
                       rasters = data_module$rasters,
                       areas = data_module$areas,
                       years = data_module$years,
                       roi_vec = description_module$roi, #  roi_vec = reactive(description_module$rv$roi)  # wrap in reactive() if not already
                       countries = description_module$countries,
                       rainfall = description_module$rainfall,
                       # chirps_summary = chirps_summary,
                       # rain_data = rain_data,
                       # temp_data = temp_data,
                       # elevation_summary = elevation_summary,
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

  # # Run download module --------------------------------------------------------
  # download_module_server(
  #   "download",
  #   datasets = available_datasets
  # )
  
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


# run the app -----------------------------------------------------------------
shinyApp(ui, server)


