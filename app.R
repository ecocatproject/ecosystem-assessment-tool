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
    # fileInput("roi", "Upload Region of Interest (Shapefile components)",
    #           multiple = TRUE,
    #           accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
    # actionButton("load_roi", "Load ROI map"),
    # hr(),
    fileInput("tifs", "Upload Ecosystem classification file(s)", multiple = TRUE, accept = ".tif"),
    uiOutput("year_inputs"),
    actionButton("process", "Process ecosystem maps"),
    hr()
  ),
  
  # add modules --------------------------------------------------------------
  description_module_ui("description"),
  criterion_a_module_ui("criteria_a"),
  criterion_b_module_ui("criteria_b"),
  # drivers_module_ui("drivers_of_change"),
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
  
# show instruction modal on start -------------------------------------------  
  showModal(
    modalDialog(
      title = div(
        style = "font-size: 20px; font-weight: bold; text-align:center;",
        "Welcome to the EcoCAT Tool"
      ),
      
      tagList(
        tags$div(
          style = "padding: 10px;",
          
          tags$p(
            style = "font-size: 16px;",
            "This tool assists with IUCN Red List of Ecosystems assessments."
          ),
          
          tags$hr(),
          
          tags$h4("To get started:", style = "text-align:left;"),
          
          tags$ul(
            style = "font-size: 15px;",
            tags$li("Use the sidebar to upload your own ecosystem map(s) (ideally in a raster format)."),
            tags$li("Once loaded, explore summary statistics and spatial outputs."),
            tags$li("Navigate through the tabs to explore different parts of the assessment."),
            tags$li("Criteria A is generated automatically. Criteria B requires the user to select the relevent buttons."),
            tags$li("A report and all the data can be downloaded in the 'Report/Download' tab"),
          ),
          
          tags$hr(),
          
          tags$p(
            style = "font-size: 14px; font-style: italic; color: #666;",
            "Tip: Hover over info icons throughout the app for additional guidance."
          )
        )
      ),
      
      easyClose = FALSE,
      footer = modalButton("Start")
    )
  )
  
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

# Run report module -------------------------------------------------------
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

  # report module server
   report_module_server("report",
                        datasets = available_datasets,
                       ecosystem_name = input$ecosystem_name,
                       author = input$author,
                       organisation = input$organisation,
                       email = input$email,
                       rasters = data_module$rasters,
                       areas = data_module$areas,
                       years = data_module$years,
                      # roi_vec = description_module$roi, #  roi_vec = reactive(description_module$rv$roi)  # wrap in reactive() if not already
                       countries = description_module$countries,
                       rainfall = description_module$rainfall,
                       # chirps_summary = chirps_summary,
                       # temp_data = temp_data,
                       # elevation_summary = elevation_summary,
                        #typology = description_module$typology,
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
  # about_module_server("about")
  
    # Dynamic year inputs -----------------------------------------------------
   extract_year <- function(filename) {
     # Look for a 4-digit number that looks like a year
     m <- regmatches(filename, regexpr("(19|20)\\d{2}", filename))
     if (length(m) == 0) return(NULL)
     as.numeric(m)
   }
   
   extract_year <- function(filename) {
     m <- regexpr("(?<!\\d)(19|20)\\d{2}(?!\\d)", filename, perl = TRUE)
     if (m[1] == -1) return(NULL)
     as.numeric(regmatches(filename, m))
   }
   
   
  # output$year_inputs <- renderUI({
  #   req(input$tifs)
  #   
  #   fnames <- input$tifs$name
  #   auto_years <- sapply(fnames, extract_year)
  #   
  #   # --- Create a list to show inside modal ---
  #   year_messages <- mapply(
  #     function(f, y) {
  #       if (!is.null(y)) paste(f, "→", y) else paste(f, "→ no year detected")
  #     },
  #     fnames, auto_years
  #   )
  #   
  #   # --- Show modal ONCE per upload ---
  #   showModal(
  #     modalDialog(
  #       title = "Auto-Detected Years",
  #       easyClose = TRUE,
  #       footer = modalButton("OK"),
  #       paste("The following auto-detected year assignments were found:"),
  #       tags$ul(
  #         lapply(year_messages, function(msg) tags$li(msg))
  #       )
  #     )
  #   )
  #       
  #       
  #   # --- Build the year selection UI ---
  #   lapply(seq_along(fnames), function(i) {
  #     selectInput(
  #       paste0("year_", i),
  #       paste("Year for", fnames[i]),
  #       choices = seq(1900, 2025, by = 1),
  #       selected = if (!is.null(auto_years[i]) &&
  #                      auto_years[i] %in% 1900:2025) auto_years[i] else 2025
  #     )
  #   })
  # })
  
   output$year_inputs <- renderUI({
     req(input$tifs)
     
     fnames <- input$tifs$name
     auto_years <- sapply(fnames, extract_year)
     
     # Build HTML rows for modal
     file_rows <- mapply(function(f, y) {
       
       if (is.null(y)) {
         # RED for missing year
         htmltools::tags$tr(
           htmltools::tags$td(f),
           htmltools::tags$td(style = "color:#c0392b; font-weight:bold;", "No year detected")
         )
       } else {
         # GREEN for detected year
         htmltools::tags$tr(
           htmltools::tags$td(f),
           htmltools::tags$td(style = "color:#27ae60; font-weight:bold;", y)
         )
       }
       
     }, fnames, auto_years, SIMPLIFY = FALSE)
     
     # Show modal
     showModal(
       modalDialog(
         title = htmltools::tags$div(
           style="font-size:20px; font-weight:bold; color:#2c3e50;",
           "Auto-Detected Years"
         ),
         easyClose = TRUE,
         footer = modalButton("OK"),
         
         # Modal body
         htmltools::tags$div(
           style="margin-bottom:10px; color:#34495e;",
           "Years were automatically extracted from filenames. You may adjust them in the selection boxes below."
         ),
         
         # Styled table
         htmltools::tags$table(
           style="
          width:100%;
          border-collapse: collapse;
          margin-top:10px;
          font-size:14px;
        ",
           htmltools::tags$thead(
             htmltools::tags$tr(
               htmltools::tags$th("Filename", style="text-align:left; border-bottom:2px solid #bdc3c7; padding:6px;"),
               htmltools::tags$th("Detected Year", style="text-align:left; border-bottom:2px solid #bdc3c7; padding:6px;")
             )
           ),
           htmltools::tags$tbody(
             lapply(file_rows, identity)
           )
         )
       )
     )
     
     # Generate year dropdowns
     lapply(seq_along(fnames), function(i) {
       selectInput(
         paste0("year_", i),
         paste("Year for", fnames[i]),
         choices = seq(1985, 2025, by = 1),
         selected = if (!is.null(auto_years[i]) &&
                        auto_years[i] %in% 1985:2025) auto_years[i] else 2025
       )
     })
   })
   
}


# run the app -----------------------------------------------------------------
shinyApp(ui, server)






