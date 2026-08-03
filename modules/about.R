# modules/about_module.R

# =============================================================================
# About Module
# =============================================================================

# This module renders html documents on: 
# 1) intro/guidance
# 2) methods
# 3) example data (itigi thicket report summary)
# 4) RLE guidelines

# To create the html docs in the 1st instance, the www/documentation folder has
# quarto files (.QMD) that can be run to compile the the html docs

# ==============================================================================
# UI
# ==============================================================================
about_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel("About & Help",
            navset_card_pill(
              # ----------------------------------------------------------------
              # about 
              nav_panel(
                "About",
                tags$iframe(
                  src = "documentation/ecocat-about.html",
                  width = "100%",
                  height = "100%",
                  style = "border:none;"
                )
              ),
              # ----------------------------------------------------------------
              # about / guidance
              nav_panel(
                "Methods",
                tags$iframe(
                  src = "documentation/ecocat-methods.html",
                  width = "100%",
                  height = "100%",
                  style = "border:none;"
                )
              ),
              # ----------------------------------------------------------------
              # example data
              nav_panel(
                "Example Data",
                tags$iframe(
                  src = "documentation/itigi_sumbu_summary.html",
                  width = "100%",
                  height = "100%",
                  style = "border:none;"
                )
              ),
              # ----------------------------------------------------------------
              # RLE guidelines
              nav_panel(
                "RLE",
                tags$iframe(
                  src = "documentation/rle-assessment-criteria.html",
                  width = "100%",
                  height = "100%",
                  style = "border:none;"
                )
              )#,
              # # ----------------------------------------------------------------
              # # Glossary
              # nav_panel(
              #   "RLE",
              #   tags$iframe(
              #     src = "documentation/glossary.html",
              #     width = "100%",
              #     height = "100%",
              #     style = "border:none;"
              #   )
              # )
             
            ))
}

# ==============================================================================
# server
# ==============================================================================
about_module_server <- function(input, output, session) {
  
  
  # Reactive value to store cache-busting parameter
  cache_buster <- reactiveVal(Sys.time())
  
  observeEvent(input$reload, {
    cache_buster(Sys.time())  # Update timestamp to force reload
  })
  
  output$iframe_ui <- renderUI({
    tags$iframe(
      src = paste0("www/myfile.html?t=", as.numeric(cache_buster())),
      style = "width:100%; height:800px; border:none;"
    )
  })
  
}

shinyApp(about_module_ui, about_module_server)

# i've edited the qmd files that create the html files to remove author and date information.  However teh new files are not being rendered  in the app - why?
