# modules/about_module.R

# =============================================================================
# About Module
# =============================================================================

# This module renders html documents on: 
# 1) intro/guidance
# 2) methods
# 3) example data (itigi thicket treport summary)
# 4) RLE guidelines

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
about_module_server <- function(input, output, session) {}

shinyApp(about_module_ui, about_module_server)



