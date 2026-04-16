# modules/about_module.R

# =============================================================================
# About Module
# =============================================================================

# This module renders html documetns on 
# 1) example data (itigi thicket report summary)
# 2) RLE guidelines
# 3) tool guidance

# ==============================================================================
# UI
# ==============================================================================
about_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel("About/Help",
            navset_card_pill(
              # ----------------------------------------------------------------
              # example data
              nav_panel(
                "Example Data",
                tags$iframe(
                  src = "documentation/itigi_sumbu_summary.html",
                  width = "100%",
                  height = "900px",
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
                  height = "900px",
                  style = "border:none;"
                )
              ),
              # ----------------------------------------------------------------
              # about / guidance
              nav_panel(
                "About",
                tags$iframe(
                  src = "documentation/ecocat-guidance.html",
                  width = "100%",
                  height = "900px",
                  style = "border:none;"
                )
              )
            ))
}

# ==============================================================================
# server
# ==============================================================================
about_module_server <- function(input, output, session) {}

shinyApp(about_module_ui, about_module_server)