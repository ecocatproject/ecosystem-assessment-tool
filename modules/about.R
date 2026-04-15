# about_module.R
# about_module_ui <- function(id) {
#   nav_panel(
#     "About",
#     card(card_body("Created with R and R Shiny for RBG Kew EcoCAT project", br(), "2025"))
#   )
# }



about_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "About/Help",
    page_fluid(navset_card_tab(
      # height = 450,
      full_screen = TRUE,
      nav_panel(
        "Example Data",
        card_body(
          fluidPage(
            tags$iframe(
              src = "documentation/itigi_sumbu_summary.html",
              width = "100%",
              height = "900px",
              style = "border:none;"
            ))
      )),
      nav_panel(
        "RLE",
        card_body(
          # tags$p(
          #   "RLE criterion doc here"
          # )
          fluidPage(
            tags$iframe(
              src = "documentation/rle-assessment-criteria.html",
              width = "100%",
              height = "900px",
              style = "border:none;"
            ))
          )),
      nav_panel(
        "About",
        card_body(
          tags$p(
            "about doc here"
          )
          ))
    )))
}

about_module_server <- function(input, output, session) {}

shinyApp(about_module_ui, about_module_server)