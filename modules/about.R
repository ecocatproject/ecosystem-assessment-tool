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
          tags$p(
            "example dataset rmd/quarto doc will go here"
      ))),
      nav_panel(
        "RLE",
        card_body(
          tags$p(
            "RLE criterion doc here"
          ))),
      nav_panel(
        "About",
        card_body(
          tags$p(
            "about doc here"
          )))
    )))
}