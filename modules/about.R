# about_module.R
about_module_ui <- function(id) {
  nav_panel(
    "About",
    card(card_body("Created with R and R Shiny for RBG Kew EcoCAT project", br(), "2025"))
  )
}
