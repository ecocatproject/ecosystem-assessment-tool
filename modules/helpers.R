# modules/helpers.R

library(shiny)
library(plotly)
library(leaflet)
library(bslib)

# ----- Generic reusable card module -----
card_module_ui <- function(id, title, tooltip_text = NULL, output_type = c("plot", "table", "map"), height = 300) {
  ns <- NS(id)
  output_type <- match.arg(output_type)
  
  card(
    full_screen = TRUE,
    card_header(title, tooltip = tooltip(
      bs_icon("info-circle"),
      tooltip_text
    )),
    height = height,
    card_body(
      switch(
        output_type,
        plot = plotlyOutput(ns("output"), height = height),
        table = tableOutput(ns("output"), height = height),
        map = leafletOutput(ns("output"), height = height)
      ),
      class = "p-0"
    )
  )
}

card_module_server <- function(id, render_fn) {
  moduleServer(id, function(input, output, session) {
    output$output <- render_fn()
  })
}

# ----- Criterion B specialized card -----
criterion_b_card_ui <- function(id, title, button_label) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(title),
    card_body(
      leafletOutput(ns("map"), height = 300),
      class = "p-0",
      hr(),
      actionButton(ns("action"), button_label, width = "100%"),
      layout_column_wrap(height = 100, page_fillable(uiOutput(ns("boxes"))))
    )
  )
}

criterion_b_card_server <- function(id, map_fn, boxes_fn, on_click) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet(map_fn())
    output$boxes <- renderUI(boxes_fn())
    observeEvent(input$action, on_click())
  })
}

#------------------------------------------------------------------------------
# helper functions
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Helper to add a small info icon with tooltip
info_icon <- function(text) {
  tags$span(
    tooltip(
      bs_icon("info-circle"),
      text,
      placement = "bottom"
    )
  )
}

#------------------------------------------------------------------------------
# Helper function to determine status
get_box_status <- function(values) {
  vals <- unique(values)
  if ("CR" %in% vals) {
    list(text = "CR", color = "red")
  } else if ("EN" %in% vals) {
    list(text = "EN", color = "orange")
  } else if ("VU" %in% vals) {
    list(text = "VU", color = "yellow")
  } else {
    list(text = "LC", color = "lightgreen")
  }
}

#------------------------------------------------------------------------------
# Helper to render empty tables when can't create assessments
empty_assess_table <- function() {
  df_empty <- data.frame(
    Message = "Only 1 year of data - can't create assessment"
  )
  
  DT::datatable(
    df_empty,
    rownames = FALSE,
    colnames = "",
    class = "display nowrap",
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      dom = 't',
      ordering = FALSE
    )
  ) %>%
    DT::formatStyle(
      "Message",
      fontWeight = "bold",
      color = "black",
      backgroundColor = "white",
      textAlign = "center"
    )
}
