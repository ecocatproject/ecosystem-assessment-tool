# modules/helpers.R

library(shiny)
library(plotly)
library(leaflet)
library(bslib)


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
  if ("CO" %in% vals) {
    list(text = "CO", color = "#000000")
  } else if ("CR" %in% vals) {
    list(text = "CR", color = "#FF0000")
  } else if ("EN" %in% vals) {
    list(text = "EN", color = "#FFA500")
  } else if ("VU" %in% vals) {
    list(text = "VU", color = "#FFFF00")
  } else if ("NT" %in% vals) {
    list(text = "NT", color = "#ADFF2F")
  } else {
    list(text = "LC", color = "#008000")
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

