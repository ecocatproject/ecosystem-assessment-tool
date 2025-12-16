# helper functions



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
    list(text = "LC", color = "darkgreen")
  }
}

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
