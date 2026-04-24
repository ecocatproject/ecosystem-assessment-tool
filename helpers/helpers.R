# modules/helpers.R

library(shiny)
library(plotly)
library(leaflet)
library(bslib)


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


# -----------------------------------------------------------------------------
make_b_value_boxes <- function(title, reactive_data, status_col,
                           data_col = NULL, data_col_1 = NULL, data_col_2 = NULL,
                           hover_text = NULL, type = c("single", "double")) {
  
  type <- match.arg(type)
  
  reactive({
    data <- reactive_data()
    req(data)  # make sure data exists
    
    # Determine the status (most recent year if relevant)
    if ("year" %in% colnames(data)) {
      max_year <- max(data$year, na.rm = TRUE)
      recent_data <- data[data$year == max_year, ]
    } else {
      recent_data <- data
    }
    
    status <- get_box_status(recent_data[[status_col]])
    
    # Map each status code to tooltip text
    status_tooltips <- list(
      "CR" = "Critically Endangered",
      "EN" = "Endangered",
      "VU" = "Vulnerable",
      "LC" = "Least Concern"
    )
    hover_status_text <- status_tooltips[[status$text]]
    
    # Create description HTML depending on type
    if (type == "single") {
      description_html <- paste0(
        paste0(
          recent_data$year, " Mapped distribution: ",
          format(round(recent_data[[data_col]], 0), big.mark = ",", trim = TRUE),
          " km<sup>2</sup> (", recent_data[[status_col]], ")"
        ),
        collapse = "<br>"
      )
    } else if (type == "double") {
      description_html <- paste0(
        paste0(
          recent_data$year, " Mapped distribution: Total occupied grid cells - ",
          format(recent_data[[data_col_1]], big.mark = ",", trim = TRUE),
          ", grid cells > 1% occupancy - ",
          format(recent_data[[data_col_2]], big.mark = ",", trim = TRUE),
          " (", recent_data[[status_col]], ")"
        ),
        collapse = "<br>"
      )
    }
    
    # Wrap status text in a small span and attach tooltip
    status_value <- tags$span(
      tooltip(
        tags$span(status$text, style = "font-weight: bold; font-size: 1.2em; display: inline-block;"),
        hover_status_text,
        placement = "right"
      )
    )
    
    # Create the value box
    value_box(
      title = div(style = "display: flex; align-items: center; justify-content: space-between;",
                  span(title, style = "display:inline-block;"),
                  info_icon(hover_text)
      ),
      value = status_value,
      p(HTML(description_html)),
      height = 200L,
      style = paste("background-color:", status$color, "; color: black;")
    )
  })
}

#------------------------------------------------------------------------------
# Helper: build descriptive lines for each row (handles optional 'scenario') - Criteria A 
# nrow(rv$area_prediction_data$type) == 2


format_assessment_lines <- function(data, status_col, data_col, method_col, method_selected, scenario_col = NULL, year_data = NULL) {
  
  method_names <- c(
    ARC = "Absolute rate of change",
    PRC = "Proportional rate of change"
  )
  
  # Replace method codes with descriptive names
  data <- data %>%
    dplyr::mutate(
      method_label = ifelse(method %in% names(method_names),
                            method_names[method],
                            method),
      perc_label = paste0(round(abs(.data[[data_col]]) * 100, 2), "%"),
      # direction = ifelse(.data[[data_col]] > 0, "decrease", "increase")
      direction = ifelse(.data[[data_col]] > 0, "increase", "decrease")
    )
  
  # only want PRC in output
  # data <- data %>%
  #   dplyr::filter(method == "PRC")
  data <- data %>% dplyr::filter(method == method_selected)
  
  #print(data)
  
  # # Only keep one row if scenario values are identical
  # if ("scenario" %in% names(data)) {
  #   # Only keep one row if there are exactly 2 rows and values are identical
  #   if (nrow(data) == 2 && all(data$perc_change[1] == data$perc_change[2])) {
  #     data <- data[1, , drop = FALSE]
  #     data$scenario <- NULL                     # Remove scenario column entirely
  #   }
  # }
  
  # Build the text per row
  if (!is.null(scenario_col) && scenario_col %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        line = paste0(
          method_selected, " (", .data[[scenario_col]], ") – ", # change method to method_label if want full name
          perc_label, " ", direction,
          " – ", .data[[status_col]])
      )
  } else {
    data <- data %>%
      dplyr::mutate(
        line = paste0(
          method_selected, " – ",                              # change method to method_label if want full name
          perc_label, " ", direction,
          " – ", .data[[status_col]])
      )
  }
  
  paste(data$line, collapse = "<br>")
}

#------------------------------------------------------------------------------
# Generalized helper to build a value_box for criteria A
make_value_box <- function(title, reactive_data, status_col, data_col, method_col, method_reactive,
                           scenario_col = NULL, hover_text = NULL, year_data = NULL) {
  reactive({
    data <- reactive_data()
    # data <- data %>% dplyr::filter(method == "PRC")
    
    # Use the current method() to filter
    current_method <- method_reactive()
    data <- data %>% dplyr::filter(.data[[method_col]] == current_method)
    
    status <- get_box_status(data[[status_col]]) # THIS MIGTH NEED TO CHANGE IF ONLY PRESENTING PRC DATA?

    # Map each status code to tooltip text
    status_tooltips <- list(
      "CR" = "Critically Endangered",
      "EN" = "Endangered",
      "VU" = "Vulnerable",
      "NT" = "Near Threatened",
      "LC" = "Least Concern"
    )

    hover_status_text <- status_tooltips[[status$text]]

    description_html <- format_assessment_lines(data, status_col, data_col, method_col, method_selected = current_method, scenario_col,  year_data = year_data)

    # Wrap status text in a small span and attach tooltip
    status_value <- tags$span(
      tooltip(
        tags$span(status$text, style = "font-weight: bold; font-size: 1.2em; display: inline-block;"),
        hover_status_text,
        placement = "right"
      )
    )

    value_box(
      # title = tagList(
      #   title,
      #   if (!is.null(hover_text)) info_icon(hover_text)  # add hover icon
      # ),
      title = div(style = "display: flex; align-items: center; justify-content: space-between;",
                  span(title, style = "display:inline-block;"),   # main title
                  info_icon(hover_text) # floating icon
      ),
      # value = status$text,
      value = status_value,
      p(HTML(description_html)),
      height = 200L,
      # theme = "bg-gradient-green-orange",
      style = paste("background-color:", status$color, "; color: black;")
    )
  })
}

# make_value_box <- function(title, reactive_data, status_col, data_col, method_col,
#                            scenario_col = NULL, hover_text = NULL, year_data = NULL,
#                            selected_method = reactive("PRC")) {
#   reactive({
#    # # req(selected_method())
#    #  # data <- reactive_data()
#    #  # req(data)
#    #  # selected_method_val <- selected_method()
#    #  # data <- data %>% dplyr::filter(method == selected_method_val)
#    #  # req(nrow(data) > 0)
#    #  data <- reactive_data()
#    #  if (is.null(data) || nrow(data) == 0) return(NULL)  # early exit if no data
#    #  
#    #  selected_method_val <- selected_method()
#    #  if (is.null(selected_method_val)) selected_method_val <- "PRC"
#    #  
#    #  data <- data %>% dplyr::filter(method == selected_method_val)
#    #  if (nrow(data) == 0) return(NULL)
#    #  
#    #  
#    #  status <- get_box_status(data[[status_col]])
#     
#     # -------------------------
#     # Grab data safely
#     data <- reactive_data()
#     if (is.null(data) || nrow(data) == 0) return(NULL)
#     
#     # -------------------------
#     # Determine selected method
#     selected_method_val <- selected_method()
#     if (is.null(selected_method_val) || !selected_method_val %in% unique(data[[method_col]])) {
#       selected_method_val <- unique(data[[method_col]])[1]  # fallback to first available
#     }
#     
#     # Filter for selected method
#     data <- data %>% dplyr::filter(.data[[method_col]] == selected_method_val)
#     if (nrow(data) == 0) return(NULL)
#     
#     # -------------------------
#     # Determine status
#     status <- get_box_status(data[[status_col]])
# 
#     # Map each status code to tooltip text
#     status_tooltips <- list(
#       "CR" = "Critically Endangered",
#       "EN" = "Endangered",
#       "VU" = "Vulnerable",
#       "NT" = "Near Threatened",
#       "LC" = "Least Concern"
#     )
# 
#     hover_status_text <- status_tooltips[[status$text]]
# 
#     description_html <- isolate({
#       req(year_data)
#       format_assessment_lines(data, status_col, data_col, method_col, scenario_col, year_data = year_data)
#     })
# 
#     status_value <- tags$span(
#       tooltip(
#         tags$span(status$text, style = "font-weight: bold; font-size: 1.2em; display: inline-block;"),
#         hover_status_text,
#         placement = "right"
#       )
#     )
# 
#     value_box(
#       title = div(style = "display: flex; align-items: center; justify-content: space-between;",
#                   span(title, style = "display:inline-block;"),
#                   info_icon(hover_text)
#       ),
#       value = status_value,
#       p(HTML(description_html)),
#       height = 200L,
#       style = paste("background-color:", status$color, "; color: black;")
#     )
#   })
# }
