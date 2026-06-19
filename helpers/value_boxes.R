# helpers/value_boxes.R

# =============================================================================
# value boxes
# =============================================================================
# This script creates provides functiosn to create value_boxes used throughout the app:

# 1. criterion a
# 2. criterion b

# =============================================================================


# =============================================================================
# 1.  function to create info for criterion a boxes
# =============================================================================

#------------------------------------------------------------------------------
# Helper: build descriptive lines for each row (handles optional 'scenario') - Criteria A 

format_assessment_lines <- function(data, status_col, data_col, method_col, method_selected, scenario_col = NULL,
                                    year_data = NULL, n_obs_years = NULL,
                                    year_start_col = NULL, year_end_col = NULL) {
  
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
      direction = ifelse(.data[[data_col]] > 0, "increase", "decrease")
    )
  

  data <- data %>% dplyr::filter(method == method_selected)

  scenario_labels <- c("recent" = "Near-term", "median" = "Long-term")
  

  # Build the text per row
  if (!is.null(scenario_col) && scenario_col %in% names(data)) {
    
    # If all scenarios produce the same perc_change, collapse to one line
    # without the scenario label (happens with only 2 years of data)
    # all_same <- length(unique(round(data[[data_col]], 6))) == 1
    # Collapse to one line when only 2 observed years
    all_same <- !is.null(n_obs_years) && n_obs_years <= 2
    
    # Build year range suffix per row if columns provided
    has_year_range <- !is.null(year_start_col) && 
      all(c(year_start_col, year_end_col) %in% names(data))
    
    if (all_same) {
      data <- data |> dplyr::slice(1) |>
        dplyr::mutate(line = paste0(
          method_selected, " \u2013 ", perc_label, " ", direction,
          " \u2013 ", .data[[status_col]],
          if (has_year_range) 
            paste0(
              " <span style='color:#555;'>(",
              .data[[year_start_col]], "\u2013", .data[[year_end_col]],
              ")</span>"
            )
          else ""
        ))
    } else {
      data <- data |>
        dplyr::mutate(
          scenario_display = ifelse(
            .data[[scenario_col]] %in% names(scenario_labels),
            scenario_labels[.data[[scenario_col]]],
            .data[[scenario_col]]
          ),
          line = paste0(
            method_selected, " (", scenario_display, ") \u2013 ",
            perc_label, " ", direction,
            if (has_year_range)       paste0(
              " <span style='color:#555;'>(",
              .data[[year_start_col]], "\u2013", .data[[year_end_col]],
              ")</span>"
            ) else ""
          )
        )
    }
  } else {
    data <- data %>%
      dplyr::mutate(
        line = paste0(
          scenario_display," ", method_selected," - ",
          perc_label, " ", direction, " - ", .data[[status_col]])
      )
  }
  
  paste(data$line, collapse = "<br>")
}


# =============================================================================
# 2.  criterion a value boxes
# =============================================================================
# Generalized helper to build a value_box for criteria A

make_value_box <- function(title, reactive_data, status_col, data_col, method_col, method_reactive,
                           scenario_col = NULL, hover_text = NULL, year_data = NULL,
                           date_range = NULL, n_obs_years = NULL, year_start_col = NULL, year_end_col = NULL) {
  reactive({
    data <- reactive_data()
    current_method <- method_reactive()
    data <- data %>% dplyr::filter(.data[[method_col]] == current_method)
    
    status <- get_box_status(data[[status_col]])
    
    status_tooltips <- list(
      "CR" = "Critically Endangered",
      "EN" = "Endangered",
      "VU" = "Vulnerable",
      "NT" = "Near Threatened",
      "LC" = "Least Concern"
    )
    hover_status_text <- status_tooltips[[status$text]]
    
    description_html <- format_assessment_lines(
      data, status_col, data_col, method_col,
      method_selected = current_method, scenario_col,
      year_data = year_data,  n_obs_years = n_obs_years,
      year_start_col = year_start_col, year_end_col = year_end_col
    )
    
    status_value <- tags$span(
      tooltip(
        tags$span(status$text,
                  style = "font-weight: bold; font-size: 1.2em; display: inline-block;"),
        hover_status_text,
        placement = "right"
      )
    )
    
    # Resolve date_range: may be a plain string or a function of the (filtered) data
    date_range_text <- if (is.function(date_range)) date_range(data) else date_range
    
    value_box(
      title = div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        # Left: title + date on same line with gap
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          span(title, style = "font-weight: bold;"),
          if (!is.null(date_range_text)) {
            span(date_range_text, style = "font-size: 0.85em; color: #555; font-weight: normal;")
          }
        ),
        # Right: info icon pinned
        div(style = "flex-shrink: 0;", info_icon(hover_text))
      ),
      value  = status_value,
      p(HTML(description_html)),
      min_height = "150px",
      style  = paste("background-color:", status$color, "; color: black;")
    )
  })
}


# =============================================================================
# 3.  criterion b value boxes
# =============================================================================
make_b_value_boxes <- function(title, reactive_data, status_col,
                               data_col = NULL, data_col_1 = NULL, data_col_2 = NULL,
                               hover_text = NULL, type = c("b1", "b2")) {
  
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
    if (type == "b1") {
      description_html <- paste0(
        paste0(
          recent_data$year, " Extent of Occurrence (EOO): ",
          format(round(recent_data[[data_col]], 0), big.mark = ",", trim = TRUE),
          " km<sup>2</sup> (", recent_data[[status_col]], ")"
        ),
        collapse = "<br>"
      )
    } else if (type == "b2") {
      description_html <- paste0(
        paste0(
          recent_data$year, " Area of Occupancy (AOO): Total occupied grid cells -  ",
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
      min_height = "150px",
      style = paste("background-color:", status$color, "; color: black;")
    )
  })
}


