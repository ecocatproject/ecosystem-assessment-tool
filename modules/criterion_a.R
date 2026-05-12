# modules/criterion_A_module.R

# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------
criterion_a_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Criterion A",
    
    # Area change table (stats etc) -------------------------------------------
    card(
      height = 150,
      fill = FALSE,
      full_screen = TRUE,
      card_header(tags$strong("Ecosystem area and change data"),
                  tooltip(
                    bs_icon("info-circle"),
                    tags$p("To view the full table, click the expand button in the bottom‑right corner.",  style = "text-align: left"),
                    tags$p("The data can be downloaded from the ", tags$b("'Report/Data download'"), "tab",  style = "text-align: left")
                  )),
      uiOutput(ns("area_change_table"))
    ),
    
    # Graph of temporal change ------------------------------------------------ 
    card(
      full_screen = TRUE,
      fill= FALSE,
      card_header(
        tags$span(
          tags$strong("Ecosystem area change trends"),
          tooltip(
            bs_icon("info-circle"),
            tags$p(tags$b("Observed"), "ecosystem areas (solid line) and ",
                   tags$b("extrapolated"), "areas (coloured dashed lines) across the timeframes relevant to the Red List of Ecosystems assessment.
                  Extrapolated rates of change differ by method.", style = "text-align: left"),
            tags$p(tags$b("Near-term trend:"), "continues the nearest observed rate of change (proportional or absolute).", style = "text-align: left"),
            tags$p(tags$b("Long-term trend:"), "applies the mean rate of change calculated across the entire observation period.", style = "text-align: left"),
            tags$p(tags$b("PRC:"), "Proportional rate of change.", style = "text-align: left"),
            tags$p(tags$b("ARC:"), "Absolute rate of change.", style = "text-align: left")
          )
        ),
        uiOutput(ns("ribbon_toggle_ui"))
      ),
      plotlyOutput(ns("area_change_graph")),
      class = "p-0"
    ),
    
    
    # button ----------------------------------------------------------------
    layout_column_wrap(
      width = "100%",
      fill = FALSE,
      class = "no-gap",
      actionButton(
        ns("switch_method"),
        label = HTML('Switch between PRC and ARC assessments results here'),
        width = "100%",
        style = "background-color: white;color: black; border: 3px solid #696969;")
    ),
    
    # Subcriterion results ----------------------------------------------------
    layout_column_wrap(
      page_fillable(max_height = "50px",
                    uiOutput(ns("criterion_boxes_a"))  # placeholder for the dynamic box
      )
    )
  )
}

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------
criterion_A_module_server <- function(id, upload_module, main_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ----------------------------------------------------------------------
    # modal to provide some user info
    modal_a_shown <- reactiveVal(FALSE) # prevent the modal from popping up multiple times per session
    
    # model pop up on tab opening 
    observeEvent(main_tab(), {
      if (identical(main_tab(), "Criterion A")) { # Only fire when Criterion A is selected
        #if (!modal_a_shown()) {
        showModal(a_quick_modal)
        modal_a_shown(TRUE)
        #}
      }
    }, ignoreInit = TRUE)
    
    #-----------------------------------------------------------------------
    # reset values when new rasters uploaded
    observeEvent(upload_module$rasters(), {
      rv$area_change_data      <- NULL
      rv$area_prediction_data  <- NULL
      output$criterion_boxes_a <- renderUI({
        div(class = "text-muted", "Upload at least two rasters to calculate Criterion A")
      })
    }, ignoreInit = TRUE)
    
    
    # ----------------------------------------------------------------------
    # Reactive values container to store data
    rv <- reactiveValues(area_change_data = NULL,
                         area_prediction_data = NULL)
    
    
    # ----------------------------------------------------------------------
    # calculate area change data for table
    observeEvent(upload_module$rasters(), {
      rv$area_change_data <- calc_area_change(upload_module$areas())
    })
    
    
    # ----------------------------------------------------------------------
    # reactive value to store currently selected method
    method <- reactiveVal("PRC")
    
    # Button changes method
    observeEvent(input$switch_method, {
      method(ifelse(method() == "PRC", "ARC", "PRC"))
    })
    
    # update button label whenever the method changes
    observe({
      if (method() == "PRC") {
        updateActionButton(
          session, "switch_method",
          label = HTML(
            'Assessment outcome based on the  
         <b><span style="color:#D55E00;">Proportional Rate of Change (PRC)</span></b> 
         - Click here for 
         <b><span style="color:#56B4E9;">Absolute Rate of Change (ARC)</span></b>'
          )
        )
      } else {
        updateActionButton(
          session, "switch_method",
          label = HTML(
            'Assessment outcome based on the 
         <b><span style="color:#56B4E9;">Absolute Rate of Change (ARC)</span></b> 
         - Click here to for 
         <b><span style="color:#D55E00;">Proportional Rate of Change (PRC)</span></b>'
          )
        )
      }
    })
    
    colors <- c("PRC" = "#D55E00", "ARC" = "#56B4E9")
    
    # Ribbon toggle — shown only when area_prediction_data has a Bound column
    output$ribbon_toggle_ui <- renderUI({
      req(rv$area_prediction_data)
      if (!"Bound" %in% names(rv$area_prediction_data)) return(NULL)
      tags$div(
        style = "display:flex; align-items:center; gap:6px; font-size:13px; color:#555;",
        checkboxInput(
          ns("show_ribbon"),
          label = tags$span(style = "font-size:13px; font-weight:normal;",
                            "Show min\u2013max range"),
          value = TRUE
        )
      )
    })
    
    
    #-----------------------------------------------------------------------
    # Render data table
    output$area_change_table <- renderUI({
      req( rv$area_change_data)
      
      # different table when bounds are present in the data
      is_bound <- "Bound" %in% names(rv$area_prediction_data)
      if(is_bound){
        act <- rv$area_change_data %>%
          ungroup() %>%
          pivot_wider(
            names_from = Bound,
            values_from = c(area, area_change, proportional_rate_change, absolute_rate_change)
          )  %>%
          mutate(area  = format(round(area_opt, 0), big.mark = ",", trim = TRUE),
                 area_change = format(round(area_change_opt, 1), big.mark = ",", trim = TRUE),
                 area_min = format(round(area_min, 0), big.mark = ",", trim = TRUE),
                 area_max = format(round(area_max, 0), big.mark = ",", trim = TRUE),
                 proportional_rate_change = round(proportional_rate_change_opt, 2),
                 absolute_rate_change = round(absolute_rate_change_opt, 2)) %>%
          dplyr::select(!c(dt, area_change_min , area_change_max,
                           proportional_rate_change_min, proportional_rate_change_max,
                           absolute_rate_change_max , absolute_rate_change_min,
                           proportional_rate_change_opt, absolute_rate_change_opt, area_opt, area_change_opt  ),
                        year, area, area_min, area_max, area_change, proportional_rate_change, absolute_rate_change) %>%
          dplyr::rename(
            `Year` = year,
            `Area (km²)` = area,
            `Area lower (km²)` = area_min,
            `Area upper (km²)` = area_max,
            `Area change (km²)` = area_change,
            `Proportional rate of change (%/year)` = proportional_rate_change,
            `Absolute rate of change (km²/year)` = absolute_rate_change
          )
      } else {
        act <-  rv$area_change_data %>%
        mutate(area  = format(round(area, 0), big.mark = ",", trim = TRUE),
               area_change = format(round(area_change, 1), big.mark = ",", trim = TRUE),
               proportional_rate_change = round(proportional_rate_change, 2),
               absolute_rate_change = round(absolute_rate_change, 2)) %>%
        dplyr::select(!dt) %>%
        dplyr::rename(
          `Year` = year,
          `Area (km²)` = area,
          `Area change (km²)` = area_change,
          `Proportional rate of change (%/year)` = proportional_rate_change,
          `Absolute rate of change (km²/year)` = absolute_rate_change
        )
      }
  
      # formatting of table (no longer a DT table)
      shiny::tags$table(
        class = "table table-sm table-striped",
        tags$thead(
          tags$tr(
            lapply(colnames(act), tags$th)
          )
        ),
        tags$tbody(
          apply(act, 1, function(row) {
            tags$tr(lapply(row, tags$td))
          })
        )
      )
    })
    
    
    #--------------------------------------------------------------------------
    # observe for the area prediction stats
    observeEvent(rv$area_change_data, {
      req(rv$area_change_data)
      
      if (NROW(rv$area_change_data) <= 1) {
        rv$area_prediction_data <- NULL
        return()
      }
      
      # Force a new object so reactive triggers
      rv$area_prediction_data <- predict_area_change(rv$area_change_data) %>% dplyr::ungroup()
    })
    
    
    # ---------------------------------------------------------------------
    # plot graph of the data
    # ---------------------------------------------------------------------
    output$area_change_graph <- renderPlotly({
      
      # Nothing to show yet — null or insufficient data
      if (is.null(rv$area_change_data) || NROW(rv$area_change_data) < 2 ||
          is.null(rv$area_prediction_data)) {
        return(
          plotly::plot_ly(type = "scatter") %>%
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                text      = "Only 1 year of data \u2014 can\u2019t create assessment",
                showarrow = FALSE,
                font      = list(size = 16, color = "black")
              ))
            )
        )
      }
      
      # let's get 'is_bound' from the data itself — avoids calling the upload
      is_bound <- "Bound" %in% names(rv$area_prediction_data)
      
      # plot the temporal graph using plot function from modules/figures.R
      make_fig_a_plotly(
        area_prediction_data = rv$area_prediction_data,
        is_bound             = is_bound,
        show_ribbon          = isTRUE(input$show_ribbon)
      )
    })
    
    
    #---------------------------------------------------------------------------
    # Assessments
    #---------------------------------------------------------------------------
    observeEvent(rv$area_prediction_data, {
      req(rv$area_prediction_data)
      
      # For bound rasters run assessment on "opt" only (central estimate)
      is_bound <- "Bound" %in% names(rv$area_prediction_data)
      results  <- if (is_bound) {
        dplyr::filter(rv$area_prediction_data, Bound == "opt")
      } else {
        rv$area_prediction_data
      }
      obsv.fut <- results %>%
        group_by(type, method, segment, scenario) %>%
        filter(segment != "past") %>%
        slice(n()) # this keeps the most recent observed only (certainly where there are just 2 data points...)
      
      # then get past
      past <- results %>%
        group_by(type, method, segment, scenario) %>%
        filter(segment == "past") %>%
        slice(1)
      
      # then combine
      past.obsv.fut <- bind_rows(past, obsv.fut) %>%
        arrange(year)
      
      
      #-----------------------------------------------------------
      # A1 assessment calculation
      a1_result <- past.obsv.fut %>%
        filter(segment %in% "past", method %in% c("ARC", "PRC")) %>%
        group_by(method) %>%
        slice_min(year, n = 1) %>%           # earliest past
        ungroup() %>%
        mutate(
          latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
          perc_change = ((latest_observed_area-area) / area)
          # perc_change = ((area-latest_observed_area) / latest_observed_area)
          
        ) %>%
        dplyr::select(method, scenario, area, latest_observed_area, perc_change) %>%
        mutate(status = dplyr::case_when(
          (perc_change) < -0.8 ~ "CR",
          (perc_change) < -0.5 ~ "EN",
          (perc_change) < -0.3 ~ "VU",
          (perc_change) < -0.27 ~ "NT",
          TRUE ~ "LC"
        ))
      
      #-----------------------------------------------------------
      # A2a assessment
      a2a_result <- past.obsv.fut %>%
        filter(!segment %in% "past", method %in% c("ARC", "PRC")) %>%
        group_by(method) %>%
        slice_max(year, n = 1) %>%           # earliest past
        ungroup() %>%
        mutate(
          latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
          perc_change = ((area-latest_observed_area) / latest_observed_area)
        ) %>%
        dplyr::select(method, scenario, area, latest_observed_area, perc_change) %>%
        mutate(status = dplyr::case_when(
          (perc_change) < -0.8 ~ "CR",
          (perc_change) < -0.5 ~ "EN",
          (perc_change) < -0.3 ~ "VU",
          (perc_change) < -0.27 ~ "NT",
          TRUE ~ "LC"
        ))
      
      #-----------------------------------------------------------
      # A2b assessment
      a2b_results <- list()
      methods <- c("ARC", "PRC")
      scenarios <- setdiff(unique(results$scenario), c("past", "observed"))
      
      for (m in methods) {
        for (s in scenarios) {
          
          # Filter and prepare data
          df <- results %>%
            filter(method %in% c(m, "observed"), scenario  %in% c(s, "past", "observed")) %>%
            arrange(year, desc(type == "observed")) %>%
            distinct(year, .keep_all = TRUE)
          
          # Define start years
          start_years <- df %>%
            filter(scenario  %in% c("past", "observed")) %>%
            filter(!(year %in% c(min(year), max(year)))) %>%
            #filter(!(year %in%  max(year))) %>%
            pull(year)
          
          
          # Calculate percent changes
          a2b_results[[m]][[s]] <- map_dfr(start_years, function(year_start) {
            year_end <- year_start + 50
            df %>%
              filter(year %in% c(year_start, year_end)) %>%
              reframe(perc_change = (area[year == year_end]-area[year == year_start]) / area[year == year_start],
                      direction =  ifelse(perc_change > 0, "increase", "decrease"))
          })
        }
      }
      
      ##Apply case_when to each data frame in the nested list
      a2b_results_labeled <- imap(a2b_results, function(method_list, method_name) {
        imap(method_list, function(df, scenario_name) {
          df %>%
            mutate(
              method = method_name,
              scenario = scenario_name,
              status = case_when(
                (perc_change) < -0.8 ~ "CR",
                (perc_change) < -0.5 ~ "EN",
                (perc_change) < -0.3 ~ "VU",
                (perc_change) < -0.27 ~ "NT", # hard coded currently
                TRUE ~ "LC"
              )
            )
        })
      })
      
      combined_results <- bind_rows(
        purrr::flatten(a2b_results_labeled)
      )
      
      # below is the same result as above
      worst_cases <- combined_results %>%
        group_by(method, scenario) %>%
        slice_min(perc_change, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(method, scenario, perc_change, status)
      
      
      #---------------------------------------------------------------------------
      # create value boxes 
      #---------------------------------------------------------------------------
      a1_box  <- make_value_box(
        title = "Subcriterion A1",
        reactive_data = reactive(a1_result),
        status_col = "status",
        data_col = "perc_change",
        method_col = "method",
        scenario_col = "scenario",
        method_reactive = method,      # <-- pass the reactive
        
        hover_text = "Past predicted change in geographic distribution (past 50 years)",
        year_data = rv$area_prediction_data
      )
      
      a2a_box <- make_value_box(
        title = "Subcriterion A2a",
        reactive_data = reactive(a2a_result),
        status_col = "status",
        data_col = "perc_change",
        method_col = "method",
        method_reactive = method,      # <-- pass the reactive
        
        scenario_col = "scenario",
        hover_text = "Future predicted change in geographic distribution (next 50 years)"
      )
      
      a2b_box <- make_value_box(
        title = "Subcriterion A2b",
        reactive_data = reactive(worst_cases),
        status_col = "status",
        data_col = "perc_change",
        method_col = "method",
        method_reactive = method,      # <-- pass the reactive
        
        scenario_col = "scenario" ,
        hover_text = "Predicted change in geographic distribution over any 50 year period (including the past, present and future)"
      )
      
      # combine the value boxes for ui rendering ------------------------------
      output$criterion_boxes_a <- renderUI({
        layout_column_wrap(
          width = "250px",
          a1_box(),
          a2a_box(),
          a2b_box()
        )
      })
      
    }) # end of Criterion A observe assessment
    
    
    #---------------------------------------------------------------------------
    # Return data
    #---------------------------------------------------------------------------
    return(list(
      area_change_data = reactive({ rv$area_change_data }),
      area_prediction_data = reactive({ rv$area_prediction_data })
    ))
    
  })
  
}