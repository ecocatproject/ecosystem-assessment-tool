# modules/criterion_A_module.R

# worst_rle_status() and the assessment logic live in helpers/rle_assessment.R

# ==============================================================================
# ui
# ==============================================================================
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
      heigh=400,
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
            tags$p(tags$b("Long-term trend:"), "applies the median rate of change calculated across the entire observation period.", style = "text-align: left"),
            tags$p(tags$b("PRC:"), "Proportional rate of change.", style = "text-align: left"),
            tags$p(tags$b("ARC:"), "Absolute rate of change.", style = "text-align: left"),
            tags$p(style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
                   tags$b("Tip:"), "You can remove plot lines by clicking on the items in the legend.")
          )
        ),
        uiOutput(ns("ribbon_toggle_ui"))
      ),
      # # Near-term rate control
      # div(
      #   style = "display:flex; align-items:center; gap:16px; padding:6px 12px;
      #            border-bottom:1px solid #eee; font-size:13px;",
      #   tags$span(style = "font-weight:500; white-space:nowrap;",
      #             "Near-term rate based on:")#,
      #   radioButtons(
      #     ns("n_recent_periods"),
      #     label   = NULL,
      #     choices = c("Most recent period" = "1",
      #                 "Mean of 2 most recent periods" = "2"),
      #     selected = "1",
      #     inline  = TRUE
      #   )
      # ),
      uiOutput(ns("extreme_rate_warning")),
      plotlyOutput(ns("area_change_graph")),
      class = "p-0"
    ),
    
    # ==============================================================================
    # button 
    layout_column_wrap(
      width = "100%",
      fill = FALSE,
      class = "no-gap",
      uiOutput(
        ns("switch_method_ui"))
      ),
    
    # ==============================================================================
    # Subcriterion results 
    uiOutput(ns("criterion_boxes_a"))  # placeholder for the dynamic box
  )
}

# ==============================================================================
# server
# ==============================================================================
criterion_A_module_server <- function(id, upload_module, main_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ==============================================================================
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
    
    # ==============================================================================
    # reset values when new rasters uploaded
    observeEvent(upload_module$rasters(), {
      rv$area_change_data      <- NULL
      rv$area_prediction_data  <- NULL
      rv$a1_result             <- NULL
      rv$a2a_result            <- NULL
      rv$a2b_result            <- NULL
      rv$criterion_a_status    <- NULL
      output$criterion_boxes_a <- renderUI({
        div(class = "text-muted", "")
      })
    }, ignoreInit = TRUE)
    
    
    # ==============================================================================
    # Reactive values container to store data
    rv <- reactiveValues(area_change_data     = NULL,
                         area_prediction_data = NULL,
                         a1_result            = NULL,  # A1  past 50 yrs
                         a2a_result           = NULL,  # A2a future 50 yrs
                         a2b_result           = NULL,  # A2b worst 50-yr window
                         criterion_a_status   = NULL)  # overall worst across A1/A2a/A2b
    
    
    # ==============================================================================
    # calculate area change data for table
    observeEvent(upload_module$rasters(), {
      rv$area_change_data <- calc_area_change(upload_module$areas())
    })
    
    
    # ==============================================================================
    # reactive value to store currently selected method
    method <- reactiveVal("PRC")
    
    # Button changes method
    observeEvent(input$switch_method, {
      method(ifelse(method() == "PRC", "ARC", "PRC"))
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
    
    
    # ==============================================================================
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
      
      # order by descending year
      act <- act %>% dplyr::arrange(desc(Year))
      
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
    
    
    # ==============================================================================
    # observe for the area prediction stats
    observeEvent(rv$area_change_data, {
      req(rv$area_change_data)
      
      print(rv$area_change_data)
      
      if (NROW(rv$area_change_data) <= 1) {
        rv$area_prediction_data <- NULL
        return()
      }
      
      # Force a new object so reactive triggers
      rv$area_prediction_data <- predict_area_change(
        rv$area_change_data#,
        # n_recent = as.integer(input$n_recent_periods)
      ) %>% dplyr::ungroup()
    })
    
    
    # ==============================================================================
    # plot graph of the data
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
                text      = "Upload at least two rasters to calculate Criterion A",
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
    
    # ==============================================================================
    # button ui toggle
    # ==============================================================================
    # Show the PRC/ARC switch button only when there's prediction data
    # (= ≥2 years of area, which is also when the value boxes get created).
    # isolate(method()) so the button isn't destroyed/recreated on every toggle —
    # the existing updateActionButton observer below handles label updates.
    output$switch_method_ui <- renderUI({
      if (is.null(rv$area_prediction_data)) return(NULL)
      
      btn_label <- #isolate({
        if (method() == "PRC") {
          HTML(
            'Assessment outcome based on the  
         <b><span style="color:#D55E00;">Proportional Rate of Change (PRC)</span></b> 
         - Click here for 
         <b><span style="color:#56B4E9;">Absolute Rate of Change (ARC)</span></b>'
          )
        } else {
          HTML(
            'Assessment outcome based on the 
         <b><span style="color:#56B4E9;">Absolute Rate of Change (ARC)</span></b> 
         - Click here for 
         <b><span style="color:#D55E00;">Proportional Rate of Change (PRC)</span></b>'
          )
        }
     # })
      
      actionButton(
        ns("switch_method"),
        label = btn_label,
        width = "100%",
        style = "background-color: white; color: black; border: 3px solid #696969;"
      )
    })
    
    
    # ==============================================================================
    # Assessments
    # ==============================================================================
    observeEvent(rv$area_prediction_data, {
      req(rv$area_prediction_data)

      # A1 / A2a / A2b - helpers/rle_assessment.R (also used by the exported script)
      res <- assess_criterion_a(rv$area_prediction_data, rv$area_change_data)
      req(res)   # NULL when < 2 observed years (e.g. single bound file)

      a1_result    <- res$a1
      a2a_result   <- res$a2a
      worst_cases  <- res$a2b
      obs_years    <- res$obs_years
      max_obs_year <- max(obs_years, na.rm = TRUE)

      # Warn when the near-term rate (>10 %/yr PRC) is likely unrealistic
      output$extreme_rate_warning <- renderUI({
        if (isTRUE(res$near_term_prc > 10)) {
          div(
            class = "alert alert-warning",
            style = "font-size:13px; margin:6px 0;",
            tags$i(class = "fas fa-exclamation-triangle"), " ",
            tags$strong("Note:"), " The near-term rate used for projection is ",
            tags$strong(paste0(round(res$near_term_prc, 1), "%/yr")),
            " which is likely to be ecologically unrealistic. Consider removing near-term result lines."
          )
        }
      })

      rv$a1_result          <- a1_result
      rv$a2a_result         <- a2a_result
      rv$a2b_result         <- worst_cases
      rv$criterion_a_status <- res$status

      # ==============================================================================
      # create value boxes 
      # ==============================================================================
      a1_box  <- make_value_box(
        title         = "Subcriterion A1",
        reactive_data = reactive(a1_result),
        status_col    = "status",
        data_col      = "perc_change",
        method_col    = "method",
        scenario_col  = "scenario",
        method_reactive = method,
        date_range    = paste0("(", max_obs_year - 50, " - ", max_obs_year, ")"),
        hover_text    = "Past predicted change in geographic distribution (past 50 years)",
        year_data     = rv$area_prediction_data,
        n_obs_years = length(obs_years)
      )
      
      a2a_box <- make_value_box(
        title         = "Subcriterion A2a",
        reactive_data = reactive(a2a_result),
        status_col    = "status",
        data_col      = "perc_change",
        method_col    = "method",
        method_reactive = method,
        scenario_col  = "scenario",
        date_range    = paste0("(", max_obs_year, " - ", max_obs_year + 50, ")"),
        hover_text    = "Future predicted change in geographic distribution (next 50 years)",
        n_obs_years = length(obs_years)
      )
      
      a2b_box <- make_value_box(
        title         = "Subcriterion A2b",
        reactive_data = reactive(worst_cases %>% dplyr::select(method, scenario, perc_change, status, year_start, year_end)),
        status_col    = "status",
        data_col      = "perc_change",
        method_col    = "method",
        method_reactive = method,
        scenario_col  = "scenario",
        date_range      = NULL,            # shown per-line instead
        year_start_col  = "year_start",    # pulls from worst_cases
        year_end_col    = "year_end",
        hover_text    = "Predicted change in geographic distribution over any 50 year period including the past, present and future.
                          Values presented are from the predicted worst case scenario 50 year period.",
        n_obs_years = length(obs_years)
      )
      
      # ==============================================================================
      # combine the value boxes for ui rendering
      output$criterion_boxes_a <- renderUI({
        # layout_column_wrap(
          # width = "250px",
          layout_columns(
            col_widths = breakpoints(
              xs = c(12, 12, 12),   # phones: one per row
              sm = c(12, 12, 12),   # small tablets: still stacked
              md = c(4, 4, 4)       # medium+: side-by-side
            ),
  
          
                    a1_box(),
          a2a_box(),
          a2b_box()
        )
        # )
      })
      
    }) # end of Criterion A observe assessment
    
    
    # ==============================================================================
    # Return data
    # ==============================================================================
    return(list(
      area_change_data     = reactive({ rv$area_change_data }),
      area_prediction_data = reactive({ rv$area_prediction_data }),
      a1_result            = reactive({ rv$a1_result }),
      a2a_result           = reactive({ rv$a2a_result }),
      a2b_result           = reactive({ rv$a2b_result }),
      criterion_a_status   = reactive({ rv$criterion_a_status })
    ))
    
  })
  
}
