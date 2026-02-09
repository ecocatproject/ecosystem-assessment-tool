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
      full_screen = TRUE,
      card_header("Ecosystem area change data",
                  tooltip(
                    bs_icon("info-circle"),
                    "Ecosystem area per assessment period"
                  )),
      # DTOutput(ns("area_change_table"), width = "100%")
      uiOutput(ns("area_change_table"))
    ),
    
    # Graph of temporal change ------------------------------------------------ 
    card(
      full_screen = TRUE,
      card_header("Ecosystem area change projections",
                  tooltip(
                    bs_icon("info-circle"),
                    "Absolute and proportional rate change by default. This will include a modelled rate if enough data"
                  )),
      plotlyOutput(ns("area_change_graph")),
      class = "p-0"
    ),
    
    # Subcriterion results ----------------------------------------------------
    layout_column_wrap(
      height = 100,
      page_fillable(
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
        
        # modal to provide some user info -----------------------------------------
        # 1) prevent the modal from popping up multiple times per session
        modal_a_shown <- reactiveVal(FALSE)
        
        # 2) model pop up on tab opening 
        observeEvent(main_tab(), {
          # Only fire when Criterion A is selected
          if (identical(main_tab(), "Criterion A")) {
            # If you want it every time, remove the guard:
            if (!modal_a_shown()) {
              showModal(a_quick_modal)
              modal_a_shown(TRUE)
            }
          }
        }, ignoreInit = TRUE)
        

       # Render table --------------------------------------------------------
       # this was a DT but now static as DT was sometimes not updating 
       # static table has same formatting - doesn't need to be an interactive table anyway
        output$area_change_table <- renderUI({
          req(area_change_data())
          
          act <- area_change_data() %>%
            mutate(area  = format(round(area, 0), big.mark = ",", trim = TRUE),
                   area_change = format(round(area_change, 1), big.mark = ",", trim = TRUE),
                   proportional_rate_change = round(proportional_rate_change, 2),
                   absolute_rate_change = round(absolute_rate_change, 2)) %>%
            # dplyr::arrange(desc(perc_roi)) %>%
            dplyr::select(!dt) %>% # remove this column
            dplyr::rename(
              `Year` = year,
              `Area (km²)` = area,
              `Area change (km²)` = area_change,
              `Proportional rate of change (%/year)` = proportional_rate_change,
              `Absolute rate of change (km²/year)` = absolute_rate_change
            )
          
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
          
        }
        )
        
        # Make outputs render even when tab is hidden
        outputOptions(output, "area_change_table", suspendWhenHidden = FALSE)

        
  # reset when new rasters uploaded ------------------------------------------
    observeEvent(upload_module$rasters(), {
      # reset UI and reactive holders
      # output$criterion_boxes_a <- renderUI({ NULL })
      output$criterion_boxes_a <- NULL   # better than renderUI(NULL)
      # output$area_change_table <- NULL   # better than renderUI(NULL)
      #rv$area_prediction_data(NULL)
    }, ignoreNULL = TRUE)
    
        
        # -------------------------------------------------------------------------
        # Reactive values container
        rv <- reactiveValues(
          area_prediction_data = NULL,
          area_change_data = NULL
        )
        
    # Reactive expression for area change
    area_change_data <- reactive({
      req(upload_module$rasters())
      cat("area_change_data recalculated\n")
      df <- calc_area_change(upload_module$areas())
      df          # <-- MUST return df
      # 
      print(df)
    })

    rv$area_change_data <- area_change_data 
    
  
    #--------------------------------------------------------------------
    # calculate past and future predictions using the above function
    # If no or insufficient data, show single-row message --------------------
    observe({
      req(area_change_data())
      if (NROW(area_change_data()) == 0) return()   # no data yet, do nothing
      
      if (NROW(area_change_data()) < 2) {
        output$area_change_graph <- renderPlotly({
          plotly::plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  x = 0.5,  # center horizontally
                  y = 0.5,  # center vertically
                  text = "Only 1 year of data - can't create assessment",
                  showarrow = FALSE,
                  xref = "paper",
                  yref = "paper",
                  font = list(size = 16, color = "black")
                )
              )
            )
        })
        
        # empty tables with message
        output$a1_assess_table <- DT::renderDT({empty_assess_table()})
        output$a2a_assess_table <- DT::renderDT({empty_assess_table()})
        output$a2b_assess_table <- DT::renderDT({empty_assess_table()})
      }
    })
    
    # observe for the area change stats
    observe({
      req(area_change_data(), NROW(area_change_data()) > 1)
      area_prediction_data <- predict_area_change(area_change_data())
      rv$area_prediction_data <- area_prediction_data # store in reactive
      cat("Updating rv$area_prediction_data\n")
      
      # ---------------------------------------------------------------------
      # plot the data
      # ---------------------------------------------------------------------
      output$area_change_graph <- renderPlotly({
        # Prepare hover text
        results <- rv$area_prediction_data %>%
          mutate(
            hover_text = paste0(
              "Year: ", year, "<br>",
              "Area: ", round(area,0), "<br>",
              "Scenario: ", scenario, "<br>",
              "Method: ", method
            )
          )
        
        # Define line styles for methods
        linetypes <- c("PRC" = "solid", "ARC" = "dot")
        
        # Define colors for scenarios
        colors <- c("median" = "#ff7f00",
                    "recent" = "#e31a1c",
                    "worst" = "darkred",
                    "recent/worst" = "darkred",
                    "past" = "steelblue")
        
        # Start Plotly
        p <- plot_ly()
        
        # Add observed line + points
        p <- p %>%
          add_trace(
            data = filter(results, type == "observed"), # results is the rv$area_prediction_data
            x = ~year, y = ~area, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'grey20', width = 3),
            marker = list(color = 'grey10', size = 6),
            hoverinfo = 'text',
            text = ~hover_text,
            name = 'Observed'
          )
        
        # Add projected lines
        proj_data <- filter(results, type == "projected")
        for(scen in unique(proj_data$scenario)) {
          for(meth in unique(proj_data$method)) {
            sub <- filter(proj_data, scenario == scen, method == meth)
            p <- add_trace(
              p,
              data = sub,
              x = ~year, y = ~area, type = 'scatter', mode = 'lines',
              line = list(color = colors[scen], dash = linetypes[meth], width = 3),
              hoverinfo = 'text',
              text = ~hover_text,
              name = paste(scen, meth, sep = "/")
            )
          }
        }
        
        # ---------------------------------------------------------------------
        # GAM smoother (only if enough points)
        # ---------------------------------------------------------------------
        n_points <- nrow(area_change_data())
     
        if (n_points > 3) {
          dat <- area_change_data()
          
          # fit GAM with try()
          gam_model <- try(
            mgcv::gam(area ~ s(year, k = max(3, n_points - 2)), data=dat),
            silent = TRUE
          )
          
          if (!inherits(gam_model, "try-error")) {
            new_years <- data.frame(year = seq(min(rv$area_prediction_data$year), max(rv$area_prediction_data$year)))
            pred_gam <- predict(gam_model, newdata = new_years, se.fit = TRUE)
            
            pred_gam <- data.frame(
              year = new_years$year,
              fit = pred_gam$fit,
              lwr = pred_gam$fit - 2 * pred_gam$se.fit,
              upr = pred_gam$fit + 2 * pred_gam$se.fit
            ) %>%
              mutate(across(c(fit, lwr, upr), ~ pmax(., 0)))
             
            # For plotting split data into before/after
            pred_gam_before <- filter(pred_gam, year < min(dat$year))
            pred_gam_after  <- filter(pred_gam, year > max(dat$year))
          
            # Add GAM traces
            p <- p %>%
              add_trace(
                data = pred_gam_before,
                x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
                line = list(color = 'blue', width = 3, dash = 'dash'),
                showlegend = FALSE,
                name="GAM"
              ) %>%
              add_trace(
                data = pred_gam_after,
                x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
                line = list(color = 'blue', width = 3, dash = 'dash'),
                name = 'GAM'
              ) %>%
              add_ribbons(
                data = pred_gam,
                x = ~year, ymin = ~lwr, ymax = ~upr,
                fillcolor = 'rgba(0,0,255,0.2)',
                line = list(color = 'transparent'),
                name = '95% CI'
              )
          }
        } # end of if >3 points
        
        # Layout
        p <- p %>%
          layout(
            xaxis = list(title = "Year"),
            yaxis = list(title = "Area"),
            legend = list(title = list(text = "<b>Scenario / Method</b>"))
          )
        p
        
      })
    })
  
    #-----------------------------------------------------------
    # Assessments
    #-----------------------------------------------------------
    #   req(area_change_data(), NROW(area_change_data()) > 1)
      observeEvent(upload_module$rasters(), ignoreInit = FALSE, {
        req(area_change_data(), NROW(area_change_data()) > 1)
        
      # actually lets do the assessment tables separately
      results <- rv$area_prediction_data
      obsv.fut <- results %>%
        group_by(type, method, scenario) %>%
        filter(scenario != "past") %>%
        slice(n()) # this keeps the most recent observed only (certainly where there are just 2 data points...)
      
      # then get past
      past <- results %>%
        group_by(type, method, scenario) %>%
        filter(scenario == "past") %>%
        slice(1)
      
      # then combine
      past.obsv.fut <- bind_rows(past, obsv.fut) %>%
        arrange(year)
    
      
      #-----------------------------------------------------------
      # A1 assessment calculation
      a1_result <- past.obsv.fut %>%
        filter(scenario %in% "past", method %in% c("ARC", "PRC")) %>%
        # filter(scenario %in% "past", method %in% c("PRC")) %>%
        group_by(method) %>%
        slice_min(year, n = 1) %>%           # earliest past
        ungroup() %>%
        mutate(
          latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
          perc_change = (latest_observed_area/area) -1
        ) %>%
        dplyr::select(method, area, latest_observed_area, perc_change) %>%
        mutate(A1 = dplyr::case_when(
          abs(perc_change) > 0.8 ~ "CR",
          abs(perc_change) > 0.5 ~ "EN",
          abs(perc_change) > 0.3 ~ "VU",
          abs(perc_change) > 0.27 ~ "NT",
          TRUE ~ "LC"
        ))
      
      #-----------------------------------------------------------
      # A2a assessment
      a2a_result <- past.obsv.fut %>%
        filter(!scenario %in% "past", method %in% c("ARC", "PRC")) %>%
        group_by(method) %>%
        slice_max(year, n = 1) %>%           # earliest past
        ungroup() %>%
        mutate(
          latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
          perc_change = (area/latest_observed_area) -1
        ) %>%
        dplyr::select(method, scenario, area, latest_observed_area, perc_change) %>%
        mutate(A2a = dplyr::case_when(
          abs(perc_change) > 0.8 ~ "CR",
          abs(perc_change) > 0.5 ~ "EN",
          abs(perc_change) > 0.3 ~ "VU",
          abs(perc_change) > 0.27 ~ "NT",
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
            filter(method != m, scenario != s) %>%
            arrange(year, desc(type == "observed")) %>%
            distinct(year, .keep_all = TRUE)
          
          # Define start years
          start_years <- df %>%
            filter(year <= max(year) - 51) %>%  # -1 - 50 = -51
            pull(year) %>%
            unique()
          
          # Calculate percent changes
          a2b_results[[m]][[s]] <- map_dfr(start_years, function(year_start) {
            year_end <- year_start + 50
            df %>%
              filter(year %in% c(year_start, year_end)) %>%
              summarise(perc_change = (area[which.max(year)]/area[which.min(year)])-1 , # (recent/oldest)-1
                        direction =  ifelse(perc_change > 0, "increase", "decrease")
              )
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
              A2b = case_when(
                abs(perc_change) > 0.8 ~ "CR",
                abs(perc_change) > 0.5 ~ "EN",
                abs(perc_change) > 0.3 ~ "VU",
                abs(perc_change) > 0.27 ~ "NT", # hard coded currently
                TRUE ~ "LC"
              )
            )
        })
      })
      
      # merge results --------------------------------------------------------
      combined_results <- bind_rows(
        purrr::flatten(a2b_results_labeled)
      )
      
      # below is the same result as above
      worst_cases <- combined_results %>%
        group_by(method, scenario) %>%
        slice_max(perc_change, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(method, scenario, perc_change, A2b)

      
      
      #  create value boxes --------------------------------------------------
      
      a1_box  <- make_value_box(
        title = "Subcriterion A1",
        reactive_data = reactive(a1_result),
        status_col = "A1",
        data_col = "perc_change",
        method_col = "method",
        hover_text = "Past predicted change in geographic distribution (past 50 years)",
        year_data = rv$area_prediction_data
      )
      
      a2a_box <- make_value_box(
        title = "Subcriterion A2a",
        reactive_data = reactive(a2a_result),
        status_col = "A2a",
        data_col = "perc_change",
        method_col = "method",
        scenario_col = "scenario",
        hover_text = "Future predicted change in geographic distribution (next 50 years)" 
      )
      
      a2b_box <- make_value_box(
        title = "Subcriterion A2b",
        reactive_data = reactive(worst_cases),
        status_col = "A2b",
        data_col = "perc_change",
        method_col = "method",
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
    
  
      # Return reactive values for use outside the module
    return(list(
      area_change_data = reactive({rv$area_change_data()}),
      area_prediction_data = reactive({rv$area_prediction_data})
    ))
    
      
    })
}

