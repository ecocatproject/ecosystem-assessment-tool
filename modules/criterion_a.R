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
      fill= FALSE,
      card_header("Ecosystem area change projections",
                  tooltip(
                    bs_icon("info-circle"),
                    "Absolute and proportional rate change by default. This will include a modelled rate if enough data"
                  )),
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
        
        # ----------------------------------------------------------------------
        # modal to provide some user info
        modal_a_shown <- reactiveVal(FALSE) # prevent the modal from popping up multiple times per session
        
        # model pop up on tab opening 
        observeEvent(main_tab(), {
          if (identical(main_tab(), "Criterion A")) { # Only fire when Criterion A is selected
            if (!modal_a_shown()) {
              showModal(a_quick_modal)
              modal_a_shown(TRUE)
            }
          }
        }, ignoreInit = TRUE)
        
        #-----------------------------------------------------------------------
        # reset values        
        observeEvent(upload_module$rasters(), {
          rv$area_change_data <- NULL
          rv$area_prediction_data <- NULL
          
          # Reset the boxes immediately
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
        
        
# # update button label whenever the method changes
#   observe({
#     if (method() == "PRC") {
#       updateActionButton(
#         session, "switch_method",
#         label = HTML(
#           'Assessment result below is using <b>Proportional Rate of Change</b> - Click here to view assessment using <b>Absolute Rate of Change (ARC)</b>'
#         )      )
#     } else {
#       updateActionButton(
#         session, "switch_method",
#         label = HTML(
#           'Assessment result below is using <b>Absolute Rate of Change</b> - Click here to view assessment using <b>Proportional Rate of Change (PRC)</b>'
#         )
#       )
#     }
#   })
#   
  
  # update button label whenever the method changes
  observe({
    if (method() == "PRC") {
      updateActionButton(
        session, "switch_method",
        label = HTML(
          'Assessment result below is using 
         <b><span style="color:#D55E00;">Proportional Rate of Change (PRC)</span></b> 
         - Click here to view assessment using 
         <b><span style="color:#56B4E9;">Absolute Rate of Change (ARC)</span></b>'
        )
      )
    } else {
      updateActionButton(
        session, "switch_method",
        label = HTML(
          'Assessment result below is using 
         <b><span style="color:#56B4E9;">Absolute Rate of Change (ARC)</span></b> 
         - Click here to view assessment using 
         <b><span style="color:#D55E00;">Proportional Rate of Change (PRC)</span></b>'
        )
      )
    }
  })
  
  colors <- c("PRC" = "#D55E00",
              "ARC" = "#56B4E9")
        #-----------------------------------------------------------------------
        # Render data table
        output$area_change_table <- renderUI({
          req( rv$area_change_data)
          
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
        
        
        # # Make outputs render even when tab is hidden
        # outputOptions(output, "area_change_table", suspendWhenHidden = FALSE)
        # # outputOptions(output, "area_change_graph", suspendWhenHidden = FALSE)
        # 
        # # output$area_change_table <- bindEvent(output$area_change_table, upload_module$rasters(), ignoreInit = TRUE)
        

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
            # req(rv$area_prediction_data)
            
            # If insufficient rows, show a message
            if (NROW(rv$area_change_data) < 2) {
              return(
                plotly::plot_ly() %>%
                  layout(
                    xaxis = list(visible = FALSE),
                    yaxis = list(visible = FALSE),
                    annotations = list(
                      list(
                        x = 0.5,
                        y = 0.5,
                        text = "Only 1 year of data - can't create assessment",
                        showarrow = FALSE,
                        xref = "paper",
                        yref = "paper",
                        font = list(size = 16, color = "black")
                      )
                    )
                  )
              )
            }
            
            
        # Prepare hover text
        results <- rv$area_prediction_data %>%
          dplyr::ungroup() %>%   # ensures Shiny sees a new object
          mutate(
            hover_text = paste0(
              "Year: ", year, "<br>",
              "Area: ", round(area,0), "<br>",
              "Scenario: ", scenario, "<br>",
              "Method: ", method
            )
          )
        
        # # Define line styles for methods
        # linetypes <- c("PRC" = "solid", "ARC" = "dot")
        # 
        # # Define colors for scenarios
        # colors <- c("median" = "#ff7f00",
        #             "recent" = "#e31a1c",
        #             "worst" = "darkred",
        #             "recent/worst" = "darkred",
        #             "past" = "steelblue")
        # 
        # # Start Plotly
        # p <- plot_ly()
        # 
        # # Add observed line + points
        # p <- p %>%
        #   add_trace(
        #     data = filter(results, type == "observed"), # results is the rv$area_prediction_data
        #     x = ~year, y = ~area, type = 'scatter', mode = 'lines+markers',
        #     line = list(color = 'grey20', width = 3),
        #     marker = list(color = 'grey10', size = 6),
        #     hoverinfo = 'text',
        #     text = ~hover_text,
        #     name = 'Observed'
        #   )
        # 
        # # Add projected lines
        # proj_data <- filter(results, type == "projected")
        # for(scen in unique(proj_data$scenario)) {
        #   for(meth in unique(proj_data$method)) {
        #     sub <- filter(proj_data, scenario == scen, method == meth)
        #     p <- add_trace(
        #       p,
        #       data = sub,
        #       x = ~year, y = ~area, type = 'scatter', mode = 'lines',
        #       line = list(color = colors[scen], dash = linetypes[meth], width = 3),
        #       hoverinfo = 'text',
        #       text = ~hover_text,
        #       name = paste(scen, meth, sep = "/")
        #     )
        #   }
        # }
        
        # Define line styles for methods
        colors <- c("PRC" = "#D55E00",
                    "ARC" = "#56B4E9")
        
        linetypes <-  c("median" = "dot", 
                        "recent" = "solid",
                        "worst" = "dash",
                        "recent/worst" = "dash",
                        "past" = "longdashdot")
        
        # Start Plotly
        p <- plot_ly()
        
        # Add observed line + points
        p <- p %>%
          add_trace(
            data = filter(results, type == "observed"), # results is the rv$area_prediction_data
            x = ~year, y = ~area, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'grey20', width = 3),
            marker = list(color = 'grey10', size = 6),
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
              line = list(color = colors[meth], 
                          dash = linetypes[scen], width = 3),
              name = paste(scen, meth, sep = "/")
            )
          }
        }
        
        # ---------------------------------------------------------------------
        # GAM smoother (only if enough points)
        n_points <- NROW(rv$area_change_data)

        if (n_points > 3) {
          dat <- rv$area_change_data
          
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
          
            # pred_gam_before_after <- rbind(pred_gam_before, pred_gam_after)
            
            # Add GAM traces
            # p <- p %>%
            #   add_trace(
            #     data = pred_gam_before,
            #     x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
            #     line = list(color = 'blue', width = 3, dash = 'dash'),
            #     showlegend = FALSE,
            #     name="GAM"
            #   ) %>%
            #   add_trace(
            #     data = pred_gam_after,
            #     x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
            #     line = list(color = 'blue', width = 3, dash = 'dash'),
            #     name = 'GAM'
            #   ) %>%
            #   add_ribbons(
            #     data = pred_gam,
            #     x = ~year, ymin = ~lwr, ymax = ~upr,
            #     fillcolor = 'rgba(0,0,255,0.2)',
            #     line = list(color = 'transparent'),
            #     name = '95% CI'
            #   )
            
            # p <- p %>%
            #   add_trace(
            #     data = pred_gam_before,
            #     x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
            #     line = list(color = 'blue', width = 3, dash = 'dash'),
            #     name = "GAM",
            #     legendgroup = "GAM",
            #     showlegend = TRUE
            #   ) %>%
            #   add_trace(
            #     data = pred_gam_after,
            #     x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
            #     line = list(color = 'blue', width = 3, dash = 'dash'),
            #     name = "GAM",
            #     legendgroup = "GAM",
            #     showlegend = FALSE
            #   ) %>%
            #   add_ribbons(
            #     data = pred_gam,
            #     x = ~year, ymin = ~lwr, ymax = ~upr,
            #     fillcolor = 'rgba(0,0,255,0.2)',
            #     line = list(color = 'transparent'),
            #     name = '95% CI',
            #     legendgroup = "GAM"
            #   )
            
            p <- p %>%
              add_trace(
                data = pred_gam_before,
                x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
                line = list(color = 'blue', width = 3, dash = 'dash'),
                name = "GAM",
                legendgroup = "GAM",
                showlegend = TRUE,
                visible = "legendonly"   # hidden by default, but present in legend
              ) %>%
              add_trace(
                data = pred_gam_after,
                x = ~year, y = ~fit, type = 'scatter', mode = 'lines',
                line = list(color = 'blue', width = 3, dash = 'dash'),
                name = "GAM",
                legendgroup = "GAM",
                showlegend = FALSE       # not shown in legend, but grouped
                # visible will follow the legend item above
              ) %>%
              add_ribbons(
                data = pred_gam,
                x = ~year, ymin = ~lwr, ymax = ~upr,
                fillcolor = 'rgba(0,0,255,0.2)',
                line = list(color = 'transparent'),
                name = '95% CI',
                # If you want the CI to toggle with GAM as well, keep the next two lines.
                legendgroup = "GAM",
                showlegend = FALSE,
                visible = "legendonly"   # start hidden too (even if not in legend)
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

        
    #---------------------------------------------------------------------------
    # Assessments
    #---------------------------------------------------------------------------
        observeEvent(rv$area_prediction_data, {
          req( rv$area_prediction_data)
  
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
      # a1_result <- past.obsv.fut %>%
      #   filter(scenario %in% "past", method %in% c("ARC", "PRC")) %>%
      #   group_by(method) %>%
      #   slice_min(year, n = 1) %>%           # earliest past
      #   ungroup() %>%
      #   mutate(
      #     latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
      #     perc_change = (latest_observed_area/area) -1
      #     
      #   ) %>%
      #   dplyr::select(method, area, latest_observed_area, perc_change) %>%
      #   mutate(A1 = dplyr::case_when(
      #     abs(perc_change) > 0.8 ~ "CR",
      #     abs(perc_change) > 0.5 ~ "EN",
      #     abs(perc_change) > 0.3 ~ "VU",
      #     abs(perc_change) > 0.27 ~ "NT",
      #     TRUE ~ "LC"
      #   ))
      a1_result <- past.obsv.fut %>%
        filter(scenario %in% "past", method %in% c("ARC", "PRC")) %>%
        group_by(method) %>%
        slice_min(year, n = 1) %>%           # earliest past
        ungroup() %>%
        mutate(
          latest_observed_area = past.obsv.fut$area[past.obsv.fut$scenario == "observed"],
          perc_change = ((latest_observed_area-area) / area)
          # perc_change = ((area-latest_observed_area) / latest_observed_area)
          
        ) %>%
        dplyr::select(method, area, latest_observed_area, perc_change) %>%
        mutate(A1 = dplyr::case_when(
          (perc_change) < -0.8 ~ "CR",
          (perc_change) < -0.5 ~ "EN",
          (perc_change) < -0.3 ~ "VU",
          (perc_change) < -0.27 ~ "NT",
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
          perc_change = ((area-latest_observed_area) / latest_observed_area)
        ) %>%
        dplyr::select(method, scenario, area, latest_observed_area, perc_change) %>%
        mutate(A2a = dplyr::case_when(
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
      
      # for (m in methods) {
      #   for (s in scenarios) {
      #     
      #     # Filter and prepare data
      #     df <- results %>%
      #       filter(method != m, scenario != s) %>%
      #       arrange(year, desc(type == "observed")) %>%
      #       distinct(year, .keep_all = TRUE)
      #     
      #     # Define start years
      #     start_years <- df %>%
      #       filter(year <= max(year) - 51) %>%  # -1 - 50 = -51
      #       pull(year) %>%
      #       unique()
      #     
      #     # Calculate percent changes
      #     a2b_results[[m]][[s]] <- map_dfr(start_years, function(year_start) {
      #       year_end <- year_start + 50
      #       df %>%
      #         filter(year %in% c(year_start, year_end)) %>%
      #         summarise(#perc_change = (area[which.max(year)]/area[which.min(year)])-1 , # (recent/oldest)-1
      #                   perc_change = (area[which.min(year)]-area[which.max(year)]) / area[which.min(year)],
      #                   perc_change = (area[which.max(year)]-area[which.min(year)]) / area[which.max(year)],
      #                   direction =  ifelse(perc_change > 0, "increase", "decrease")
      #         )
      #     })
      #   }
      # }
        
        for (m in methods) {
          for (s in scenarios) {
            
            # Filter and prepare data
            df <- results %>%
              filter(method %in% c(m, "observed"), scenario %in% c(s, "past", "observed")) %>%
              arrange(year, desc(type == "observed")) %>%
              distinct(year, .keep_all = TRUE)
            
            # Define start years
            start_years <- df %>%
              filter(scenario %in% c("past", "observed")) %>%
              filter(!(year %in% c(min(year), max(year)))) %>%
              #filter(!(year %in%  max(year))) %>%
              pull(year)
            
            
            # Calculate percent changes
            a2b_results[[m]][[s]] <- map_dfr(start_years, function(year_start) {
              year_end <- year_start + 50
              df %>%
                filter(year %in% c(year_start, year_end)) %>%
                summarise(perc_change = (area[year == year_end]-area[year == year_start]) / area[year == year_start],
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
              A2b = case_when(
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
        dplyr::select(method, scenario, perc_change, A2b)

      
      #---------------------------------------------------------------------------
      # create value boxes 
      #---------------------------------------------------------------------------
      a1_box  <- make_value_box(
        title = "Subcriterion A1",
        reactive_data = reactive(a1_result),
        status_col = "A1",
        data_col = "perc_change",
        method_col = "method",
        
        method_reactive = method,      # <-- pass the reactive
        
        hover_text = "Past predicted change in geographic distribution (past 50 years)",
        year_data = rv$area_prediction_data
      )

      a2a_box <- make_value_box(
        title = "Subcriterion A2a",
        reactive_data = reactive(a2a_result),
        status_col = "A2a",
        data_col = "perc_change",
        method_col = "method",
        method_reactive = method,      # <-- pass the reactive
        
        scenario_col = "scenario",
        hover_text = "Future predicted change in geographic distribution (next 50 years)"
      )

      a2b_box <- make_value_box(
        title = "Subcriterion A2b",
        reactive_data = reactive(worst_cases),
        status_col = "A2b",
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

