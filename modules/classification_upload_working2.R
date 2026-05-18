# modules/classification_upload


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
# UI  - sidebar only
classification_module_ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    width = 320,
    
    # 1) File upload (hidden when example = TRUE)
    conditionalPanel(
      condition = "!input.example",
      tags$span(
        tags$strong(style = "font-size:14px;", "1) Upload your ecosystem map(s)"), tooltip(
          bs_icon("info-circle"),
          "The files must be in raster format (e.g., GeoTIFF, .tif). You can also try the tool using our example ecosystem maps.​"
        ),
        tags$p(style = "margin:0 0 3px; font-size:13px; color:#6b7280; line-height:1.5;",
               "Upload maps from multiple years to assess changes in ecosystem distribution (Criterion A)."),
  
       
        # fileInput(ns("tifs"), "", multiple = TRUE, accept = ".tif")
        uiOutput(ns("tifs_ui"))
      )
    ),
    
    # 2) Example dataset toggle option
    checkboxInput(ns("example"),
                  tags$span( tags$strong(style = "font-size:14px;","Or use our example ecosystem maps"),
                             tooltip(bs_icon("info-circle"),
                                     "Classification of the Itigi-Sumbu thicket ecosystem in Zambia. Further details can be
              found in the 'Example dataset' section of the 'About & Help' Tab"
                             )), value = FALSE),
    
    # 4) separator line is better than hr()  - less white space
    div(style = "border-top: 1px solid #ccc; margin: 10px 0;"),
    
    # 5) remaining sidebar sections 
    # uiOutput(ns("part2")),
    uiOutput(ns("year_sidebar_section")),
    uiOutput(ns("ecosystem_dropdown")),
    uiOutput(ns("run_description")),
    uiOutput(ns("process_btn_text")),
    uiOutput(ns("process_btn_ui")),
    uiOutput(ns("refine_revert_ui"))
  )
}

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
classification_module_server <- function(id, drawn_poly=reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # file reset key
    file_key <- reactiveVal(0)
    
    observeEvent(input$example, {
      
      if (isTRUE(input$example)) {
        file_key(file_key() + 1)  # forces new fileInput
      }
      
    })
    
    # -------------------------------------------------------------------------
    # file input UI - either upload or example 
    output$tifs_ui <- renderUI({
      
      if (isTRUE(input$example)) return(NULL)
      
      key <- file_key()
      
      fileInput(
        ns(paste0("tifs_", key)), "", multiple = TRUE, accept = ".tif"
      )
    })
    
    
    # ------------------------------------------------------------------------
    ## reset helper 
    reset <- function(id) {
      shiny::updateTextInput(session, id, value = "")
      shiny::updateSelectInput(session, id, selected = character(0))
      shiny::updateCheckboxInput(session, id, value = FALSE)
    }
    
    #--------------------------------------------------------------------------
    # new reset attempt
    data_source <- reactive({
      if (isTRUE(input$example)) {
        "example"
      } else if (!is.null(input$tifs)) {
        "upload"
      } else {
        NULL
      }
    })
    
    # subsetted or not?
    subset_poly  <- reactiveVal(NULL)   # the crop polygon currently applied (NULL = full extent)
    is_subsetted <- reactiveVal(FALSE)  # whether current results are from a subset
    
    
    
    observeEvent(data_source(), {
      
      
      cached_full$rasters          <- NULL
      cached_full$rasters_original <- NULL
      cached_full$rasters_change   <- NULL
      cached_full$areas            <- NULL
      
      # ---- Reset stored years immediately so example years don't bleed ----
      stored_years$years <- list()  
      
      # ---- Reset reactiveVals ----
      rasters(NULL)
      rasters_original(NULL)
      areas(NULL)
      
      # ---- Reset ALL dynamic inputs ----
      # years
      if (!is.null(tifs())) {
        lapply(seq_along(tifs()$name), function(i) {
          reset(ns(paste0("year_", i)))
        })
      }
      
      # other inputs
      reset(ns("selected_layer"))
      reset(ns("cat_source"))
      reset(ns("cat_file"))
      reset(ns("aux"))
      reset(ns("class"))
      
    })
    
    # --------------------------------------------------------------------------
    # Persistent year store — add near the top of your module server
    stored_years <- reactiveValues(years = list())
    
    # --------------------------------------------------------------------------
    # Reset + open modal whenever tifs() changes
    observeEvent(tifs(), {
      
      # Reset all processing and display state whenever new files are loaded
      rasters(NULL)
      rasters_original(NULL)
      rasters_change(NULL)
      areas(NULL)
      
      # Clear subset/draw state so stale polygons don't carry over
      subset_poly(NULL)
      is_subsetted(FALSE)
      
      # Clear the full-extent cache
      cached_full$rasters          <- NULL
      cached_full$rasters_original <- NULL
      cached_full$rasters_change   <- NULL
      cached_full$areas            <- NULL
      
      
      # Full reset every time tifs() changes — covers example↔upload switching
      fnames     <- tifs()$name          # NULL-safe: if tifs() is NULL, this errors
      fnames     <- basename(fnames)     # and the whole observer just stops, which is fine
      auto_years <- sapply(fnames, extract_year)
      
      stored_years$years <- setNames(
        lapply(seq_along(fnames), function(i) {
          yr <- auto_years[[i]]
          if (!is.null(yr) && !is.na(yr) && yr %in% 1750:2150) {
            as.character(yr)
          } else {
            NA_character_
          }
        }),
        paste0("year_", seq_along(fnames))
      )
      
      build_year_inputs()
    }, ignoreNULL = TRUE)   # ignoreNULL = TRUE so NULL tifs() doesn't trigger reset
    
    # --------------------------------------------------------------------------
    # Save inputs → store on Confirm, then close
    observeEvent(input$confirm_years, {
      req(tifs())
      fnames <- basename(tifs()$name)
      for (i in seq_along(fnames)) {
        key <- paste0("year_", i)
        val <- input[[key]]    # numericInput returns a number or NA
        stored_years$years[[key]] <- if (!is.null(val) && !is.na(val)) {
          as.character(as.integer(val))
        } else {
          NA_character_
        }
      }
      removeModal()
    })
    
    # Re-open from sidebar
    observeEvent(input$open_year_modal, {
      build_year_inputs()
    })
    
    #--------------------------------------------------------------------------
    # REACTIVE WRAPPER FOR INPUT TIFFS OR EXAMPLE FILES ----
    tifs <- reactive({
      
      # If user ticked "use example"
      if (isTRUE(input$example)) {
        
        # Example files inside ./www/example/
        example_files <- list.files(
          "www/examples/",
          full.names = TRUE,
          pattern = ".tif"
        )
        
        # Mimic tifs() data frame
        return(
          data.frame(
            name      = basename(example_files),
            size      = file.info(example_files)$size,
            type      = "image/tiff",
            datapath  = example_files,
            stringsAsFactors = FALSE
          )
        )
      }
      
      ## Otherwise use uploaded files
      key <- file_key()
      input[[paste0("tifs_", key)]]
      
    })
    
    
    # reactive variables to store data ----------------------------------------
    rasters          <- reactiveVal(NULL)
    rasters_original <- reactiveVal(NULL)
    areas            <- reactiveVal(NULL)
    rasters_change <- reactiveVal(NULL)
    
    
    cached_full <- reactiveValues(
      rasters          = NULL,
      rasters_original = NULL,
      rasters_change   = NULL,
      areas            = NULL
    )
    
    # reactive for layer names --------- --------------------------------------
    lyr_names <- reactive({
      req(tifs())
      print(tifs())
      names(rast(tifs()$datapath))
    })
    
    # Reactive year inputs ----------------------------------------------------
    year_inputs <- reactive({
      req(tifs())
      sapply(seq_along(tifs()$name), function(i) {
        val <- input[[paste0("year_", i)]]
        if (!is.null(val) && !is.na(val)) as.character(as.integer(val)) else NA_character_
      })
    })
    
    # Reactive selected layer -------------------------------------------------
    selected_layer <- reactive({
      input$selected_layer
    })
    
    # Reactive process button ------------------------------------------------
    process_btn <- reactive({
      input$process
    })
    process_btn <- reactive(process_trigger())
    
    # Helper reactives to avoid repeated work -------------------------------------
    r_rast <- reactive({
      req(tifs())
      try(rast(tifs()$datapath[1]), silent = TRUE)
    })
    
    r_freq <- reactive({
      if (inherits(r_rast(), "try-error")) return(NULL)
      try(freq(r_rast()), silent = TRUE)
    })
    
    # --------------------------------------------------------------------------
    # TRUE when the uploaded raster(s) use min/opt/max layer names.
    # All downstream logic branches on this single reactive.
    is_bound_raster <- reactive({
      r <- r_rast()
      if (inherits(r, "try-error")) return(FALSE)
      identical(sort(names(r)), c("max", "min", "opt"))
    })
    
    # --------------------------------------------------------------------------
    # Conditionally render years ui
    output$year_sidebar_section <- renderUI({
      
      req(tifs())
      fnames <- basename(tifs()$name)
      
      # extract years
      years_vec <- sapply(seq_along(fnames), function(i) {
        key <- paste0("year_", i)
        yr <- stored_years$years[[key]]
        
        if (is.null(yr) || is.na(yr) || !nzchar(yr) || yr == "NA") {
          return(Inf)   # put missing years at bottom
        }
        
        as.numeric(yr)
      })
      
      # order by year
      ord <- order(years_vec, fnames)
      
      tags$div(
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;
             margin-bottom:4px;",
          tags$span(
            tags$strong(style = "font-size:14px;", "2) Check year:"),
            tooltip(bs_icon("info-circle"),
                    "Each map requires a year input before the 'process button' is available")
          ),
          actionButton(
            ns("open_year_modal"),
            label = tagList(icon("pencil-alt"), " Edit"),
            class = "btn-xs btn-outline-secondary",
            style = "padding:2px 8px; font-size:11px;"
          )
        ),
        tags$div(
          style = "border-radius:4px; padding:5px 6px;",       # transparent: no background/border
          tags$div(
            style = "display:flex; justify-content:space-between; font-size:11px;
               font-weight:bold; color:#888; padding:2px 4px 5px;
               border-bottom:1px solid #ccc; margin-bottom:3px;",
            tags$span("File"), tags$span("Year")
          ),
          tagList(lapply(ord, function(i) {
            key    <- paste0("year_", i)
            yr     <- stored_years$years[[key]]
            valid  <- !is.null(yr) && !is.na(yr) && nzchar(yr) && yr != "NA"
            colour <- if (valid) "#27ae60" else "#c0392b"
            label  <- if (valid) yr else "--"
            
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center;
                 font-size:12px; padding:3px 4px; border-bottom:1px dotted #ccc;",
              tags$span(
                style = "color:#444; overflow:hidden; text-overflow:ellipsis;
                   white-space:nowrap; max-width:72%; display:inline-block;",
                title = fnames[i],
                fnames[i]
              ),
              tags$span(
                style = paste0("color:", colour, "; font-weight:bold; white-space:nowrap;"),
                label
              )
            )
          }))
        ),
        tags$hr(style = "margin:8px 0;")
      )
    })
    
    # --------------------------------------------------------------------------
    # Conditionally render dropdown
    output$ecosystem_dropdown <- renderUI({
      req(tifs())
      
      # Return NULL if raster loading failed
      if (inherits(r_rast(), "try-error")) return(NULL)
      
      # # ---- min/opt/max bound raster: no layer selection needed ---------------
      if (!is_bound_raster()) {
      #   return(tagList(
      #     tags$div(
      #       class = "alert alert-info",
      #       style = "font-size:13px; padding:8px; margin-bottom:6px;",
      #       bs_icon("info-circle"), " ",
      #       tags$strong("Min / Opt / Max layers detected."),
      #       " All three bounds will be processed automatically — no layer selection required."
      #     ),
      #     div(style = "border-top: 1px solid #ccc; margin: 0;")
      #   ))
     # return(NULL)
      # }
      if (NROW(r_freq()) > 1) {
        if (nlyr(r_rast()) > 1) {
          # # Multiple raster layers
          # years <- sapply(seq_along(tifs()$name),
          #                 function(i) input[[paste0("year_", i)]])
          tagList(
            selectInput(
              ns("selected_layer"),
              tags$strong(style = "font-size:14px;"
                          ,"3) Multiple data layers detected - select the layer/ecosystem required:"),
              
              # choices = lyr_names(),
              choices = c("Select a layer..." = "", lyr_names()),
              selected = NULL#lyr_names()[1]
            ),
            div(style = "border-top: 1px solid #ccc; margin: 0;")) # was hr()
          
        } else {
          # Multiple categories detected
          vals_chr <- as.character(r_freq()$value)
          
          tagList(
            # 3) Category source choice
            radioButtons(
              ns("cat_source"),
              tags$strong(style = "font-size:14px;","3a) Do you have a file describing the category levels?"),
              choices = c(
                "Yes – I have a .aux.xml file" = "xml",
                "Yes – I have a 2-column .csv file" = "csv",
                "No – use raster values only"  = "raster"
              ),
              selected = "raster"
            ),
            
            # 3b) Dynamic UI
            uiOutput(ns("category_ui")),
            
            # add horizontal rule
            div(style = "border-top: 1px solid #ccc; margin: 0;")
          )
          
        }
      }
      
      }
    })
    
    # --------------------------------------------------------------------------
    # create category ui
    output$category_ui <- renderUI({
      req(input$cat_source)
      
      if (input$cat_source == "xml") {
        tagList(
          fileInput(
            ns("aux"),  
            tags$strong(style = "font-size:14px;","3b) Upload corresponding .aux.xml file"),
            accept = ".xml"),
          uiOutput(ns("class_ui"))
        )
        
      } else if (input$cat_source == "csv") {
        tagList(
          fileInput(
            ns("cat_file"),  tags$strong(style = "font-size:14px;","3b) Upload corresponding 2-column .csv file"), accept = ".csv"),
          #uiOutput(ns("column_preview_ui")),
          uiOutput(ns("class_ui"))
        )
        
      } else {
        # Raster-only categories
        vals_chr <- as.character(r_freq()$value)
        
        selectInput(
          ns("selected_layer"),
          tags$strong(style = "font-size:14px;","3b) Select the class/ecosystem required:"),
          choices  = vals_chr,
          selected = vals_chr[1]
        )
      }
    })
    
    # --------------------------------------------------------------------------
    # Render the class dropdown if an auxiliary file is uploaded
    output$class_ui <- renderUI({
      req(cat_table())  # cat_table() should read/process the uploaded XML
      selectInput(
        ns("class"),
        "Select class",
        choices = setNames(cat_table()$value, cat_table()$label)
      )
    })
    
    # --------------------------------------------------------------------------
    # Parse .aux.xml to get category table
    cat_table <- reactive({
      req(input$cat_source)
      
      # --------------------------
      if (input$cat_source == "xml") {
        req(input$aux)
        
        xml <- xml2::read_xml(input$aux$datapath)
        
        # --------------------------
        # Try GDALRasterAttributeTable first
        rows <- xml2::xml_find_all(xml, ".//GDALRasterAttributeTable//Row")
        
        if (length(rows) > 0) {
          
          value <- as.numeric(xml2::xml_text(xml2::xml_find_all(rows, ".//F[1]")))
          label <- xml2::xml_text(xml2::xml_find_all(rows, ".//F[2]"))
          
          df <- data.frame(value = value,
                           label = label,
                           stringsAsFactors = FALSE)
          
        } else {
          
          # --------------------------
          # Fallback: CategoryNames only
          categories <- xml2::xml_find_all(xml, ".//CategoryNames//Category")
          
          # Extract text
          label <- trimws(xml2::xml_text(categories))
          
          # Remove empty / blank entries
          label <- label[nzchar(label)]
          
          # Optional: remove duplicates
          label <- unique(label)
          
          raster_vals <- sort(terra::freq(r_rast)[,1])
          
          if (length(label) != length(raster_vals)) {
            validate(
              paste(
                "XML category count (", length(label),
                ") does not match raster value count (",
                length(raster_vals), ").",
                "Manual matching required."
              )
            )
          }
        }
      }
      
      # --------------------------
      # CSV
      else if (input$cat_source == "csv") {
        
        req(input$cat_file)
        
        df_raw <- read.csv(input$cat_file$datapath, stringsAsFactors = FALSE)
        
        if (ncol(df_raw) < 2) {
          validate("File must contain at least two columns.")
        }
        
        # ---- Auto-detect numeric/factor column (raster values)
        numeric_col <- which(sapply(df_raw, function(x)
          is.numeric(x) || is.integer(x) || is.factor(x)))
        
        # ---- Auto-detect character column (labels)
        char_col <- which(sapply(df_raw, is.character))
        
        if (length(numeric_col) == 0 || length(char_col) == 0) {
          validate("Could not detect numeric and character columns.")
        }
        
        value_col <- numeric_col[1]
        label_col <- char_col[1]
        
        df <- data.frame(
          value = as.numeric(df_raw[[value_col]]),
          label = as.character(df_raw[[label_col]]),
          stringsAsFactors = FALSE
        )
      }
      
      else {
        return(NULL)
      }
      
      df
    })
    
    
    # --------------------------------------------------------------------------
    # resolved selection
    # resolved_selection <- eventReactive(process_btn(), {
    #   # If single-value raster, just return the raster value or first layer
    #   r <- r_rast()
    #   if (nlyr(r) == 1 && length(terra::unique(r)[,1]) <= 1) {
    #     return(as.character(terra::unique(r)[,1]))
    #   }
    #   
    #   # For categorical rasters or XML/CSV categories
    #   if (!is.null(input$cat_source) && input$cat_source %in% c("csv","xml")) {
    #     req(input$class)
    #     return(as.numeric(input$class))
    #   } else if (!is.null(input$selected_layer)) {
    #     return(input$selected_layer)
    #   } else {
    #     return(names(r)[1]) # fallback
    #   }
    # })
    
    resolved_selection <- eventReactive(process_btn(), {
      
      r <- r_rast()
      req(r)
      
      # ---- 1. special case: min/opt/max → no selection needed ----
      if (identical(sort(names(r)), c("max","min","opt"))) {
        return(NULL)
      }
      
      # ---- 2. categorical via CSV/XML ----
      if (!is.null(input$cat_source) && input$cat_source %in% c("csv","xml")) {
        req(input$class)
        return(as.numeric(input$class))
      }
      
      # ---- 3. layer selection ----
      if (!is.null(input$selected_layer) && nzchar(input$selected_layer)) {
        return(input$selected_layer)
      }
      
      # ---- 4. fallback ----
      return(NULL)
    })
    
    # --------------------------------------------------------------------------
    # Dynamic year inputs
    extract_year <- function(filename) {
      m <- regexpr("(?<!\\d)(19|20)\\d{2}(?!\\d)", filename, perl = TRUE)
      if (m[1] == -1) return(NULL)
      as.numeric(regmatches(filename, m))
    }
    
    # --------------------------------------------------------------------------
    # build_year_inputs — reads from stored_years so edits persist on reopen
    build_year_inputs <- function() {
      req(tifs())
      fnames <- basename(tifs()$name)
      
      rows <- lapply(seq_along(fnames), function(i) {
        key <- paste0("year_", i)
        sel <- stored_years$years[[key]]
        
        # Convert to numeric for numericInput; NA renders as empty
        yr_num <- suppressWarnings(as.numeric(sel))
        yr_val <- if (!is.null(yr_num) && !is.na(yr_num) &&
                      yr_num %in% 1750:2150) yr_num else NA
        
        valid <- !is.na(yr_val)
        badge <- if (valid) {
          tags$span(style = "color:#27ae60; font-weight:bold; font-size:11px;",
                    "\u2713 detected")
        } else {
          tags$span(style = "color:#c0392b; font-weight:bold; font-size:11px;",
                    "\u26a0 not found \u2014 enter manually")
        }
        
        tags$tr(
          style = "border-bottom:1px solid #f0f0f0;",
          tags$td(
            style = "padding:8px 10px; vertical-align:middle; width:60%;",
            tags$div(style = "font-size:13px; word-break:break-all;", fnames[i]),
            tags$div(style = "margin-top:2px;", badge)
          ),
          tags$td(
            style = "padding:8px 10px; vertical-align:middle;",
            numericInput(           # <-- replaces selectInput entirely
              inputId = ns(key),
              label   = NULL,
              value   = yr_val,
              min     = 1750,
              max     = 2150,
              step    = 1,
              width   = "110px"
            )
          )
        )
      })
      
      # show the modal of editable years - see helpers/modals.R
      showModal(years_modal(rows, ns))
    }
    
    
    
    # Render a compact summary in the sidebar (refreshes on confirm)
    output$year_summary_sidebar <- renderUI({
      req(tifs())
      
      input$confirm_years  # also refresh on confirm
      
      fnames <- basename(tifs()$name)
      
      # Build reactive dependency on ALL year inputs so sidebar
      # updates live even without pressing Confirm
      year_vals <- lapply(seq_along(fnames), function(i) {
        input[[ns(paste0("year_", i))]]
      })
      
      header <- tags$div(
        style = "display:flex; justify-content:space-between;
             font-size:11px; font-weight:bold; color:#888;
             padding:2px 4px 4px 4px; border-bottom:1px solid #ddd;
             margin-bottom:4px;",
        tags$span("Filename"),
        tags$span("Year")
      )
      
      rows <- lapply(seq_along(fnames), function(i) {
        yr_val <- year_vals[[i]]
        valid  <- !is.null(yr_val) && yr_val != "Select a year..."
        colour <- if (valid) "#27ae60" else "#c0392b"
        label  <- if (valid) yr_val    else "—"
        
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;
               font-size:12px; padding:3px 4px;
               border-bottom:1px dotted #eee;",
          tags$span(
            style = "color:#444; overflow:hidden; text-overflow:ellipsis;
                 white-space:nowrap; max-width:70%; display:inline-block;",
            title = fnames[i],   # tooltip shows full name on hover
            fnames[i]
          ),
          tags$span(
            style = paste0("color:", colour, "; font-weight:bold; white-space:nowrap;"),
            label
          )
        )
      })
      
      tags$div(
        style = "margin-top:6px; background:#f9f9f9;
             border:1px solid #e0e0e0; border-radius:4px; padding:6px;",
        header,
        tagList(rows)
      )
    })
    
    # --------------------------------------------------------------------------
    # Conditional hr after year selection
    output$hr_after_years <- renderUI({
      if (isTRUE(input$year_inputs))div(style = "border-top: 1px solid #ccc; margin: 0;")
    })
    
    # --------------------------------------------------------------------------
    # checkbox for description calculation
    output$run_description <- renderUI({
      req(tifs())
      tags$span(
        if (isTRUE(is_bound_raster())) {
          tagList(tags$strong(style = "font-size:14px;",
                              "3) Select here for calculation of descriptive variables"))
        } else if (nlyr(r_rast()) > 1 || NROW(r_freq()) > 1) {
          tagList(tags$strong(style = "font-size:14px;",
                              "4) Select here for calculation of descriptive variables"))
        } else {
          tagList(tags$strong(style = "font-size:14px;",
                              "3) Select here for calculation of descriptive variables"))
        },
        
        tooltip(bs_icon("info-circle"),
                "Uncheck this box to skip calculating descriptive variables (this will speed up processing time).",
                tagList(
                  tags$p("The following varibles will be calculated:"),
                  tags$ul(
                    tags$li(("Rainfall")), #tags$b
                    tags$li(("Temperature")),
                    tags$li(("Elevation")),
                    tags$li(("Country overlap")),
                    tags$li(("Global Ecosystem Typology"))
                  ))),
        checkboxInput(ns("run_description"),label = NULL, value = TRUE),
        div(style = "border-top: 1px solid #ccc; margin: 0;")
      )
    })
    
# =============================================================================
# condition process button text
# =============================================================================
    output$process_btn_text <- renderUI({
      # only display if there 
      req(tifs())
      
      tags$span(
        # ifelse(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1,
        #        tagList(tags$strong(style = "font-size:14px;",  "5) Click the process button below to start the ecosystem assessment: ")),
        #        tagList(tags$strong(style = "font-size:14px;", "4) Click the process button below to start the ecosystem assessment: "))),
        if (isTRUE(is_bound_raster())) {
          tagList(tags$strong(style = "font-size:14px;",
                              "4) Click the process button below to start the ecosystem assessment:"))
        } else if (nlyr(r_rast()) > 1 || NROW(r_freq()) > 1) {
          tagList(tags$strong(style = "font-size:14px;",
                              "5) Click the process button below to start the ecosystem assessment:"))
        } else {
          tagList(tags$strong(style = "font-size:14px;",
                              "4) Click the process button below to start the ecosystem assessment:"))
        },
        
        
        tooltip(bs_icon("info-circle"),
                "The 'Process ecosystem maps' button will only be selectable when 
                a year for each classifcation map (section 2) has been entered."
        )
      )
    })
    
# =============================================================================
# condition process button
# =============================================================================
    output$process_btn_ui <- renderUI({
      req(tifs())
      
      # Check every year in the store: must be numeric and within 1750–2150
      years_valid <- all(sapply(seq_along(tifs()$name), function(i) {
        yr <- stored_years$years[[paste0("year_", i)]]
        if (is.null(yr) || is.na(yr) || !nzchar(yr) || yr == "NA") return(FALSE)
        yr_num <- suppressWarnings(as.numeric(yr))
        !is.na(yr_num) && yr_num >= 1750 && yr_num <= 2150
      }))
      
      # Layer selection also required when applicable, but NOT for bound rasters
      layer_valid <- is_bound_raster() || is.null(input$selected_layer) || nzchar(input$selected_layer)
      subset_drawn <- !is.null(drawn_poly()) || isTRUE(is_subsetted())
      
      tagList(
        actionButton(
          ns("process"),
          "Process ecosystem maps",
          disabled = !(years_valid && layer_valid) || subset_drawn
        )#,
        # if (subset_drawn) {
        #   tags$p(style = "font-size:11px; color:#c0392b; margin-top:4px;",
        #          "Revert to full extent before re-uploading new data.")
        # }
      )
    })
    
# =============================================================================
# conditional process subset ui and button
# =============================================================================
    output$refine_revert_ui <- renderUI({
      if (is.null(rasters())) return(NULL)
      if (is.null(drawn_poly()) && !isTRUE(is_subsetted())) return(NULL)
      
      tagList(
        # div(style = "border-top: 1px solid #ccc; margin: 10px 0;"),
        # tags$div(
        #   style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
        #   tags$strong(style = "font-size:14px;", "Refine analysis area"),
        #   actionLink(ns("show_draw_help"), tagList(bs_icon("info-circle"), " How to draw"),
        #              style = "font-size:12px;")
        # ),
        tags$span(
          if (isTRUE(is_bound_raster())) {
            tagList(tags$strong(style = "font-size:14px;",
                                "5) Refine Area of analysis (optional):"))
          } else if (nlyr(r_rast()) > 1 || NROW(r_freq()) > 1) {
            tagList(tags$strong(style = "font-size:14px;",
                                "6) Refine Area of analysis (optional)"))
          } else {
            tagList(tags$strong(style = "font-size:14px;",
                                "5) Refine Area of analysis (optional)"))
          },
    
          tooltip(bs_icon("info-circle"),
                  "Optionally subset the ecosytem map to assess an area of interest."
          )
            # actionLink(ns("show_draw_help"), tagList(bs_icon("info-circle"), " How to draw"),
            #            style = "font-size:12px;")
        ),
        
        
        if (!is.null(drawn_poly())) {
          actionButton(ns("refine_area"), "Re-process refined area",
                       icon = icon("crop"), style = "width:100%; margin-top:4px;",
                       class = "btn-warning")
        },
        # tags$p(style = "font-size:11px; color:#c0392b; margin-top:4px;",
        #        "Revert to full extent before re-uploading new data."),
        if (isTRUE(is_subsetted())) {
          actionButton(ns("revert_area"), "Revert to full extent",
                       icon = icon("undo"), style = "width:100%; margin-top:4px;",
                       class = "btn-outline-secondary")
        }
      )
    })
    
    
    
    

    
    
    
    
    
    
    
    
    # -----------------------------------------------------------------------------
    # process rasters 
    # -----------------------------------------------------------------------------
    
    
    # check_resolution <- function(file){
    #   r <- rast(file)
    #   res_m <- res(project(r, "EPSG:3857"))  # metres
    #   min_res <- min(res_m)
    #   if (min_res < 100) {
    #     return(list(
    #       ok = FALSE,
    #       res = round(min_res, 2)
    #     ))
    #   }
    #   
    #   list(ok = TRUE, res = round(min_res, 2))
    # }

    # Single trigger — incremented by any of the three buttons
    process_trigger <- reactiveVal(0)
    
    observeEvent(input$process, {
      process_trigger(isolate(process_trigger()) + 1)
    })
    
    observeEvent(input$refine_area, {
      req(drawn_poly())
      subset_poly(drawn_poly())
      is_subsetted(TRUE)
      process_trigger(isolate(process_trigger()) + 1)
    })
    
    observeEvent(input$revert_area, {
      req(!is.null(cached_full$rasters))   # cache must exist
      
      subset_poly(NULL)
      is_subsetted(FALSE)
      drawn_poly(NULL)                        # signals description module to clear map
      
      # Restore directly from cache — no reprocessing
      rasters(cached_full$rasters)
      rasters_original(cached_full$rasters_original)
      rasters_change(cached_full$rasters_change)
      areas(cached_full$areas)
      
      # Do NOT increment process_trigger here
    })
    
    

# =============================================================================
# Process uploaded TIFs when the process_button is clicked
# =============================================================================
    observeEvent(process_trigger(), {
      req(process_trigger() > 0)   # don't fire on initialisation (value = 0)
      req(tifs())
      files <- tifs()$datapath
      
      
      #print(is_bound_raster())
      
      
      # check <- check_resolution(tifs()$datapath)
      # if (!check$ok) {
      #   
      #   showModal(modalDialog(
      #     title = "Raster too detailed",
      #     paste0(
      #       "Uploaded raster resolution is ",
      #       check$res,
      #       " m.\n\n",
      #       "Please upload an aggregated raster (≥100 m cells)."
      #     ),
      #     easyClose = TRUE
      #   ))
      #   
      #   return(NULL)
      # }
      # 
      # show modal popup
      showModal(upload_modal)
      
      # ------------------------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = length(files))
      progress$set(message = "Processing classification maps...", value = 0)
      
      # ------------------------------------------------------------------------
      # reset previous data
      areas(NULL)
      rasters(NULL)         
      rasters_original(NULL)
      rasters_change(NULL)
      
      # ------------------------------------------------------------------------
      # create some variables + ensure year order is correct
      years <- year_inputs()               # see app.R server for year_inputs
      area_vals <- numeric(length(files))
      order_idx <- order(as.numeric(years))
      files <- files[order_idx]
      years <- years[order_idx]
      
      
      # ------------------------------------------------------------------------
      # identify selected layer + whether this is a bound raster
      sel_name          <- resolved_selection()
      is_bound_raster_val <- is_bound_raster()   # snapshot — don't call reactive inside loop
      
      # ------------------------------------------------------------------------
      # layer selection or mask function
      select_layer_or_mask <- function(r_all, sel_name) {
        # If no selection provided, return first layer or whole raster
        if (is.null(sel_name) || !nzchar(sel_name)) {
          return(if (nlyr(r_all) == 1) r_all[[1]] else r_all[[1]])
        }
        
        # Case A: sel_name matches a layer name
        if (sel_name %in% names(r_all)) {
          print("Case A")
          return(r_all[[sel_name]])
        }
        
        # Case B/C: single-layer raster
        if (nlyr(r_all) == 1) {
          print("Case B/C - getting uvals")
          r_single <- r_all[[1]]
          # uvals <- unique(values(r_single))
          # uvals <- uvals[!is.na(uvals)]
          uvals <- terra::unique(r_single)[,1]
          uvals <- uvals[!is.na(uvals)]
          
          # Case C: single category → binary mask
          if (length(uvals) == 1) {
            print("Case C - creating file")
            # return(ifel(!is.na(r_single), 1, NA))
            # r_single <- terra::ifel(!is.na(r_single), 1, NA,
            #                         filename = tempfile(fileext = ".tif"), overwrite = TRUE)
            return(r_single)
          }
          
          # Try numeric match
          sel_num <- suppressWarnings(as.numeric(sel_name))
          # if (!is.na(sel_num)) {
          #   return(ifel(r_single == sel_num, 1, NA))
          # }
          if (!is.na(sel_num)) {
            print("Case C - creating file for selected name")
            r_single <- terra::ifel(r_single == sel_num, 1, NA,
                                    filename = tempfile(fileext = ".tif"), overwrite = TRUE)
            return(r_single)
          }
          
          
          # Try factor levels or fallback to string match
          lev <- levels(r_single)
          if (!is.null(lev) && length(lev) >= 1 && nrow(lev[[1]]) > 0) {
            print("Case C - attempting factor levels")
            lev_tbl <- lev[[1]]
            lbl_col <- intersect(c("label", "levels", "Labels", "Level"), names(lev_tbl))
            val_col <- intersect(c("value", "ID", "Values", "Class"), names(lev_tbl))
            if (length(lbl_col) >= 1 && length(val_col) >= 1) {
              idx <- match(sel_name, lev_tbl[[lbl_col[1]]])
              if (!is.na(idx)) {
                v <- lev_tbl[[val_col[1]]][idx]
                # return(ifel(r_single == v, 1, NA))
                r_single <- terra::ifel(r_single == v, 1, NA,
                                        filename = tempfile(fileext = ".tif"), overwrite = TRUE)
                return(r_single)
                
              }
            }
          }
          print("fallback")
          # Fallback: compare as string
          # return(app(r_single, fun = function(x) ifelse(x == sel_name, 1, NA), cores = 1))
          r_single <- terra::ifel(r_single == sel_name, 1, NA,
                                  filename = tempfile(fileext = ".tif"), overwrite = TRUE)
          return(r_single)
          
        }
        
        # Multi-layer but invalid selection → fallback to first layer
        return(r_all[[1]])
      }
      
      
      # loop through each file to calculate areas ------------------------------
      for (i in seq_along(files)) {
        print("going through files ")
        r_all <- rast(files[i])
        
        # ---- Crop to subset polygon if one is active ----
        if (!is.null(subset_poly())) {
          crop_vect <- terra::project(subset_poly(), terra::crs(r_all))

        #   # Guard: if extents don't overlap (e.g. new upload in different region)
        #   # clear the stale polygon and fall back to full extent silently
        #   if (!terra::relate(terra::ext(r_all), terra::ext(crop_vect), "intersects")) {
        #     subset_poly(NULL)
        #     is_subsetted(FALSE)
        #   } else {
        #     r_all <- terra::crop(r_all, crop_vect)
        #     r_all <- terra::mask(r_all, crop_vect)
        #   }
        # }

          if (!terra::relate(terra::ext(r_all), terra::ext(crop_vect), "intersects")) {
            subset_poly(NULL)
            is_subsetted(FALSE)
          } else {
            r_all <- terra::mask(r_all, crop_vect)   # cells outside any polygon → NA

          }
        }
        

        
        
        
        # ----------------------------------------------------------------------
        # BRANCH A: min/opt/max bound raster — calculate area for all 3 bounds
        # ----------------------------------------------------------------------
        if (is_bound_raster_val) {
          
          bound_names <- c("min", "opt", "max")
          
          for (b in bound_names) {
            r_b <- r_all[[b]]
            
            if (global(r_b, "isNA") == 0) {
              r_b <- classify(r_b, matrix(c(0, NA), ncol = 2))
            }
            
            if (!is.na(crs(r_b)) && !is.lonlat(r_b)) {
              cell_area <- prod(res(r_b))
              n_cells   <- terra::global(r_b, "sum", na.rm = TRUE)[1, 1]
              area_m2   <- n_cells * cell_area
            } else {
              area_m2 <- expanse(r_b, unit = "m")[, 2]
            }
            
            area_df_b <- data.frame(
              Year     = years[i],
              Bound    = b,
              Area_ha  = area_m2 / 10000,
              Area_km2 = area_m2 / 1e6
            )
            
            areas(if (is.null(areas())) area_df_b else rbind(areas(), area_df_b))
          }
          
          # Use "opt" layer as the representative raster for display / change map
          r_orig <- r_all[["opt"]]
          if (global(r_orig, "isNA") == 0) {
            r_orig <- classify(r_orig, matrix(c(0, NA), ncol = 2))
          }
          
        } else {
          # ----------------------------------------------------------------------
          # BRANCH B: standard single-layer / categorical path (existing logic)
          # ----------------------------------------------------------------------
          r_orig <- select_layer_or_mask(r_all, sel_name)
          
          print("select layer complete")
          if (global(r_orig, "isNA") == 0) {
            print("going through files - 1st if")
            r_orig <- classify(r_orig, matrix(c(0, NA), ncol = 2))
          }
          
          print("doing mask")
          print("mask complete")
          
          if (!is.na(crs(r_orig)) && !is.lonlat(r_orig)) {
            print("going through files - 2nd if")
            cell_area <- prod(res(r_orig))
            n_cells   <- terra::global(r_orig, "sum", na.rm = TRUE)[1, 1]
            area_vals[i] <- n_cells * cell_area
          } else {
            print("area calc")
            area_vals[i] <- expanse(r_orig, unit = "m")[, 2]
          }
          
          print(area_vals)
          
          area_df <- data.frame(
            Year     = years[i],
            Area_ha  = area_vals[i] / 10000,
            Area_km2 = area_vals[i] / 1e6
          )
          
          if (is.null(areas())) {
            areas(area_df)
          } else {
            print("areas 1")
            print(areas())
            areas(rbind(areas(), area_df))
          }
          
          print("areas all")
          print(areas())
          
        } # end BRANCH B
        

        # Only sample the most recent raster (last in sequence)
        # r_orig is either the standard layer (B) or the "opt" layer (A)
        if (i == length(files)) {
          print("sampling raster")
          names(r_orig) <- years[i]
          
          # Sample latest raster — defines the display grid for all further work
          r_map <- spatSample(r_orig, 1000000, method = "regular", as.raster = TRUE, na.rm = TRUE)
          r_map <- tryCatch({
            project(r_map, "EPSG:4326")
          }, error = function(e) { crs(r_map) <- "EPSG:4326"; r_map })
          
          rasters(r_map)
          
          # Loss map: work entirely on small sampled rasters to avoid memory issues.
          # Loss = cell was present (value == 1) in oldest but absent (NA) in latest.
          if (length(files) > 1) {
            r_oldest_raw <- rast(files[1])
            r_oldest_raw <- if (is_bound_raster_val) {
              r_oldest_raw[["opt"]]
            } else {
              select_layer_or_mask(r_oldest_raw, sel_name)
            }
            
            # Sample oldest at same density BEFORE projecting — keeps memory low.
            # na.rm = FALSE retains absence/outside cells so loss areas stay visible.
            r_oldest_samp <- spatSample(r_oldest_raw, 1000000, method = "regular",
                                        as.raster = TRUE, na.rm = FALSE)
            r_oldest_samp <- tryCatch({
              project(r_oldest_samp, "EPSG:4326", method = "near")
            }, error = function(e) { crs(r_oldest_samp) <- "EPSG:4326"; r_oldest_samp })
            
            # Snap to r_map's exact grid — all further ops are on small rasters only
            r_oldest_grid <- resample(r_oldest_samp, r_map, method = "near")
            
            # Loss: present in oldest and absent (NA) in latest sample
            r_loss <- ifel(r_oldest_grid == 1 & is.na(r_map), 1L, NA)
          
            # ---- Crop to subset polygon if one is active ----
            if (!is.null(subset_poly())) {
              crop_vect <- terra::project(subset_poly(), terra::crs(r_all))
              
              if (!terra::relate(terra::ext(r_all), terra::ext(crop_vect), "intersects")) {
                subset_poly(NULL)
                is_subsetted(FALSE)
              } else {
                r_loss <- terra::mask(r_loss, crop_vect)   # cells outside any polygon → NA
              }
            }
            
            
            rasters_change(r_loss)
            rm(r_oldest_raw, r_oldest_samp, r_oldest_grid, r_loss)
            gc()
          }
        }
        
        
        
        # Clean up temporary files periodically on long runs
        if ((i %% 3) == 0) {
          try(terra::tmpFiles(remove = TRUE), silent = TRUE)
          gc()
        }
        
      } # end of for loop
      
      
      # store data in relevant reactive variables ------------------------------
      rasters(r_map)
      rasters_original(r_orig)
      # rasters_original(tifs()$datapath[length(tifs()$datapath)])
      years(year_inputs())
      
      # Store full-extent results for instant revert
      if (is.null(subset_poly())) {
        cached_full$rasters          <- rasters()
        cached_full$rasters_original <- rasters_original()
        cached_full$rasters_change   <- rasters_change()
        cached_full$areas            <- areas()
      }
      
      
      # hide progress once done ------------------------------------------------
      progress$close()
      
      # removeModal()
      if (!isTRUE(input$run_description)) {
        removeModal()    # no descriptive vars — close immediately
        if (is.null(subset_poly())) showModal(draw_instructions_modal) # If run_description TRUE, description module handles removeModal + showModal

      }

      # currently this modal is showign too early - hash out for now      
      # # Show drawing instructions only after a full-extent run
      # if (is.null(subset_poly())) {
      #   showModal(draw_instructions_modal)
      # }
      
      
    })
    
    # this works, but just below is attemtp to hold back teh modal creation....
    # output$refine_revert_ui <- renderUI({
    #   # req(rasters()) # Only show after initial process has run
    #   # req(!is.null(drawn_poly()) || isTRUE(is_subsetted()))  # hides when no draw and not subsetted
    #   
    #   # Explicit NULL return clears the UI — req() only silently keeps old content
    #   if (is.null(rasters())) return(NULL)
    #   if (is.null(drawn_poly()) && !isTRUE(is_subsetted())) return(NULL)
    # 
    #   
    #   tagList(
    #     div(style = "border-top: 1px solid #ccc; margin: 10px 0;"),
    #     
    #     # Refine button — visible when a polygon has been drawn
    #     if (!is.null(drawn_poly())) {
    #       tagList(
    #         tags$span(
    #           tags$strong(style = "font-size:14px;",
    #                       if (is_subsetted()) "Re-process refined area" else "Process refined area"),
    #           tooltip(bs_icon("info-circle"),
    #                   "Re-run the analysis cropped to the polygon drawn on the map.")
    #         ),
    #         actionButton(ns("refine_area"), "Re-process refined area",
    #                      icon  = icon("crop"),
    #                      style = "width:100%; margin-top:4px;",
    #                      class = "btn-warning")
    #       )
    #     },
    #     
    #     # Revert button — visible only when currently showing a subset
    #     if (isTRUE(is_subsetted())) {
    #       tagList(
    #         tags$span(
    #           tags$strong(style = "font-size:14px; margin-top:8px; display:block;",
    #                       "Revert to full extent"),
    #           tooltip(bs_icon("info-circle"),
    #                   "Re-run the analysis on the original full-extent rasters.")
    #         ),
    #         actionButton(ns("revert_area"), "Revert to full extent",
    #                      icon  = icon("undo"),
    #                      style = "width:100%; margin-top:4px;",
    #                      class = "btn-outline-secondary")
    #       )
    #     }
    #   )
    # })
    

    
    observeEvent(input$show_draw_help, {
      showModal(draw_instructions_modal)
    })
    
    
    # # Refine: apply the drawn polygon and reprocess
    # observeEvent(input$refine_area, {
    #   req(drawn_poly())
    #   subset_poly(drawn_poly())
    #   is_subsetted(TRUE)
    #   # Trigger reprocessing via the same process button reactive chain
    #   # by directly calling the processing logic — see note below
    #   shinyjs::click(ns("process"))   # or use a shared processing function
    # })
    # # 
    # # # Revert: clear the subset and reprocess on full extent
    # # observeEvent(input$revert_area, {
    # #   subset_poly(NULL)
    # #   is_subsetted(FALSE)
    # #   shinyjs::click(ns("process"))
    # # })
    # 
    # observeEvent(input$revert_area, {
    #   req(!is.null(cached_full$rasters))   # cache must exist
    #   
    #   subset_poly(NULL)
    #   is_subsetted(FALSE)
    #   
    #   # Restore directly from cache — no reprocessing
    #   rasters(cached_full$rasters)
    #   rasters_original(cached_full$rasters_original)
    #   rasters_change(cached_full$rasters_change)
    #   areas(cached_full$areas)
    #   shinyjs::click(ns("process"))
    #   
    #   # Do NOT increment process_trigger here
    # })
    
    
  
    
    
   
    
    # --------------------------------------------------------------------------
    # return data
    return(list(
      rasters = rasters,
      rasters_original = rasters_original,
      rasters_change   = rasters_change,
      is_subsetted = is_subsetted,
      
      areas = areas,
      years = reactive(as.numeric(stored_years$years)),
      process = process_btn, 
      selected_layer = selected_layer,
      lyr_names = lyr_names,
      tifs             = tifs,
      example          = reactive(input$example),
      is_bound_raster  = is_bound_raster,              # NEW: lets criterion_a branch on data type
      run_description = reactive({input$run_description})
    ))
    
    
  })
}