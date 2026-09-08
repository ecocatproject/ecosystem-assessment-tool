# modules/classification_upload


# ============================================================================
# UI
# ============================================================================
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
    div(style = "border-top: 1px solid #ccc; margin: 0px 0;"), # margin was 10, set as 0 to reduce space
    
    # 5) remaining sidebar sections 
    # uiOutput(ns("part2")),
    uiOutput(ns("year_sidebar_section")),
    uiOutput(ns("ecosystem_dropdown")),
    uiOutput(ns("run_description")),
    uiOutput(ns("process_btn_text")),
    uiOutput(ns("process_btn_ui")),
    uiOutput(ns("subset_guide_ui")),
    div(style = "margin-top:-5px;", # -10 reduces all spacing between revert and above button
        uiOutput(ns("refine_revert_ui"))
    )
  )
}


# ============================================================================
# SERVER
# ============================================================================
classification_module_server <- function(id, drawn_poly=reactive(NULL), session_tmp = tempdir()) {
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
      } else if (!is.null(input$tifs)) { # input[[paste0("tifs_", file_key())]]
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
      
      # If tifs() is NULL (user unchecked example with no prior upload),
      # stop here — nothing left to build year inputs from.
      if (is.null(tifs())) {
        stored_years$years <- list()
        return()
      }
      
      # Full reset every time tifs() changes — covers example↔upload switching
      fnames     <- basename(tifs()$name)     # and the whole observer just stops, which is fine
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
    }, ignoreNULL = FALSE)   # ignoreNULL = TRUE so NULL tifs() doesn't trigger reset
    
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
    
    # Which files are ticked "Include" ---------------------------------------
    # Returns ORIGINAL file positions (1..N in tifs()$name) that are included.
    # Defaults to included when a checkbox hasn't registered yet (NULL), so a
    # fresh upload processes everything until the user unticks something.
    included_idx <- reactive({
      req(tifs())
      n <- length(tifs()$name)
      keep <- vapply(seq_len(n), function(i) {
        v <- input[[paste0("include_", i)]]
        is.null(v) || isTRUE(v)
      }, logical(1))
      which(keep)
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
        
        # Compact + truly centre the Shiny checkboxInput in the Include column.
        # Shiny emits BS3 (.checkbox) or BS5 (.form-check) markup depending on
        # the theme, and both reserve left padding for the (empty) label, which
        # pushes the box off-centre. Zero that out and drop the box into normal
        # flow so justify-content:center actually centres the visible control.
        tags$style(HTML("
          .es-year-list .shiny-input-container { margin:0; min-height:0; width:auto; }
          .es-year-list .checkbox,
          .es-year-list .form-check   { margin:0; min-height:0; padding-left:0; }
          .es-year-list .checkbox label { margin:0; min-height:0; padding-left:0; }
          .es-year-list input[type=checkbox],
          .es-year-list .form-check-input {
            position:static; float:none; margin:0;
          }
        ")),
        
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;
             margin-bottom:4px;",
          tags$span(
            tags$strong(style = "font-size:14px;", "2) Check year:"),
            tooltip(bs_icon("info-circle"),
                    "Each included map requires a year input before the 'process button' is available. Untick a map to leave it out of the assessment.")
          ),
          actionButton(
            ns("open_year_modal"),
            label = tagList(icon("pencil-alt"), " Edit"),
            class = "btn-xs btn-outline-secondary",
            style = "padding:2px 8px; font-size:11px;"
          )
        ),
        tags$div(
          class = "es-year-list",
          style = "border-radius:4px; padding:5px 6px;",       # transparent: no background/border
          # header row (3 columns: File | Year | Include)
          tags$div(
            style = "display:flex; align-items:center; font-size:11px;
               font-weight:bold; color:#888; padding:2px 4px 5px;
               border-bottom:1px solid #ccc; margin-bottom:3px;",
            tags$span(style = "flex:1 1 auto;",                   "File"),
            tags$span(style = "flex:0 0 40px; text-align:right;",  "Year"),
            tags$span(style = "flex:0 0 52px; text-align:center;", "Include")
          ),
          tagList(lapply(ord, function(i) {
            key    <- paste0("year_", i)
            yr     <- stored_years$years[[key]]
            valid  <- !is.null(yr) && !is.na(yr) && nzchar(yr) && yr != "NA"
            colour <- if (valid) "#27ae60" else "#c0392b"
            label  <- if (valid) yr else "--"
            
            # Preserve the checkbox state across re-renders WITHOUT creating a
            # reactive dependency (that would recreate the boxes on every year
            # edit and reset them). isolate() reads the current value; default
            # to TRUE (included) before the input has registered.
            cur      <- isolate(input[[paste0("include_", i)]])
            init_val <- if (is.null(cur)) TRUE else isTRUE(cur)
            
            tags$div(
              style = "display:flex; align-items:center;
                 font-size:12px; padding:3px 4px; border-bottom:1px dotted #ccc;",
              tags$span(
                style = "flex:1 1 auto; color:#444; overflow:hidden;
                   text-overflow:ellipsis; white-space:nowrap;",
                title = fnames[i],
                fnames[i]
              ),
              tags$span(
                style = paste0("flex:0 0 40px; text-align:right; color:", colour,
                               "; font-weight:bold; white-space:nowrap;"),
                label
              ),
              tags$div(
                style = "flex:0 0 52px; display:flex; justify-content:center;",
                checkboxInput(ns(paste0("include_", i)), label = NULL,
                              value = init_val, width = "auto")
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
    
    
    # =============================================================================
    # RESOLVED SELECITON
    # =============================================================================
    
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
    
    # =============================================================================
    # DYNAMIC YEAR INPUT SECTION
    # =============================================================================
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
                    "\u2713 year detected")
        } else {
          tags$span(style = "color:#c0392b; font-weight:bold; font-size:11px;",
                    "\u26a0 year not found \u2014 enter manually")
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
    
    
    # =============================================================================
    # checkbox for description calculation
    # =============================================================================
    output$run_description <- renderUI({
      req(tifs())
      
      # Step number prefix depends on raster type
      step <- if (isTRUE(is_bound_raster()) || !(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1)) "3" else "4"
      
      tags$div(
        style = "margin-top:0; line-height:1.2;",
        tags$strong(style = "font-size:14px;",
                    paste0(step, ") Select here for calculation of descriptive variables:")),
        
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
      
      # Step number prefix depends on raster type
      step <- if (isTRUE(is_bound_raster()) || !(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1)) "4" else "5"
      
      tags$span(
        tags$strong(style = "font-size:14px;",
                    paste0(step, ") Click the process button below to start the ecosystem assessment:")),
        tooltip(bs_icon("info-circle"),
                "Draw polygon(s) on the map to restrict the analysis to a specific area.")
      )
    })
    
    
    # =============================================================================
    # conditional process button
    # =============================================================================
    output$process_btn_ui <- renderUI({
      req(tifs())
      
      # Only the INCLUDED files need a valid year. Excluded files are ignored.
      idx <- included_idx()
      
      years_valid <- length(idx) >= 1 && all(sapply(idx, function(i) {
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
    # conditional guide note 
    # =============================================================================
    # Guide note: appears after processing, disappears once user starts refining
    output$subset_guide_ui <- renderUI({
      req(rasters())
      if (!is.null(drawn_poly()) || isTRUE(is_subsetted())) return(NULL)
      
      # # Step number prefix depends on raster type
      # step <- if (isTRUE(is_bound_raster()) || !(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1)) "5" else "6"
      
      div(
        style = "background:#f0f7ff; border-left:3px solid #669900;
                 padding:8px 10px; border-radius:3px;
                 font-size:12px; margin-top:2px; line-height:1.5; margin-bottom: -8px;",
        
        tagList(
          tags$i(class = "fas fa-info-circle", style = "color:#669900; margin-right:4px;"),
          tags$strong("Maps processed."),
          " Optionally refine your ecosystem area by applying an area filter after using the map drawing tools in the ",
          tags$strong('Description'),
          "tab."),
        tags$div(
          style = "display:flex; justify-content:flex-end; margin-top:6px;",
          actionLink(
            ns("show_draw_help"),
            tagList(bs_icon("pencil"), " How to refine"),
            style = "font-size:12px; text-decoration:none;
             border:1px solid #669900; border-radius:4px;
             padding:3px 8px; color:#669900;"
          )
        )
        
        # tags$i(class = "fas fa-arrow-down", style = "color:#669900;")
      )
    })
    
    
    
    
    # =============================================================================
    # conditional process subset ui and button
    # =============================================================================
    output$refine_revert_ui <- renderUI({
      req(tifs())
      if (is.null(rasters())) return(NULL)
      
      # Step number prefix depends on raster type
      step <- if (isTRUE(is_bound_raster()) || !(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1)) "5" else "6"
      
      tagList(
        
        # ---- Section A: Refine by area ----------------------------------
        # Only shown once a polygon has been drawn or a subset is active
        if (!is.null(drawn_poly()) || isTRUE(is_subsetted())) tagList(
          div(style = "border-top:1px solid #e0e0e0; margin:-6px 0 4px;"), # was 4, reduces teh gap between hr line and the process button(above it)
          tags$span(
            tags$strong(style = "font-size:14px;",
                        paste0(step, ") Refine by area (optional)")),
            tooltip(bs_icon("info-circle"),
                    "Draw polygon(s) on the map to restrict the analysis to a specific area.")
          ),
          if (!is.null(drawn_poly())) {
            actionButton(ns("refine_area"), "Apply area filter",
                         icon = icon("crop"),
                         style = "width:100%; margin-top:6px;",
                         class = "btn-warning")
          }
        ),
        
        # ---- Revert -----------------------------------------------------
        if (isTRUE(is_subsetted())) tagList(
          #div(style = "border-top:1px solid #e0e0e0; margin:8px 0 6px;"),
          actionButton(ns("revert_area"), "Revert to full extent",
                       icon = icon("undo"),
                       style = "width:100%; margin-top:4px;",
                       class = "btn-outline-secondary")
        )
      )
    })
    
    
    
    # =============================================================================
    # PROCESS RASTERS
    # =============================================================================
    
    check_ncells <- function(file){
      r <- rast(file)
      ncells <- ncell(r)  # metres
     
      if (ncells  > 40000) {
        return(list(
          ok = FALSE,
          res = ncells
        ))
      }

      list(ok = TRUE, res = ncells)
    }
    
    # small helper to render "417 million" style counts
    fmt_cells <- function(n) {
      if (n >= 1e9)      paste0(round(n / 1e9, 1), " billion")
      else if (n >= 1e6) paste0(round(n / 1e6),   " million")
      else               format(n, big.mark = ",")
    }
    
    # ---- add near your other reactiveVals ----
    ack_big <- reactiveVal(FALSE)   # user has acknowledged the big-raster warning
    
    # ---- per-file, robust; 40000 is a placeholder threshold (that's ~200x200!) ----
    check_ncells <- function(files, limit = 40000000) {
      ncells <- max(vapply(files, function(f) terra::ncell(terra::rast(f)), numeric(1)))
      list(ok = ncells <= limit, res = ncells)
    }

    
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
    observeEvent(process_btn(), {
      req(process_trigger() > 0)   # don't fire on initialisation (value = 0)
      req(tifs())
      
      # ---- Restrict to the files ticked "Include" ---------------------------
      # Build files AND years from the SAME integer index set so they can never
      # drift out of alignment or pick up NA paths. idx holds ORIGINAL positions
      # (1..N in tifs()$name); year_i / include_i inputs use those same indices.
      idx <- included_idx()
      req(length(idx) >= 1)                 # nothing ticked -> do nothing
      
      files <- tifs()$datapath[idx]
      
      # -----------------------------------------------------------------------
      # apply the ncell check
      # check <- check_ncells(tifs()$datapath)
      # if (!check$ok) {
      #   showModal(modalDialog(
      #     title = div(
      #       style = "text-align:center; padding-bottom:0.25rem; ",
      #       tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
      #              tagList(icon("exclamation-triangle"),
      #                      "The map files may be too large to process"))),
      # 
      #   tagList(
      #     tags$div(
      #       style = "padding:0.5rem 0.25rem;",
      #       
      #       tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; text-transform:uppercase; letter-spacing:0.05em; margin:0 0 0.9rem;",
      #              "The maps have", check$res, "cells - this will be cause the tool to be slow and impact functionality"),
      #       tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; text-transform:uppercase; letter-spacing:0.05em; margin:0 0 0.9rem;",
      #              "It is recommended to reduce the resolution of the data (aggreagate) or reduce the extent of area"),
      #       p(HTML(paste0("Reducing the resolution fixes this: memory falls with the ",
      #                     "<b>square</b> of cell size, so 30 m \u2192 90 m uses about ",
      #                     "9\u00d7 less.")))
      #     )
      #   ),
      #   easyClose = FALSE,
      #   footer = modalButton("Start")
      #   ))
      #   
      #   return(NULL)
      # }
      
      
      # big-raster warning — only if too large AND not already acknowledged
      check <- check_ncells(files)
      # if (!check$ok && !ack_big()) {
      #   showModal(modalDialog(
      #     title = div(
      #       style = "text-align:center; padding-bottom:0.25rem;",
      #       tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
      #              tagList(icon("exclamation-triangle"),
      #                      "The map files may be too large to process"))),
      #     tagList(
      #       tags$div(
      #         style = "padding:0.5rem 0.25rem;",
      #         tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; text-transform:uppercase; letter-spacing:0.05em; margin:0 0 0.9rem;",
      #                paste0("The maps have ", format(check$res, big.mark = ","),
      #                       " cells — this will increase processing time significantly. It may also impact functionality by queuing analysis depending on the number of concurrent users or cause the tool to crash")),
      #         tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
      #                "It is recommended to reduce the resolution (aggregate) or reduce the extent"),
      #         # p(HTML(paste0("Reducing the resolution fixes this: memory falls with the ",
      #         #               "<b>square</b> of cell size, so 30 m \u2192 90 m uses about ",
      #         #               "9\u00d7 less.")))
      #         tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
      #                "Future versions of this tool will provide instructions/functionality to process large data files"),
      #       )
      #     ),
      #     easyClose = FALSE,
      #     footer = tagList(
      #       modalButton("Cancel"),
      #       actionButton(ns("proceed_big"), "Process anyway",
      #                    style = "background:#669900; border-color:#669900; color:#fff;")
      #     )
      #   ))
      #   return(NULL)
      # }
      if (!check$ok && !ack_big()) {
        showModal(modalDialog(
          title = div(
            style = "text-align:center; padding-bottom:0.25rem;",
            tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
                   tagList(icon("exclamation-triangle"),
                           "Large dataset detected"))),
          tagList(
            tags$div(
              style = "padding:0.5rem 0.25rem;",
              # tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
              #        paste0("The uploaded maps contain ", fmt_cells(check$res),
              #               " cells, which exceeds the recommended size for the current shared processing capacity.")),
              tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
                     tagList("The uploaded maps contain ",
                             tags$b(paste0(fmt_cells(check$res), " cells")),
                             ", which exceeds the recommended size for the current shared processing capacity.")),
              tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
                     "Processing may be queued depending on the number of concurrent users, take longer to complete, or fail if computing resource limits are reached."),
              tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
                     "To improve performance, consider reducing the map resolution or study area extent, or processing smaller areas separately."),
              tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; letter-spacing:0.05em; margin:0 0 0.9rem;",
                     "Future releases of the RLE Assessment Tool will include further support for processing larger datasets."),
            )
          ),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("proceed_big"), "Process anyway",
                         style = "background:#669900; border-color:#669900; color:#fff;")
          )
        ))
        return(NULL)
      }
      
      # -----------------------------------------------------------------------
      # show modal popup
      showModal(upload_modal)
      
      # ------------------------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = length(files))
      progress$set(message = "Processing classification maps...", value = 0)
      # on.exit(try(progress$close(), silent = TRUE), add = TRUE)
      
      # ------------------------------------------------------------------------
      # reset previous data
      areas(NULL)
      rasters(NULL)         
      rasters_original(NULL)
      rasters_change(NULL)
      
      # ------------------------------------------------------------------------
      # create some variables + ensure year order is correct
      # Subset years with the SAME idx used for files above, so the two vectors
      # are the same length and element-for-element aligned. Order the subset by
      # year; both vectors are permuted with one shared order_idx.
      years     <- year_inputs()[idx]      # see app.R server for year_inputs
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
      
      # Load a raster, optimizing to INT1U if stored as float
      # Writes to a temp file if conversion is needed; otherwise loads as-is.
      optimize_raster <- function(file, target_dtype = "INT1U") {
        r <- terra::rast(file)
        current_dtype <- terra::datatype(r)
        if (current_dtype %in% c("FLT4S", "FLT8S", "FLT4U", "FLT8U")) {
          message(
            "Optimizing: ", basename(file),
            " (", current_dtype, " -> ", target_dtype, ")"
          )
          tmp <- tempfile(fileext = ".tif")
          terra::writeRaster(r, tmp,
                             datatype = target_dtype,
                             gdal = c("COMPRESS=LZW"),
                             overwrite = TRUE)
          r <- terra::rast(tmp)
        }
        r
      }
      
      
      
      
      # loop through each file to calculate areas ------------------------------
      for (i in seq_along(files)) {
        print("going through files ")
        progress$inc(1, detail = paste0(i, " of ", length(files)))
        r_all <- rast(files[i])
        
        print(r_all)
        print(mem_info(r_all))
        print(free_RAM())
        
        r_all <- optimize_raster(files[i])
        
        print(r_all)
        print(mem_info(r_all))
        print(free_RAM())
        
        # # ---- Crop to subset polygon if one is active ----
        if (!is.null(subset_poly())) {
          # poly      <- subset_poly()
          # crop_vect <- if (inherits(poly, "SpatVector")) poly else terra::vect(poly)
          # crop_vect <- terra::project(crop_vect, terra::crs(r_all))
          crop_vect <- subset_poly() |>
            # terra::vect() |>
            terra::project(terra::crs(r_all))
          
          # 1st check extents overlap and fall back to full extent silently
          if (!terra::relate(terra::ext(r_all), terra::ext(crop_vect), "intersects")) {
            subset_poly(NULL)
            is_subsetted(FALSE)
          } else {
            tmp_crop   <- file.path(session_tmp, paste0("crop_", i, ".tif"))
            tmp_result <- file.path(session_tmp, paste0("masked_", i, ".tif"))
            
            # 1. Crop to extent first — reduces object size before masking
            r_cropped <- terra::crop(r_all, crop_vect)
            terra::writeRaster(r_cropped, tmp_crop, overwrite = TRUE)
            r_cropped <- terra::rast(tmp_crop)
            rm(r_all); gc()
            
            # 2. Rasterize and mask on the smaller cropped object
            poly_mask <- terra::rasterize(crop_vect, r_cropped, field = 1, background = NA)
            terra::writeRaster(
              terra::mask(r_cropped, poly_mask),
              tmp_result, overwrite = TRUE
            )
            r_all <- terra::trim(terra::rast(tmp_result))
            rm(r_cropped, poly_mask); gc()
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
            
            # If a subset is active, crop the oldest raster to the polygon BEFORE sampling
            # so spatSample produces the same effective resolution as r_map.
            # Without this, the oldest is sampled across its full extent and ends up
            # blocky after resample() snaps it onto r_map's fine subset grid.
            if (!is.null(subset_poly())) {
              poly_native <- terra::project(subset_poly(), terra::crs(r_oldest_raw))
              if (terra::relate(terra::ext(r_oldest_raw),
                                terra::ext(poly_native), "intersects")) {
                r_oldest_raw <- terra::crop(r_oldest_raw, poly_native)
                r_oldest_raw <- terra::mask(r_oldest_raw, poly_native)
                r_oldest_raw <- terra::trim(r_oldest_raw)
              }
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
            
            rasters_change(r_loss)
            rm(r_oldest_raw, r_oldest_samp, r_oldest_grid, r_loss)
            gc()
          }
        }
        
        
        
        # Clean up temporary files periodically on long runs
        # if ((i %% 3) == 0) {
        #   try(terra::tmpFiles(remove = TRUE), silent = TRUE)
        #   gc()
        # }
        if ((i %% 3) == 0) {
          # Only remove THIS session's terra temp files, not the whole process's
          tf <- terra::tmpFiles(current = TRUE, remove = FALSE)
          unlink(tf[startsWith(tf, session_tmp)])
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
        show_draw <- is.null(subset_poly())        # read the reactive HERE, in the observer
        session$onFlushed(function() {
          removeModal(session = session)
          if (show_draw) showModal(draw_instructions_modal, session = session)
        }, once = TRUE)
      }
      
    })
    
    # ---- proceed-anyway: acknowledge, then re-fire the processing observer ----
    observeEvent(input$proceed_big, {     # <- paste here
      removeModal()
      ack_big(TRUE)
      process_trigger(process_trigger() + 1)
    })
    
    observeEvent(tifs(), {                # <- the reset from the last step
      ack_big(FALSE)
    }, ignoreInit = TRUE)
    
    # this works- attemtp to hold back teh modal creation....
    observeEvent(input$show_draw_help, {
      if(!is.null(rasters_original()))
      showModal(draw_instructions_modal)
    })
    # observeEvent(input$show_draw_help, {
    #   message("[classif] show_draw_help fired; modal in scope? ", exists("draw_instructions_modal"))
    #   req(exists("draw_instructions_modal"))
    #   showModal(draw_instructions_modal)
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
      # run_description = reactive({input$run_description})
      run_description = reactive({
        v <- input$run_description
        if (is.null(v)) TRUE else isTRUE(v)   # default TRUE until the dynamic input registers
      })
    ))
    
    
  })
}




