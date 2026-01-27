# modules/classification_upload


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

# UI  - sidebar only

  classification_module_ui <- function(id) {
    ns <- NS(id)
    sidebar(
      width = 300,
      fileInput(ns("tifs"), "1) Upload Ecosystem classification file(s)",
                multiple = TRUE, accept = ".tif"),
      hr(),
      uiOutput(ns("part2")),
      uiOutput(ns("year_inputs")),
      uiOutput(ns("hr_after_years")),
      uiOutput(ns("ecosystem_dropdown")),
      uiOutput(ns("process_btn_text")),
      uiOutput(ns("process_btn_ui"))
    )
  }


# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
classification_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive variables to store data ----------------------------------------
    rasters          <- reactiveVal(NULL)
    rasters_original <- reactiveVal(NULL)
    areas            <- reactiveVal(NULL)
    
    # reactive for layer namse --------- --------------------------------------
    lyr_names <- reactive({
      req(input$tifs)
      names(rast(input$tifs$datapath))
    })
    
    # Reactive year inputs ----------------------------------------------------
    year_inputs <- reactive({
      req(input$tifs)
      sapply(seq_along(input$tifs$name),
             function(i) input[[paste0("year_", i)]])
    })
    

    # Reactive selected layer -------------------------------------------------
    selected_layer <- reactive({
      input$selected_layer
    })
    
    # Reactive process button ------------------------------------------------
    process_btn <- reactive({
      input$process
    })
    
    
    # Helper reactives to avoid repeated work -------------------------------------
    r_rast <- reactive({
      req(input$tifs)
      try(rast(input$tifs$datapath[1]), silent = TRUE)
    })
    
    r_freq <- reactive({
      if (inherits(r_rast(), "try-error")) return(NULL)
      try(freq(r_rast()), silent = TRUE)
    })
    
    
    # Conditionally render dropdown ------------------------------------------
    output$ecosystem_dropdown <- renderUI({
      req(input$tifs)
      
      # Load the first tif to inspect its layers
      if (inherits(r_rast(), "try-error")) return(NULL)

      if (NROW(r_freq()) > 1) {
      # Only proceed if >1 layer
      if (nlyr(r_rast()) > 1) {
        # Check that all year inputs are filled in
        years <- sapply(seq_along(input$tifs$name),
                        function(i) input[[paste0("year_", i)]])
        tagList(
        selectInput(ns("selected_layer"),
                    "3) Multiple data layers detected - select the layer/ecosystem required:",
                    choices = lyr_names(),
                    selected = lyr_names()[1]),
        hr()
        )
      } else {
        vals_chr <- as.character(r_freq()$value)

        tagList(
        selectInput(
          ns("selected_layer"),
          "3) Multiple categories detected — select the class/ecosystem required:",
          choices  = vals_chr,
          selected = vals_chr[1]
        ),
        hr()
        )

      }
      }
    })
    
    # Dynamic year inputs -----------------------------------------------------
    extract_year <- function(filename) {
      m <- regexpr("(?<!\\d)(19|20)\\d{2}(?!\\d)", filename, perl = TRUE)
      if (m[1] == -1) return(NULL)
      as.numeric(regmatches(filename, m))
    }
    
    
    output$year_inputs <- renderUI({
      req(input$tifs)
      
      fnames <- input$tifs$name
      auto_years <- sapply(fnames, extract_year)
      
      # Build HTML rows for modal
      file_rows <- mapply(function(f, y) {
        
        if (is.null(y)) {
          # RED for missing year
          htmltools::tags$tr(
            htmltools::tags$td(f),
            htmltools::tags$td(style = "color:#c0392b; font-weight:bold;", "No year detected")
          )
        } else {
          # GREEN for detected year
          htmltools::tags$tr(
            htmltools::tags$td(f),
            htmltools::tags$td(style = "color:#27ae60; font-weight:bold;", y)
          )
        }
        
      }, fnames, auto_years, SIMPLIFY = FALSE)
      
      # Show modal
      showModal(
        modalDialog(
          title = htmltools::tags$div(
            style="font-size:20px; font-weight:bold; color:#2c3e50;",
            "Auto-Detected Years"
          ),
          easyClose = TRUE,
          footer = modalButton("OK"),
          
          # Modal body
          htmltools::tags$div(
            style="margin-bottom:10px; color:#34495e;",
            "Years were automatically extracted from filenames. You can adjust them in the sidebar (section 2 - drop down list)."
          ),
          
          # Styled table
          htmltools::tags$table(
            style="
          width:100%;
          border-collapse: collapse;
          margin-top:10px;
          font-size:14px;
        ",
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th("Filename", style="text-align:left; border-bottom:2px solid #bdc3c7; padding:6px;"),
                htmltools::tags$th("Detected Year", style="text-align:left; border-bottom:2px solid #bdc3c7; padding:6px;")
              )
            ),
            htmltools::tags$tbody(
              lapply(file_rows, identity)
            )
          )
        )
      )
      
      # Generate year dropdowns
      dropdowns <-lapply(seq_along(fnames), function(i) {
          selectInput(
          ns(paste0("year_", i)),   # <-- use ns() here
          paste(fnames[i]),
          choices = c("Not found", rev(seq(1900, 2025, by = 1))),
          selected = if (!is.null(auto_years[i]) &&
                         auto_years[i] %in% 1900:2025) auto_years[i] else "Not found"
        )
      })
    
      # Append a single hr() after all dropdowns
      tagList(dropdowns, hr())
      
      
    })
    
    output$part2 <- renderUI({
      # if (isTRUE(input$year_inputs)) 
      req(input$tifs)
      paste("2) Check/select year for each classification map")
    })
    
    
    # Conditional hr after year selection
    output$hr_after_years <- renderUI({
      if (isTRUE(input$year_inputs)) hr()
    })
    
    
    # conditional process button text  
    output$process_btn_text <- renderUI({
      # if (isTRUE(input$year_inputs)) 
      req(input$tifs)
      tags$span(
        ifelse(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1,
               "4) Click the process button below to start the ecosystem assessment: ",
               "3) Click the process button below to start the ecosystem assessment: "),
        tooltip(bs_icon("info-circle"),
                "The 'process button' will only appear when the years are distinct and/or complete for each file"
        )
      )
    })
    
    # Conditional process button
    output$process_btn_ui <- renderUI({
      req(input$tifs)
      years_raw <- sapply(seq_along(input$tifs$name),
                          function(i) input[[paste0("year_", i)]])
      
      # Normalize years: trim and convert plausible numerics
      normalize_year <- function(y) {
        if (is.null(y)) return(NA_character_)
        y_chr <- trimws(as.character(y))
        if (y_chr == "" || tolower(y_chr) %in% c("not known", "unknown")) return(NA_character_)
        # keep 4-digit numeric strings as-is; otherwise NA
        if (grepl("^\\d{4}$", y_chr)) return(y_chr)
        return(NA_character_)
      }
      
      years <- vapply(years_raw, normalize_year, FUN.VALUE = character(1))
      
      # Validity checks
      all_valid <- all(!is.na(years))
      more_than_one_file <- length(years) > 1
      more_than_one_unique <- length(unique(years)) > 1
      
      # Only show button when conditions are met
      if (!all_valid) return(NULL)
      
      if (!more_than_one_file) {
        # Single file: valid year is enough
        actionButton(ns("process"), "Process ecosystem maps")      } else {
        # Multiple files: require distinct years
        if (more_than_one_unique) {
          actionButton(ns("process"), "Process ecosystem maps")        } else {
          return(NULL)
        }
      }
    })
    
    
    # -----------------------------------------------------------------------------
    # process rasters 
    # -----------------------------------------------------------------------------
    
    
    # observeEvent(input$tifs, {
    #   updateSelectInput(
    #     session,
    #     "selected_layer",
    #     choices = character(0),
    #     selected = character(0)
    #   )
    # }, ignoreInit = TRUE)
    
    #--------------------------------------------------------------------------
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      
      req(input$tifs)                    # see app.R server for tifs_inputs
      files <- input$tifs$datapath
      years <- year_inputs()               # see app.R server for year_inputs
      # req(length(years) == length(files))  # sanity check
      
      # show modal popup
      showModal(upload_modal)
      
      # # Call the data module function (see data.R) to process rasters
      # outputs <- process_rasters(files, years, selected_layer)
      
      # str(outputs)
      # Description data ------------------------------------------------------
      # Create a Progress object
      # progress <- shiny::Progress$new(min = 0,
                                      # max = 2)
      # progress$set(message = "extacting data...", value = 0)
      progress <- shiny::Progress$new(min = 0, max = length(files))
      progress$set(message = "Processing classification maps...", value = 0)

      # reset previous data
      areas(NULL)
      
      rasters(NULL)         
      rasters_original(NULL) 
      areas(NULL)           
      
      ras_list <- list(NULL)
      ras_list_original <- list(NULL)
      area_vals <- numeric(length(files))
      
      order_idx <- order(as.numeric(year_inputs()))
      files <- files[order_idx]
      year_inputs <- year_inputs()[order_idx]
    
      
      select_layer_or_mask <- function(r_all, sel_name) {
        # If no selection provided, return first layer or whole raster
        if (is.null(sel_name) || !nzchar(sel_name)) {
          return(if (nlyr(r_all) == 1) r_all[[1]] else r_all[[1]])
        }
        
        # Case A: sel_name matches a layer name
        if (sel_name %in% names(r_all)) {
          return(r_all[[sel_name]])
        }
        
        # Case B/C: single-layer raster
        if (nlyr(r_all) == 1) {
          r_single <- r_all[[1]]
          uvals <- unique(values(r_single))
          uvals <- uvals[!is.na(uvals)]
          
          # Case C: single category → binary mask
          if (length(uvals) == 1) {
            return(ifel(!is.na(r_single), 1, NA))
          }
          
          # Try numeric match
          sel_num <- suppressWarnings(as.numeric(sel_name))
          if (!is.na(sel_num)) {
            return(ifel(r_single == sel_num, 1, NA))
          }
          
          # Try factor levels or fallback to string match
          lev <- levels(r_single)
          if (!is.null(lev) && length(lev) >= 1 && nrow(lev[[1]]) > 0) {
            lev_tbl <- lev[[1]]
            lbl_col <- intersect(c("label", "levels", "Labels", "Level"), names(lev_tbl))
            val_col <- intersect(c("value", "ID", "Values", "Class"), names(lev_tbl))
            if (length(lbl_col) >= 1 && length(val_col) >= 1) {
              idx <- match(sel_name, lev_tbl[[lbl_col[1]]])
              if (!is.na(idx)) {
                v <- lev_tbl[[val_col[1]]][idx]
                return(ifel(r_single == v, 1, NA))
              }
            }
          }
          # Fallback: compare as string
          return(app(r_single, fun = function(x) ifelse(x == sel_name, 1, NA), cores = 1))
        }
        
        # Multi-layer but invalid selection → fallback to first layer
        return(r_all[[1]])
      }
      
      
      sel_name <- selected_layer()
      
      
      
      
      for (i in seq_along(files)) {
        r_all <- rast(files[i])
        r_orig <- select_layer_or_mask(r_all, sel_name)
        

        # r_orig <- select_spatraster(files[i], sel_name, fallback_multi = "first")
        print(r_orig)
          

        if (global(r_orig, "isNA") == 0) {
          r_orig <- classify(r_orig, matrix(c(0, NA), ncol = 2))
        }
        mask <- r_orig == 1
        if (!is.na(crs(r_orig)) && !is.lonlat(r_orig)) {
          cell_area <- prod(res(r_orig))
          area_vals[i] <- sum(values(mask, na.rm = TRUE)) * cell_area
        } else {
          area_vals[i] <- sum(expanse(mask, unit = "m"), na.rm=T)
        }
        area_df <- data.frame(
          Year = year_inputs[i],
          Area_ha = area_vals[i] / 10000,
          Area_km2 = area_vals[i] / 1e6
        )
        
        # now add row(s)
        if (is.null(areas())) {
          areas(area_df)
        } else {
          areas(rbind(areas(), area_df))
        }
        
        print(areas())

        # r_map <- tryCatch({
        #   project(r_orig, "EPSG:4326")
        # }, error = function(e) { crs(r_orig) <- "EPSG:4326"; r_orig })
        names(r_orig) <- year_inputs[i]
        # names(r_orig) <- names(r_map) <- year_inputs[i]
        ras_list_original[[i]] <- r_orig
        
        
        # Only sample the most recent raster (last in sequence)
        if (i == length(files)) {
          r_map <- spatSample(r_orig, 100000, method = "regular", as.raster = TRUE, na.rm=T)
          r_map <- tryCatch({
            project(r_orig, "EPSG:4326")
          }, error = function(e) { crs(r_orig) <- "EPSG:4326"; r_orig })
          ras_list[[i]] <- r_map
          
        }
        
        r_map <- spatSample(r_orig, 100000, method = "regular", as.raster = TRUE, na.rm=T)
        r_map <- tryCatch({
          project(r_orig, "EPSG:4326")
        }, error = function(e) { crs(r_orig) <- "EPSG:4326"; r_orig })
        ras_list[[i]] <- r_map
        
      } # end of for loop
      
      # store data in relevant reactive variables -----------------------------
      rasters(ras_list)
      rasters_original(ras_list_original)
      years(year_inputs)
      
      
      # hide progress once done
      progress$close()


      })
    
    # -----------------------------------------------------------------------------
    # return data
    # -----------------------------------------------------------------------------
    
    # Return a list of reactives
    return(list(
      
      rasters = rasters,
      rasters_original = rasters_original,
      areas = areas,
      # years = years,
      # process_rasters = process_rasters
      #latest = latest,
      #tifs = reactive(input$tifs),   # raw fileInput object this was used by description module in the process_btn
      years = year_inputs,
      process = process_btn, 
      selected_layer = selected_layer,
      lyr_names = lyr_names
    ))
    
    
  })
    }