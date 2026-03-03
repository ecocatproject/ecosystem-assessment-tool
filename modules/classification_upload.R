# modules/classification_upload


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

# UI  - sidebar only

classification_module_ui <- function(id) {
  ns <- NS(id)
  sidebar(
    width = 300,
    # fileInput(ns("tifs"), "1) Upload Ecosystem classification file(s)",
    #           multiple = TRUE, accept = ".tif"),
    # 
    tags$span(
      "1) Upload Ecosystem classification file(s)",
      tooltip(bs_icon("info-circle"), "The file must be a raster format - e.g. .tif, it can be a single layer or multi-layer raster"),
      fileInput(ns("tifs"), "", multiple = TRUE, accept = ".tif")),
    hr(),
    uiOutput(ns("part2")),
    uiOutput(ns("year_inputs")),
    uiOutput(ns("hr_after_years")),
    uiOutput(ns("ecosystem_dropdown")),
    uiOutput(ns("run_description")),
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
    
    # reactive for layer names --------- --------------------------------------
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
    
    
    # --------------------------------------------------------------------------
    # Conditionally render dropdown
    output$ecosystem_dropdown <- renderUI({
      req(input$tifs)
      
      # Return NULL if raster loading failed
      if (inherits(r_rast(), "try-error")) return(NULL)
      
      # Only proceed if >1 layer
      if (NROW(r_freq()) > 1) {
        if (nlyr(r_rast()) > 1) {
          # Multiple raster layers
          years <- sapply(seq_along(input$tifs$name),
                          function(i) input[[paste0("year_", i)]])
          tagList(
            selectInput(
              ns("selected_layer"),
              "3) Multiple data layers detected - select the layer/ecosystem required:",
              choices = lyr_names(),
              selected = lyr_names()[1]
            ),
            hr()
          )
          
        } else {
          # Multiple categories detected
          vals_chr <- as.character(r_freq()$value)
          
          tagList(
            # 3) Category source choice
            radioButtons(
              ns("cat_source"),
              "3a) Do you have a file describing the category levels?",
              choices = c(
                "Yes – I have a .aux.xml file" = "xml",
                "Yes – I have a 2-column .csv file" = "csv",
                "No – use raster values only"  = "raster"
              ),
              selected = "raster"
            ),
            
            # 3b) Dynamic UI
            uiOutput(ns("category_ui")),
            
            hr()
          )
          
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
            ns("aux"),  "3b) Upload corresponding .aux.xml file", accept = ".xml"),
          uiOutput(ns("class_ui"))
        )
        
      } else if (input$cat_source == "csv") {
        tagList(
          fileInput(
            ns("cat_file"), "3b) Upload corresponding 2-column .csv file", accept = ".csv"),
          #uiOutput(ns("column_preview_ui")),
          uiOutput(ns("class_ui"))
        )
        
      } else {
        # Raster-only categories
        vals_chr <- as.character(r_freq()$value)
        
        selectInput(
          ns("selected_layer"),
          "3b) Select the class/ecosystem required:",
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
    # cat_table <- reactive({
    #   req(input$aux)
    #   
    #   xml <- read_xml(input$aux$datapath)
    #   rows <- xml_find_all(xml, ".//Row")
    #   
    #   value <- as.numeric(xml_text(xml_find_all(rows, ".//F[1]")))
    #   label <- xml_text(xml_find_all(rows, ".//F[2]"))
    #   
    #   data.frame(value = value, label = label, stringsAsFactors = FALSE)
    # })
    
    
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
          # categories <- xml2::xml_find_all(xml, ".//CategoryNames//Category")
          # 
          # if (length(categories) == 0) {
          #   validate("No usable category information found in XML.")
          # }
          # 
          # label <- xml2::xml_text(categories)
          categories <- xml2::xml_find_all(xml, ".//CategoryNames//Category")
          
          # Extract text
          label <- trimws(xml2::xml_text(categories))
          
          # Remove empty / blank entries
          label <- label[nzchar(label)]
          
          # Optional: remove duplicates
          label <- unique(label)
          
          # raster_vals <- sort(terra::unique(r_rast)[,1])
          raster_vals <- sort(terra::freq(r_rast)[,1])
          #raster_vals <- raster_vals[!is.na(raster_vals)]
          
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
      
      # Optional: remove NA rows
      # df <- df[!is.na(df$value) & !is.na(df$label), ]
      
      df
    })
    
    
    # --------------------------------------------------------------------------
    # resolved selection
    resolved_selection <- eventReactive(process_btn(), {
      # If single-value raster, just return the raster value or first layer
      r <- r_rast()
      if (nlyr(r) == 1 && length(terra::unique(r)[,1]) <= 1) {
        return(as.character(terra::unique(r)[,1]))
      }
      
      # For categorical rasters or XML/CSV categories
      if (!is.null(input$cat_source) && input$cat_source %in% c("csv","xml")) {
        req(input$class)
        return(as.numeric(input$class))
      } else if (!is.null(input$selected_layer)) {
        return(input$selected_layer)
      } else {
        return(names(r)[1]) # fallback
      }
    })
    
    # --------------------------------------------------------------------------
    # part 2 ui sidebar
    output$part2 <- renderUI({
      # if (isTRUE(input$year_inputs)) 
      req(input$tifs)
      # paste("2) Check/select year for each classification map")
      tags$span(
        "2) Check/select year for each classification map",
        tooltip(bs_icon("info-circle"), "Each must have a input before the 'process button' is available"))
    })
    
    
    
    # --------------------------------------------------------------------------
    # Dynamic year inputs
    extract_year <- function(filename) {
      m <- regexpr("(?<!\\d)(19|20)\\d{2}(?!\\d)", filename, perl = TRUE)
      if (m[1] == -1) return(NULL)
      as.numeric(regmatches(filename, m))
    }
    
    # --------------------------------------------------------------------------
    # render year ui
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
    
    
    
    # --------------------------------------------------------------------------
    # Conditional hr after year selection
    output$hr_after_years <- renderUI({
      if (isTRUE(input$year_inputs)) hr()
    })
    
    # --------------------------------------------------------------------------
    # checkbox for description calculation
    output$run_description <- renderUI({
      req(input$tifs)
      tags$span(
        ifelse(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1,
               "4) Select here for calculation of descriptive variables",
               "3) Select here for calculation of descriptive variables"),
        tooltip(bs_icon("info-circle"),
                "By checking this box, the descriptive variables will be calculated for most recent ecosystem map. This will slightly increasing the processing time"),
        checkboxInput(ns("run_description"),label = NULL, value = FALSE),
        hr()
      )
    })
    
    output$process_btn_text <- renderUI({
      # if (isTRUE(input$year_inputs)) 
      req(input$tifs)
      tags$span(
        ifelse(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1,
               "5) Click the process button below to start the ecosystem assessment: ",
               "4) Click the process button below to start the ecosystem assessment: "),
        tooltip(bs_icon("info-circle"),
                "The 'process button' will only appear when the years are distinct and/or complete for each file"
        )
      )
    })
    
    # output$run_description <- renderUI({
    #   req(input$tifs)
    #   tagList(
    #     tags$label("3) Select here for calculation of descriptive variables", 
    #                `for` = ns("run_description"), style = "margin:0;"),
    #     tags$input(type = "checkbox", id = ns("run_description"), style = "margin:0; padding:0; display:inline-block;"),
    #     hr()
    #   )
    # })
    
    # output$run_description <- renderUI({
    #   req(input$tifs)
    #   tagList(
    #     div(
    #       tags$span("3) Select here for calculation of descriptive variables"),
    #        style = "display:flex; align-items:center; gap:10px; margin:0; padding:0;",
    #       #style = "display:flex; gap:10px; margin:0; padding:0;",
    #       checkboxInput(ns("run_description"), label = NULL, value = FALSE)
    #       #,
    #              #   style = "font-size:18px;")  # bigger text
    #     ),
    #     tags$style(HTML(paste0("
    #   #", ns("run_description"), " {
    #     width: 50px;    /* make checkbox bigger */
    #     height: 30px;   /* make checkbox bigger */
    #     cursor: pointer;
    #   }
    # "))),
    #     hr()
    #   )
    # })
    
    # --------------------------------------------------------------------------
    # conditional process button text  
    output$process_btn_text <- renderUI({
      # if (isTRUE(input$year_inputs)) 
      req(input$tifs)
      tags$span(
        ifelse(nlyr(r_rast()) > 1 || NROW(r_freq()) > 1,
               "5) Click the process button below to start the ecosystem assessment: ",
               "4) Click the process button below to start the ecosystem assessment: "),
        tooltip(bs_icon("info-circle"),
                "The 'process button' will only appear when the years are distinct and/or complete for each file"
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Conditional process button
    output$process_btn_ui <- renderUI({
      req(input$tifs)
      # # Hide if XML required but missing
      # if (!is.null(input$cat_source) &&
      #     input$cat_source %in% c("xml", "csv") &&
      #     is.null(input$aux)) {
      #   return(NULL)
      # }
      
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
    
    
    #--------------------------------------------------------------------------
    # Process uploaded TIFs when the process_button is clicked
    observeEvent(process_btn(), {
      gc()
      req(input$tifs)                    # see app.R server for tifs_inputs
      files <- input$tifs$datapath
      
      # check <- check_resolution(input$tifs$datapath)
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
      
     # check <- check_resolution(input$file_upload$datapath)
     #  if (!check$ok) {
     #    
     #    showModal(modalDialog(
     #      title = "Raster too detailed",
     #      paste0(
     #        "Uploaded raster resolution is ",
     #        check$res,
     #        " m.\n\n",
     #        "Please upload an aggregated raster (≥100 m cells)."
     #      ),
     #      easyClose = TRUE
     #    ))
     #    
     #    return(NULL)
     #  }
     
      # ------------------------------------------------------------------------
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = length(files))
      progress$set(message = "Processing classification maps...", value = 0)
      
      # ------------------------------------------------------------------------
      # reset previous data
      areas(NULL)
      rasters(NULL)         
      rasters_original(NULL) 
      
      # ------------------------------------------------------------------------
      # create some variables + ensure year order is correct
      years <- year_inputs()               # see app.R server for year_inputs
      area_vals <- numeric(length(files))
      order_idx <- order(as.numeric(years))
      files <- files[order_idx]
      years <- years[order_idx]
      
      
      # ------------------------------------------------------------------------
      # identify selected layer
      sel_name <- resolved_selection()
      
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
        r_orig <- select_layer_or_mask(r_all, sel_name)
        
        print("select layer complete")
        if (global(r_orig, "isNA") == 0) {
          # if (terra::global(!is.na(r_orig), "sum")[1,1] == ncell(r_orig)) {
            print("going through files - 1st if")
          r_orig <- classify(r_orig, matrix(c(0, NA), ncol = 2))
          }
        
        print("doing mask")
        #mask <- r_orig == 1
        print("mask complete")
        
        if (!is.na(crs(r_orig)) && !is.lonlat(r_orig)) {
          print("going through files - 2nd if")
          cell_area <- prod(res(r_orig))
          # area_vals[i] <- sum(values(mask, na.rm = TRUE)) * cell_area
          n_cells <- terra::global(r_orig, "sum", na.rm = TRUE)[1,1]
          area_vals[i] <- n_cells * cell_area
                  } else {
                    print("area calc")
          area_vals[i] <- expanse(r_orig, unit = "m")[,2]
        }
        
        print(area_vals)
        
        area_df <- data.frame(
          Year = years[i],
          Area_ha = area_vals[i] / 10000,
          Area_km2 = area_vals[i] / 1e6
        )
        
        # now add row(s)
        if (is.null(areas())) {
          areas(area_df)
        } else {
          print("areas 1")
          print(areas())
          
          areas(rbind(areas(), area_df))
        }
        
        print("areas all")
        print(areas())
        
        
        # Only sample the most recent raster (last in sequence)
        # this is the plotted map on leaflet
        if (i == length(files)) {
          print("sampling raster")
          names(r_orig) <- years[i]
          r_map <- spatSample(r_orig, 1000000, method = "regular", as.raster = TRUE, na.rm=T)
          r_map <- tryCatch({
            project(r_map, "EPSG:4326") # project(r_map, "EPSG:3857", method = "near")
          }, error = function(e) { crs(r_map) <- "EPSG:4326"; r_map }) # project(r_map, "EPSG:3857", method = "near")
          
        }
        
        # Clean up tempory files periodically on long runs
        if ((i %% 3) == 0) {
          try(terra::tmpFiles(remove = TRUE), silent = TRUE)
          gc()
        }
        
        
      } # end of for loop
      
      
      # store data in relevant reactive variables ------------------------------
      rasters(r_map)
      rasters_original(r_orig)
      # rasters_original(input$tifs$datapath[length(input$tifs$datapath)])
      years(years)
      
      # hide progress once done ------------------------------------------------
      progress$close()
    })
    
    # --------------------------------------------------------------------------
    # return data
    return(list(
      rasters = rasters,
      rasters_original = rasters_original,
      areas = areas,
      years = year_inputs,
      process = process_btn, 
      selected_layer = selected_layer,
      lyr_names = lyr_names,
      run_description = reactive({
        input$run_description})
    ))
    
    
  })
}
