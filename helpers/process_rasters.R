
# process_rasters

# function to process tif inputs ------------------------------------------
process_rasters <- function(files, year_inputs,
                            # multi_ecosystem,
                            selected_layer) {
  
  # Create a Progress object
  progress <- shiny::Progress$new(min = 0, max = length(files))
  progress$set(message = "Processing classification maps...", value = 0)
  
  # reset previous data
  areas(NULL)
  
  ras_list <- list()
  ras_list_original <- list()
  area_vals <- numeric(length(files))
  
  order_idx <- order(as.numeric(year_inputs))
  files <- files[order_idx]
  year_inputs <- year_inputs[order_idx]
  
  # Snapshot the UI selections at start
  sel_name <- selected_layer()
  #print(sel_name)
  
  for (i in seq_along(files)) {
    
    # Update progress
    progress$inc(i/length(files), detail = paste0("Processing ", year_inputs[i], " classification"))   
    
    # Load full raster
    r_all <- rast(files[i])
    
    # # If multi_ecosystem, subset to selected_layer
    #   if (!is.null(sel_name)) {
    #   # pick only the chosen layer
    #   if (sel_name %in% names(r_all)) {
    #     r_orig <- r_all[[sel_name]]
    #   } else {
    #     # fallback: just take first layer if name not found
    #     r_orig <- r_all[[1]]
    #   }
    # } else {
    #   # default: single‑layer raster
    #   r_orig <- r_all
    # }
    # 
    
    
    # If multi_ecosystem, subset to selected_layer OR mask a single-layer to a selected value
    if (!is.null(sel_name) && sel_name != "") {
      
      if (sel_name %in% names(r_all)) {
        # ---- Case A: sel_name is a layer name ----
        r_orig <- r_all[[sel_name]]
        
      } else if (nlyr(r_all) == 1) {
        # ---- Case B: single-layer raster + category selection ----
        r_single <- r_all[[1]]
        
        # Try to interpret sel_name as a numeric class first
        sel_num <- suppressWarnings(as.numeric(sel_name))
        
        if (!is.na(sel_num)) {
          # Numeric class match
          r_orig <- ifel(r_single == sel_num, 1, NA)
          
        } else {
          # Non-numeric: try factor levels/labels
          lev <- levels(r_single)
          if (!is.null(lev) && length(lev) >= 1 && nrow(lev[[1]]) > 0) {
            lev_tbl <- lev[[1]]
            lbl_col <- intersect(c("label", "levels", "Labels", "Level"), names(lev_tbl))
            val_col <- intersect(c("value", "ID", "Values", "Class"), names(lev_tbl))
            
            if (length(lbl_col) >= 1 && length(val_col) >= 1) {
              idx <- match(sel_name, lev_tbl[[lbl_col[1]]])
              if (!is.na(idx)) {
                v <- lev_tbl[[val_col[1]]][idx]
                r_orig <- ifel(r_single == v, 1, NA)
              } else {
                # Fallback: compare factor labels directly
                r_fac <- as.factor(r_single)
                r_orig <- ifel(r_fac == sel_name, 1, NA)
              }
            } else {
              # Levels table exists but columns are unexpected — compare labels directly
              r_fac <- as.factor(r_single)
              r_orig <- ifel(r_fac == sel_name, 1, NA)
            }
            
          } else {
            # Final fallback: string-coded raster without levels — compare strings
            # Use app to avoid recycling issues for character rasters
            r_orig <- app(r_single, fun = function(x) ifelse(x == sel_name, 1, NA), cores = 1)
          }
        }
        
      } else {
        # ---- Case C: sel_name didn't match a layer; multi-layer but no valid selection ----
        r_orig <- r_all[[1]]
      }
      
    } 
    
    
    if (global(r_orig, "isNA") == 0) {
      r_orig <- classify(r_orig, matrix(c(0, NA), ncol = 2))
    }
    mask <- r_orig == 1
    if (!is.na(crs(r_orig)) && !is.lonlat(r_orig)) {
      cell_area <- prod(res(r_orig))
      area_vals[i] <- sum(values(mask, na.rm = TRUE)) * cell_area
    } else {
      area_vals[i] <- sum(expanse(mask, unit = "m"))
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
    
    #print(ras_list[[i]])
  } # end of for loop
  
  # store data in relevant reactive variables -----------------------------
  rasters(ras_list)
  rasters_original(ras_list_original)
  years(year_inputs)
  
  # close progress
  progress$close()
  
} # end of process rasters