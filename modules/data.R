# data module

data_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # reactive variables to store data ----------------------------------------
    rasters <- reactiveVal()
    rasters_original <- reactiveVal()
    areas <- reactiveVal()
    years <- reactiveVal()

        
    # function to process tif inputs ------------------------------------------
    process_rasters <- function(files, year_inputs) {
      
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
      
      for (i in seq_along(files)) {
        
        # Update progress
        progress$inc(i/length(files), detail = paste0("Processing ", year_inputs[i], " classification"))   
        
        r_orig <- rast(files[i])
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
                
        r_map <- tryCatch({
          project(r_orig, "EPSG:4326")
        }, error = function(e) { crs(r_orig) <- "EPSG:4326"; r_orig })
        names(r_orig) <- names(r_map) <- year_inputs[i]
        ras_list_original[[i]] <- r_orig
        
        
        # Only sample the most recent raster (last in sequence)
        if (i == length(files)) {
          r_map <- spatSample(r_orig, 100000, method = "regular", as.raster = TRUE, na.rm=T)
          ras_list[[i]] <- r_map
        }
        r_map <- spatSample(r_orig, 100000, method = "regular", as.raster = TRUE, na.rm=T)
        ras_list[[i]] <- r_map
        
      } # end of for loop
      
      # store data in relevant reactive variables -----------------------------
      rasters(ras_list)
      rasters_original(ras_list_original)
      years(year_inputs)
      
      # close progress
      progress$close()
    
      } # end of process rasters
    

    # return the data/function ------------------------------------------------
    return(list(
      rasters = rasters,
      rasters_original = rasters_original,
      areas = areas,
      years = years,
      process_rasters = process_rasters
    ))
  })
}


