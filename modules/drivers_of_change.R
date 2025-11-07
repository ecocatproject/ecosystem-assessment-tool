# modules/drivers_of_change.R

drivers_module_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Drivers of Change",
      # Card for Criterion B1 -------------------------------------------------
      card(
        full_screen = TRUE,
        # card_header("Subcriteria B1"),
        card_body(
          leafletOutput(ns("map_drivers"), height = 300),
          class = "p-0",# remove padding
          hr(),
          actionButton(ns("get_drivers"), "Click here to map areas of ecosystem change and assess recent land-use to highlight drivers of change", width = "100%"),
          card(layout_column_wrap(
            height = 100,
            page_fillable(
              plotlyOutput(ns("drivers_graph")))
          )
          )
        )
      )
  )
}

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------
library(rstac)
library(sf)
library(ggplot2)

# drivers_module_server <- function(id, rasters_original, year_inputs, rasters) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # # -------------------------------------------------------------------------
#     # # Initialize map
#     # output$map_drivers <- renderLeaflet({
#     #   if (is.null(rasters()) || length(rasters()) < 1){
#     #     # return a blank map
#     #     leaflet() %>% addTiles()
#     #   } else {
#     #     r <- rasters()[[1]]
#     #     ctr <- crds(centroids(vect(ext(r))))
#     #     leaflet() %>% addTiles() %>% setView(lng = ctr[1], lat = ctr[2], zoom = 6)
#     #     }
#     # 
#     #   })
#     # 
#     # ------------------- Initialize blank map -------------------
#     output$map_drivers <- renderLeaflet({
#       leaflet() %>% addTiles()
#     })
#     
#     # ------------------- Helper functions -------------------
#     get_change_mask <- function(oldest, recent, n = 100000) {
#       change <- terra::mask(oldest, recent, inverse = TRUE)
#       spatSample(change, n, method = "regular", as.raster = TRUE)
#     }
#     
#     merge_colors <- function(raster, table) {
#       coltab_df <- as.data.frame(coltab(raster)[[1]]) %>%
#         mutate(hex = rgb(red, green, blue, alpha, maxColorValue = 255))
#       table %>% left_join(coltab_df[, c("value", "hex")], by = "value")
#     }
#     
#     render_drivers_plot <- function(data) {
#       plot_ly(
#         data = data,
#         x = ~reorder(lc_class, -perc),
#         y = ~perc,
#         type = 'bar',
#         text = ~paste0(sprintf("%.1f", perc), "%"),
#         textposition = 'outside',
#         marker = list(color = ~hex),
#         hovertemplate = paste("<b>%{x}</b><br>Percentage: %{y:.1f}%")
#       ) %>%
#         layout(
#           title = "Land Cover Composition within Mask",
#           xaxis = list(title = "Land Cover Class", tickangle = -45),
#           yaxis = list(title = "Percentage of Area (%)"),
#           showlegend = FALSE,
#           margin = list(b = 100)
#         )
#     }
#     
#  
#     # ------------------------------------------------------------------------
#     # get maps
#     observeEvent(input$get_drivers, {
#       esa_lc_lookup <- data.frame(map_code = c(seq(10,90, by = 10), 95, 100),
#                                   lc_class = c("tree_cover", "shrubland", "grassland", "cropland", "built-up", "bare_sparse",
#                                                "snow_ice", "permanent_water", "herbaceous_wetland", "mangroves", "moss_lichen"))
#       # # Create a Progress object
#       # progress <- shiny::Progress$new(min = 0, max = 3)
#       # progress$set(message = "Calculating AOO...", value = 0)
#       
#       req(rasters_original(), length(rasters_original()) > 0) # checks the list of rasters is not empty
#       
#       # get the stored tifs and name by year
#       recent <- rasters_original()[[length(rasters_original())]] # 
#       oldest <- rasters_original()[[1]] #
#       
#       
#       # 1. if only 1 year show message that it can't be done
#       # 2. get oldest and most recent rasters
#       change <- terra::mask(oldest, recent, inverse = TRUE)
#       change_sampled <- spatSample(change, 100000, method = "regular", as.raster = TRUE)
#       
#     
#       leafletProxy(ns("map_drivers"))  %>%
#         addRasterImage(change_sampled, opacity = 0.7, colors = "red",
#                        group = "ecosystem change") %>%
#         addLayersControl(
#           overlayGroups = c("ecosystem change"),
#           options = layersControlOptions(collapsed = FALSE)
#         )
#       
#      
#       # 
#       ms_query <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
#       ms_collections <- ms_query |>
#         rstac::collections() |>
#         rstac::get_request()
#       collections <- "esa-worldcover"
#       ms_esa_query <- rstac::stac_search(
#         q = ms_query,
#         collections = collections,
#         datetime = 
#           "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
#         bbox = st_bbox(st_as_sf(vect(ext(oldest))))
#       ) |>
#         rstac::get_request()
#       ms_query_signin <- rstac::items_sign(
#         ms_esa_query, rstac::sign_planetary_computer()
#       )
#       output_dir = tempdir()
#       esa_map_url <- rstac::assets_url(ms_esa_query, "map")
#       
#       make_vsicurl_url <- function(base_url, collection) {
#         paste0(
#           "/vsicurl", 
#           "?pc_url_signing=yes",
#           paste0("&pc_collection=", collection),
#           "&url=",
#           base_url
#         )
#       }
#       map_url <- make_vsicurl_url(esa_map_url, "esa-worldcover")
#       
#       out_file <- tempfile(fileext = ".tif")
#       
#       sf::gdal_utils(
#           "warp",
#           source = map_url,
#           destination = out_file,
#           options = c(
#             # "-t_srs", sf::st_crs(it_bbox)$wkt,
#             "-te", sf::st_bbox(vect(ext(oldest)))
#           )
#         )
#       
#       land_cover_raster <- rast(out_file)
#       
#       change_aligned <- project(change, land_cover_raster, method = "near")
#       
#       # Keep lc values where mask == 1
#       lc_masked <- mask(land_cover_raster, change_aligned, maskvalue = NA)
#       lc_masked_sampled <- spatSample(lc_masked, 100000, method = "regular", as.raster = TRUE)
#       
#       lc_table <- freq(lc_masked)
# 
#       
#       leafletProxy(ns("map_drivers"))  %>%
#         addRasterImage(lc_masked_sampled, opacity = 0.7, colors = "red",
#                        group = "lc") %>%
#         addLayersControl(
#           overlayGroups = c("ecosystem change", "lc"),
#           options = layersControlOptions(collapsed = FALSE)
#         )
#       
#       
#     
#       merged_table <- lc_table %>%
#         left_join(esa_lc_lookup, by = c("value" = "map_code")) %>%
#         mutate(
#           total = sum(count, na.rm = TRUE),
#           perc = 100 * count / total
#         ) %>%
#         arrange(desc(perc)) %>%
#         select(lc_class, value, count, perc)
#       
#     
#       # Extract color table as a data frame
#       coltab_df <- as.data.frame(coltab(land_cover_raster)[[1]]) %>%
#         mutate(hex = rgb(red, green, blue, alpha, maxColorValue = 255)) # add hex codes
#       
#       # merge with table
#       merged_table_col <- merged_table %>%
#         left_join(coltab_df[, c("value", "hex")], by = "value")
#       
#       # bar plot
#       output$drivers_graph <- renderPlotly({
#         plot_ly(
#           data = merged_table_col,
#           x = ~reorder(lc_class, -perc),
#           y = ~perc,
#           type = 'bar',
#           text = ~paste0(sprintf("%.1f", perc), "%"),       # labels on bars
#           textposition = 'outside',
#           marker = list(color = ~hex),
#           hovertemplate = paste(
#             "<b>%{x}</b><br>",
#             "Percentage: %{y:.1f}%"  # shows percentage on hover
#           )
#         ) %>%
#           layout(
#             title = "Land Cover Composition within Mask",
#             xaxis = list(title = "Land Cover Class", tickangle = -45),
#             yaxis = list(title = "Percentage of Area (%)"),
#             showlegend = FALSE,
#             margin = list(b = 100)  # to avoid overlap of x-axis labels
#           )
#       })
#     
#   
# 
#     
#     })
#     
#      })
# 
#   }
# drivers_module_server <- function(id, rasters_original, year_inputs, rasters) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
# 
#     # ------------------- Initialize blank map -------------------
#     output$map_drivers <- renderLeaflet({
#       leaflet() %>% addTiles()
#     })
# 
#     # ------------------- Helper functions -------------------
#     get_change_mask <- function(oldest, recent, n = 100000) {
#       change <- terra::mask(oldest, recent, inverse = TRUE)
#       spatSample(change, n, method = "regular", as.raster = TRUE)
#     }
# 
#     merge_colors <- function(raster, table) {
#       coltab_df <- as.data.frame(coltab(raster)[[1]]) %>%
#         mutate(hex = rgb(red, green, blue, alpha, maxColorValue = 255))
#       table %>% left_join(coltab_df[, c("value", "hex")], by = "value")
#     }
# 
#     render_drivers_plot <- function(data) {
#       plot_ly(
#         data = data,
#         x = ~reorder(lc_class, -perc),
#         y = ~perc,
#         type = 'bar',
#         text = ~paste0(sprintf("%.1f", perc), "%"),
#         textposition = 'outside',
#         marker = list(color = ~hex),
#         hovertemplate = paste("<b>%{x}</b><br>Percentage: %{y:.1f}%")
#       ) %>%
#         layout(
#           title = "Land Cover Composition within Mask",
#           xaxis = list(title = "Land Cover Class", tickangle = -45),
#           yaxis = list(title = "Percentage of Area (%)"),
#           showlegend = FALSE,
#           margin = list(b = 100)
#         )
#     }
# 
#     # ------------------- Observe button click -------------------
#     observeEvent(input$get_drivers, {
#       req(rasters_original(), length(rasters_original()) > 1)
# 
#       oldest <- rasters_original()[[1]]
#       recent <- rasters_original()[[length(rasters_original())]]
# 
#       # ------------------- Async STAC & Raster Processing -------------------
#       future({
#         # STAC download with tryCatch
#         stac_result <- tryCatch({
#           ms_query <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
#           ms_collections <- ms_query |> rstac::collections() |> rstac::get_request()
#           collections <- "esa-worldcover"
#           ms_esa_query <- rstac::stac_search(
#             q = ms_query,
#             collections = collections,
#             datetime = "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
#             bbox = st_bbox(st_as_sf(vect(ext(oldest))))
#           ) |> rstac::get_request()
# 
#           ms_query_signin <- rstac::items_sign(ms_esa_query, rstac::sign_planetary_computer())
#           esa_map_url <- rstac::assets_url(ms_esa_query, "map")
# 
#           make_vsicurl_url <- function(base_url, collection) {
#             paste0("/vsicurl?pc_url_signing=yes",
#                    paste0("&pc_collection=", collection),
#                    "&url=", base_url)
#           }
#           map_url <- make_vsicurl_url(esa_map_url, "esa-worldcover")
# 
#           out_file <- tempfile(fileext = ".tif")
#           sf::gdal_utils("warp",
#                          source = map_url,
#                          destination = out_file,
#                          options = c("-te", sf::st_bbox(vect(ext(oldest)))))
#           rast(out_file)
# 
#         }, error = function(e) {
#           # If STAC fails, return NULL
#           message("STAC request failed: ", e$message)
#           NULL
#         })
#         stac_result
#       }) %...>%  # then
#         (function(land_cover_raster) {
#           if (is.null(land_cover_raster)) {
#             showNotification("STAC ESA WorldCover download failed. Displaying only change mask.", type = "error")
#             return(NULL)
#           }
# 
#           # ------------------- Calculate change mask -------------------
#           change_sampled <- get_change_mask(oldest, recent)
# 
#           # Align and mask land cover raster
#           change_aligned <- project(change_sampled, land_cover_raster, method = "near")
#           lc_masked <- mask(land_cover_raster, change_aligned, maskvalue = NA)
#           lc_masked_sampled <- spatSample(lc_masked, 100000, method = "regular", as.raster = TRUE)
# 
#           # Calculate frequencies and percentages
#           lc_table <- freq(lc_masked)
#           esa_lc_lookup <- data.frame(
#             map_code = c(seq(10,90, by = 10), 95, 100),
#             lc_class = c("tree_cover","shrubland","grassland","cropland","built-up","bare_sparse",
#                          "snow_ice","permanent_water","herbaceous_wetland","mangroves","moss_lichen")
#           )
# 
#           merged_table <- lc_table %>%
#             left_join(esa_lc_lookup, by = c("value" = "map_code")) %>%
#             mutate(total = sum(count, na.rm = TRUE),
#                    perc = 100 * count / total) %>%
#             arrange(desc(perc)) %>%
#             select(lc_class, value, count, perc)
# 
#           merged_table_col <- merge_colors(land_cover_raster, merged_table)
# 
#           # ------------------- Update Leaflet -------------------
#           leafletProxy(ns("map_drivers")) %>%
#             clearShapes() %>%
#             addRasterImage(change_sampled, opacity = 0.7, colors = "red", group = "Ecosystem change") %>%
#             addRasterImage(lc_masked_sampled, opacity = 0.7, colors = "red", group = "Land Cover") %>%
#             addLayersControl(
#               overlayGroups = c("Ecosystem change", "Land Cover"),
#               options = layersControlOptions(collapsed = FALSE)
#             )
# 
#           # ------------------- Render Plotly -------------------
#           output$drivers_graph <- renderPlotly({
#             render_drivers_plot(merged_table_col)
#           })
# 
#         }) %...!%  # catch promise errors
#         (function(e){
#           showNotification(paste("Error processing rasters:", e$message), type = "error")
#         })
# 
#     })
# 
#   })
# }

drivers_module_server <- function(id, rasters_original, year_inputs, rasters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # # ------------------- Initialize blank map -------------------
    # output$map_drivers <- renderLeaflet({
    #   leaflet() %>% addTiles()
    # })
    
        # -------------------------------------------------------------------------
        # Initialize map
        output$map_drivers <- renderLeaflet({
          if (is.null(rasters()) || length(rasters()) < 1){
            # return a blank map
            leaflet() %>% addTiles()
          } else {
            r <- rasters()[[1]]
            ctr <- crds(centroids(vect(ext(r))))
            leaflet() %>% addTiles() %>% setView(lng = ctr[1], lat = ctr[2], zoom = 6)
            }

          })

    
    # ------------------- Helper functions -------------------
    get_change_mask <- function(oldest, recent, n = 100000) {
      change <- terra::mask(oldest, recent, inverse = TRUE)
      spatSample(change, n, method = "regular", as.raster = TRUE)
    }
    
    merge_colors <- function(raster, table) {
      coltab_df <- as.data.frame(coltab(raster)[[1]]) %>%
        mutate(hex = rgb(red, green, blue, alpha, maxColorValue = 255))
      table %>% left_join(coltab_df[, c("value", "hex")], by = "value")
    }
    
    render_drivers_plot <- function(data) {
      plot_ly(
        data = data,
        x = ~reorder(lc_class, -perc),
        y = ~perc,
        type = 'bar',
        text = ~paste0(sprintf("%.1f", perc), "%"),
        textposition = 'outside',
        marker = list(color = ~hex),
        hovertemplate = paste("<b>%{x}</b><br>Percentage: %{y:.1f}%")
      ) %>%
        layout(
          title = "Land Cover Composition within Mask",
          xaxis = list(title = "Land Cover Class", tickangle = -45),
          yaxis = list(title = "Percentage of Area (%)"),
          showlegend = FALSE,
          margin = list(b = 100)
        )
    }
    
    # ------------------- Observe button click -------------------
    observeEvent(input$get_drivers, {
      ras_list <- rasters_original()
      
      # --- Early exits ----
      if (is.null(ras_list) || length(ras_list) == 0) {
        showModal(modalDialog(
          title = "No raster data",
          div(style = "text-align:center;",
              tags$h4("No rasters available"),
              tags$p("Please upload or select raster data before running change detection.")
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
      
      if (length(ras_list) == 1) {
        showModal(modalDialog(
          title = "Insufficient data for change detection",
          div(style = "text-align:center;",
              tags$h4("Cannot calculate change"),
              tags$p("Only one year of raster data is available."),
              tags$p(tags$em("Please upload or select multiple years of data."))
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
      
      # --- Processing modal + progress ---
      showModal(modalDialog(
        title = "Mapping Drivers of Change",
        tagList(
          div(style = "text-align:center;",
              tags$h4("Processing..."),
              tags$p("Calculating ecosystem change and land cover composition."),
              shinycssloaders::withSpinner(uiOutput(ns("dummy_spinner")), type = 5)
          )
        ),
        easyClose = FALSE,
        footer = NULL
      ))
      
      progress <- shiny::Progress$new(session, min = 0, max = 4)
      progress$set(message = "Starting analysis...", value = 0)
      
      oldest <- ras_list[[1]]
      recent <- ras_list[[length(ras_list)]]
      
      # --- Run future safely ---
      fut <- future({
        library(terra)
        library(rstac)
        progress_steps <- list()
        
        # Step 1
        progress_steps[[1]] <- "Calculating change mask"
        change_sampled <- get_change_mask(oldest, recent)
        
        # Step 2 - try STAC
        progress_steps[[2]] <- "Downloading ESA WorldCover"
        stac_result <- tryCatch({
          ms_query <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
          ms_esa_query <- rstac::stac_search(
            q = ms_query,
            collections = "esa-worldcover",
            datetime = "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
            bbox = st_bbox(st_as_sf(vect(ext(oldest))))
          ) |> rstac::get_request()
          ms_query_signin <- rstac::items_sign(ms_esa_query, rstac::sign_planetary_computer())
          esa_map_url <- rstac::assets_url(ms_esa_query, "map")
          
          make_vsicurl_url <- function(base_url, collection) {
            paste0("/vsicurl?pc_url_signing=yes",
                   paste0("&pc_collection=", collection),
                   "&url=", base_url)
          }
          map_url <- make_vsicurl_url(esa_map_url, "esa-worldcover")
          
          esa_map_url <- rstac::assets_url(ms_query_signin, "map")
          map_url <- paste0("/vsicurl/", esa_map_url)
          
          out_file <- tempfile(fileext = ".tif")
          sf::gdal_utils("warp", source = map_url, destination = out_file,
                         options = c("-te", sf::st_bbox(vect(ext(oldest)))))
          rast(out_file)
        }, error = function(e) NULL)
        
        list(change_sampled = change_sampled, land_cover_raster = stac_result)
      })
      
      # --- Promise resolution (in Shiny session context) ---
      fut %...>% (function(res) {
        # Safely close modal + progress
        removeModal()
        try(progress$close(), silent = TRUE)
        
        change_sampled <- res$change_sampled
        land_cover_raster <- res$land_cover_raster
        
        if (is.null(land_cover_raster)) {
          showNotification("STAC ESA WorldCover download failed. Displaying only change mask.", type = "error")
          leafletProxy(ns("map_drivers")) %>%
            clearShapes() %>%
            addRasterImage(change_sampled, opacity = 0.7, colors = "red",
                           group = "Ecosystem change") %>%
            addLayersControl(
              overlayGroups = c("Ecosystem change"),
              options = layersControlOptions(collapsed = FALSE)
            )
          return()
        }
        
        # Mask, summarize, and plot
        change_aligned <- project(change_sampled, land_cover_raster, method = "near")
        lc_masked <- mask(land_cover_raster, change_aligned, maskvalue = NA)
        lc_masked_sampled <- spatSample(lc_masked, 100000, method = "regular", as.raster = TRUE)
        
        lc_table <- freq(lc_masked)
        esa_lc_lookup <- data.frame(
          map_code = c(seq(10,90, by = 10), 95, 100),
          lc_class = c("tree_cover","shrubland","grassland","cropland","built-up",
                       "bare_sparse","snow_ice","permanent_water",
                       "herbaceous_wetland","mangroves","moss_lichen")
        )
        
        merged_table <- lc_table %>%
          left_join(esa_lc_lookup, by = c("value" = "map_code")) %>%
          mutate(total = sum(count, na.rm = TRUE),
                 perc = 100 * count / total) %>%
          arrange(desc(perc)) %>%
          select(lc_class, value, count, perc)
        
        merged_table_col <- merge_colors(land_cover_raster, merged_table)
        
        leafletProxy(ns("map_drivers")) %>%
          clearShapes() %>%
          addRasterImage(change_sampled, opacity = 0.7, colors = "red", group = "Ecosystem change") %>%
          addRasterImage(lc_masked_sampled, opacity = 0.7, colors = "red", group = "Land Cover") %>%
          addLayersControl(
            overlayGroups = c("Ecosystem change", "Land Cover"),
            options = layersControlOptions(collapsed = FALSE)
          )
        
        output$drivers_graph <- renderPlotly({
          render_drivers_plot(merged_table_col)
        })
        
        showNotification("Drivers of change analysis complete!", type = "message")
        
      }) %...!% (function(e) {
        removeModal()
        try(progress$close(), silent = TRUE)
        showNotification(paste("Error processing rasters:", e$message), type = "error")
      })
    })
  })
}





