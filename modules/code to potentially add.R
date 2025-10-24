# code to potentially add...

#------------------------------------------------------------------------------
# add simialr to app.R?
# raster_module_server("raster", 
#                      data_module = data_module,
#                      tifs_input = reactive({ input$tifs }),
#                      year_inputs = reactive({ sapply(seq_along(input$tifs$name), function(i) input[[paste0("year_", i)]]) }),
#                      process_btn = reactive({ input$process }),  # <- THIS IS REQUIRED)
#                      leaflet_proxy = reactive(leafletProxy("description-roi_map"))
#                      # leaflet_proxy = description$leaflet_proxy  # pass the shared map proxy
#                      
#                      )


# # Create the central data module
# data_description_module <- data_description_module_server("data")

# if seperate data_roi_description - base code on below:  
# data_description_module_server(
#   "desc",
#   roi_vec = roi_vec,
#   countries = countries,
#   process_roi_btn = process_roi_btn,
#   roi_countries = roi_countries,
#   typology_table = typology_table
# )


#------------------------------------------------------------------------------
# to add to xxx



#------------------------------------------------------------------------------
# to add to xxx