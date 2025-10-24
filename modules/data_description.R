# data module

data_description_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables to store data ----------------------------------------
    # reactive variables to store data ----------------------------------------
    roi_countries <- reactiveVal(data.frame())  # store country stats table
    chirps_summary <- reactiveVal()
    elevation_summary <- reactiveVal()
    rain_data <- reactiveVal()
    temp_data <- reactiveVal()
    typology_table <- reactiveVal(list())     
    
    # xxx ---------------------------------------------------------------------

    
    
    # return the data ---------------------------------------------------------
    return(list(
      # reactive variables to store data ----------------------------------------
      roi_countries = roi_countries,  # store country stats table
      chirps_summary = rchirps_summary,
      elevation_summary = elevation_summary,
      rain_data = rain_data,
      temp_data = temp_data,
      typology_table = typology_table
    ))
  })
}


