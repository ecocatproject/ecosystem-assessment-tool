# modules/data_download.R

# Module UI
download_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Download Data",
    card(
      card_body(
        checkboxGroupInput(ns("dataset_choice"), "Choose datasets:",
                           choices = names(list(
                             "Criterion A" = mtcars,
                             "Subcriterion B1" = iris,
                             "Subcriterion B2" = airquality
                           ))),
        br(),
        downloadButton(ns("download_file"), "Download")
      )
    ))
  
  }



# 
# # Module Server
# download_module_server <-  function(id, datasets) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     output$download_file <- downloadHandler(
#       filename = function() {
#         selected <- input$dataset_choice
#         if (length(selected) == 1) {
#           paste0(selected, ".csv")
#         } else {
#           "datasets.zip"
#         }
#       },
#       content = function(file) {
#         selected <- input$dataset_choice
#         tmpdir <- tempdir()
#         files <- c()
#         
#         for (ds in selected) {
#           data <- datasets[[ds]]
#           file_path <- file.path(tmpdir, paste0(ds, ".csv"))
#           write.csv(data, file_path, row.names = FALSE)
#           files <- c(files, file_path)
#         }
#         
#         if (length(files) > 1) {
#           zip::zipr(file, files)  # multiple CSVs → ZIP
#         } else {
#           file.copy(files, file, overwrite = TRUE)  # single CSV
#         }
#       }
#     )
#   })
