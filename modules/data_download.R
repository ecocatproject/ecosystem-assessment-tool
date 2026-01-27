# modules/data_download.R

# Module UI
# download_module_ui <- function(id) {
#   ns <- NS(id)
#   
#   nav_panel(
#     "Download Data",
#     card(
#       card_body(
#         checkboxGroupInput(ns("dataset_choice"), "Choose datasets:",
#                            choices = names(list(
#                              "Criterion A - area change data" = criterion_a_module$area_change_data,
#                              "Criterion A - area prediction data" = criterion_a_module$area_change_data
#                            ))),
#         br(),
#         downloadButton(ns("download_file"), "Download")
#       )
#     ))
#   
#   }


download_module_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Download Data",
    card(
      card_body(
        checkboxGroupInput(ns("dataset_choice"), "Choose datasets:", choices = NULL),
        br(),
        downloadButton(ns("download_file"), "Download")
      )
    )
  )
}

# module server
download_module_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Update checkboxes whenever available datasets change
    observe({
      ds <- datasets()
      updateCheckboxGroupInput(
        session,
        "dataset_choice",
        choices = names(ds)
      )
    })
    
    output$download_file <- downloadHandler(
      filename = function() "selected_datasets.zip",
      content = function(file) {
        
        ds <- datasets()
        tmpdir <- tempdir()
        file_paths <- c()
        
        safe_filename <- function(name) {
          name <- gsub("[:/\\?<>\\*|]", "_", name, perl = TRUE)
          name <- gsub(" +", "_", name)
          paste0(name, ".csv")
        }
        
        for (name in input$dataset_choice) {
          dat <- ds[[name]]()
          
          file_path <- file.path(tmpdir, safe_filename(name))
          write.csv(dat, file_path, row.names = FALSE)
          
          file_paths <- c(file_paths, file_path)
        }
        
        zip::zip(zipfile = file, files = file_paths)
      }
    )
    
  })
}

# Module Server
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
# }
