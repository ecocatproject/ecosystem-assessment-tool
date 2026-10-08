# modules/data_download.R

#-------------------------------------------------------------------------------
# Module UI
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# module server
#-------------------------------------------------------------------------------

download_module_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Update checkboxes whenever available datasets change
    # observe({
    #   ds <- datasets()
    #   updateCheckboxGroupInput(
    #     session,
    #     "dataset_choice",
    #     choices = names(ds),
    #   )
    # })

    observe({
      ds <- datasets()
      req(length(ds) > 0)
      
      choices <- c(names(ds), "Select All")
      
      updateCheckboxGroupInput(
        session,
        "dataset_choice",
        choices = choices,
        selected = "Select All"   # only this is selected initially
      )
    })
    
    observeEvent(input$dataset_choice, {
      ds_names <- names(datasets())
      
      # If "Select All" is clicked → select everything
      if ("Select All" %in% input$dataset_choice) {
        updateCheckboxGroupInput(
          session,
          "dataset_choice",
          selected = ds_names
        )
      }
    })
    
    observeEvent(input$dataset_choice, {
      ds_names <- names(datasets())
      
      if ("Select All" %in% input$dataset_choice) {
        updateCheckboxGroupInput(
          session,
          "dataset_choice",
          selected = ds_names
        )
      } else if (setequal(input$dataset_choice, ds_names)) {
        # If user manually deselects one → remove all
        updateCheckboxGroupInput(
          session,
          "dataset_choice",
          selected = character(0)
        )
      }
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

