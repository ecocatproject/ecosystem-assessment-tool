# helpers/modals.R

# =============================================================================
# modals
# =============================================================================
# This script creates the modals used throughout the app
# 1) start up 
# 2) upload_modal
# 3) a quick
# 4) b quick
# 5) b help
# 6) b1
# 7) b2

# currently not the report modal..... 
# =============================================================================


# =============================================================================
# 1) start up modal
# =============================================================================
start_up_modal <- modalDialog(
    title = div(
      style = "font-size: 20px; font-weight: bold; text-align:center;",
      "Welcome to the EcoCAT RLE Assessment Tool"
    ),
    
    tagList(
      tags$div(
        style = "padding: 10px;",
        
        tags$p(
          style = "font-size: 16px;",
          "This interactive tool assists with IUCN Red List of Ecosystems assessments."
        ),
        
        tags$hr(),
        
        tags$h4("To get started:", style = "text-align:left;"),
        
        tags$ul(
          style = "font-size: 15px;",
          tags$li("Use the sidebar to upload your own ecosystem map(s) (ideally in a raster format)."),
          tags$li("Once loaded, explore summary statistics and spatial outputs."),
          tags$li("Navigate through the tabs to explore different parts of the assessment."),
          tags$li("Criteria A assessment is generated automatically."),
          tags$li("Criteria B assessment requires the user to select the relevent buttons at the bottom of the page."),
          tags$li("A report and all the data can be downloaded in the 'Report/Data download' tab"),
        ),
        
        tags$hr(),
        
        tags$p(
          style = "font-size: 12px; font-style: italic; color: #7f8c8d;",
          tags$b("Tip:"), "Hover over info icons \U001F6C8\ufe0f throughout the app for additional guidance."
        )
      )
    ),
    
    easyClose = FALSE,
    footer = modalButton("Start")
  )

# =============================================================================
# 2) upload modal 
# =============================================================================
upload_modal <- modalDialog(
  title = "Processing classification maps",
  tagList(
    div(style = "text-align:center;",
        tags$h4("Please wait... "),
        # tags$p("This will take approximately 15 seconds per classification map"),
        tags$p(tags$b("Criteria A"), "results will be visible on the ",tags$b("'Criteria A'")," tab on completion"),
        withSpinner(uiOutput("dummy_spinner"), type = 5)  # CSS spinner
    )
  ),
  easyClose = FALSE,
  footer = NULL
)



# =============================================================================
# 3) criterion a quick modal 
# =============================================================================
a_quick_modal <- modalDialog(
  title = "Criterion A – quick guide",
  tagList(
    tags$p("Criterion A Assessement - reduction in geographic distribution over ANY of the following time periods:"),
    tags$ul(
      tags$li(tags$b("A1 - the past 50 years")),
      tags$li(tags$b("A2a - the next 50 years")),
      tags$li(tags$b("A2b - any 50-year period including the past, present and future"))
    ),
    # link to RLE guidelines
    tags$p(
      tags$b("Source: "),
      "RLE Guidelines - ",
      tags$a(
        href = "https://portals.iucn.org/library/sites/library/files/documents/2024-021-En.pdf",
        "Click here",
        target = "blank",
        style = "color: #2c7fb8;"
      )
    ),
    # image of Criterion A criterion taken from the RLE guidelines
    tags$img(
      src = "a_table.png",
      width = "100%",
     style = "margin-top: 15px;margin-bottom: 50px"
    ),
    tags$hr(),
    tags$p("Tip: The lines of each legend item can be removed/shown by selecting them in the legend.")
  ),
  easyClose = TRUE,
  footer = tagList(modalButton("Close")),
  size = "l"
)

# =============================================================================
# criterion b quick 
# =============================================================================
b_quick_modal <- modalDialog(
  title = "Criterion B – quick guide",
  tagList(
    tags$p("Criterion B Assessement"),
    tags$ul(
      tags$li(tags$b("B1 (EOO): "), "Click 'Calculate EOO - Subcriteria B1' to compute the minimum convex polygon area."),
      tags$li(tags$b("B2 (AOO): "), "Click 'Calculate AOO - Subcriteria B2' to compute the occupied grid cell area.")
    ),
    tags$p("Tip: Pan/zoom the map; results will appear in the boxes below.")
  ),
  easyClose = TRUE,
  footer = tagList(
    modalButton("Close"),
    actionButton(("open_help"), "More help")
  ),
  size = "m"
)

# =============================================================================
# criterion b more help
# =============================================================================
b_help_modal <- modalDialog(
  title = "Criterion B – quick guide",
  tagList(
    tags$p("Criterion B Assessement"),
    tags$ul(
      tags$li(tags$b("B1 (EOO): "), "Click 'Calculate EOO - Subcriteria B1' to compute the minimum convex polygon area."),
      tags$li(tags$b("B2 (AOO): "), "Click 'Calculate AOO - Subcriteria B2' to compute the occupied grid cell area.")
    ),
    tags$p("Tip: Pan/zoom the map; results will appear in the boxes below.")
  ),
  easyClose = TRUE,
  footer = tagList(
    modalButton("Close"),
    actionButton(("open_help"), "More help")
  ),
  size = "m"
)

# =============================================================================
# subcriterion b1
# =============================================================================
# b1_modal <- modalDialog(
#   title = "Subcriteria B1",
#   tagList(
#     tags$p("Extent of Occurrence (EOO)"),
#     tags$ul(
#       tags$li(tags$b("B1 (EOO): "), "Computing the minimum convex polygon (MCP) area to measure the ecosystem Extent of Occurrence (EOO)")),
#     tags$p("Tip: Map layers can be toggled on and off; assessment results will appear in the boxes below and downloadable in the report tab."),
#     tagList(p("Please wait..."))
#   ),
#   easyClose = FALSE,
#   footer = tagList(
#     modalButton("Close")#,
#     # actionButton(("open_help"), "More help")
#   ),
#   size = "m"
# )
b1_modal <- modalDialog(
  title = "Subcriteria B1 Assessment",
  tagList(
    div(style = "text-align:center;",
        tags$h4("Please wait... "),
        # tags$p("This will take approximately 15 seconds per classification map"),
        # tags$p(tags$b("Criteria A"), "results will be visible on the ",tags$b("'Criteria A'")," tab on completion"),
        tags$p("Computing the minimum convex polygon (MCP) area to measure the ecosystem Extent of Occurrence (EOO)"),
        withSpinner(uiOutput("dummy_spinner"), type = 5),  # CSS spinner
        tags$p("Tip: Map layers can be toggled on and off; assessment results will appear in the boxes below and downloadable in the report tab.")
    )
  ),
  easyClose = FALSE,
  footer = NULL
)

# =============================================================================
# subcriterion b2 
# =============================================================================
# b2_modal <- modalDialog(
#   title = "Criterion B – quick guide",
#   tagList(
#     tags$p("Area of Occurrence (AOO)"),
#     tags$ul(
#       tags$li(tags$b("B2 (AOO): "), "Computing the occupied grid cell area (of 10km grid cells) to measure the ecosystem Area of Occurrence (AOO)")),
#   tags$p("Tip: Map layers can be toggled on and off; assessment results will appear in the boxes below and downloadable in the report tab."),
#   tagList(p("Please wait..."))
#   ),
#   easyClose = FALSE,
#   footer = tagList(
#     modalButton("Close")#,
#     #actionButton(("open_help"), "More help")
#   ),
#   size = "m"
# )

b2_modal <- modalDialog(
  title = "Subcriteria B2 Assessment",
  tagList(
    div(style = "text-align:center;",
        tags$h4("Please wait... "),
        tags$p("Computing the occupied grid cell area (of 10km grid cells) to measure the ecosystem Area of Occurrence (AOO)"),
        withSpinner(uiOutput("dummy_spinner"), type = 5),  # CSS spinner
        tags$p("Tip: Map layers can be toggled on and off; assessment results will appear in the boxes below and downloadable in the report tab.")
        

    )
  ),
  easyClose = FALSE,
  footer = NULL
)


# =============================================================================
# # report / download
# =============================================================================
# report_modal <- modalDialog(
#  tagList(
#     div(
#       style = "text-align:center;",
#       tags$h4("Please wait while your RLE assessment report is being generated."),
#      # tags$p("This process usually takes approximately 10 seconds."),
#       tags$p(tags$em(paste0("Rendering the ", input$report_format, " report..."))),
#       withSpinner(uiOutput(("dummy_spinner")), type = 5)
#     )
#   ),
#   easyClose = TRUE,
#   footer = NULL
# )
# report_modal <- modalDialog(
#   title = "Criterion A – quick guide",
#   tagList(
#     tags$p("Criterion A Assessement"),
#     tags$ul(
#       tags$li(tags$b("A1 (past 50 years): "), "text here")
#     ),
#     tags$p("Tip: The lines of each graph can be turn on and off by selecting them")
#   ),
#   easyClose = TRUE,
#   footer = tagList(
#     modalButton("Close")#,
#     # actionButton(("open_help"), "More help")
#   ),
#   size = "m"
# )