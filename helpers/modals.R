# helpers/modals.R

# =============================================================================
# modals
# =============================================================================
# This script creates the modals used throughout the app:

# 1. start up 
# 2. upload_modal
# 2b.years
# 2c. draw instructions for subsetting
# 3. a quick
# 4. b quick
# 5. b help
# 6. b1
# 7. b2
# currently not the report modal..... 

# =============================================================================


# =============================================================================
# 1.  start up modal
# =============================================================================
start_up_modal <- modalDialog(
    tags$style(HTML(".modal-title { width:100%; text-align:center; }")),
  
  title = div(
    style = "text-align:center; padding-bottom:0.25rem; ",
    tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
           "Welcome to the EcoCAT RLE Assessment Tool​"),
    tags$p(style = "font-size:13px; color:#6b7280; margin:0;",
           "An interactive tool for RLE ecosystem assessments")
  ),
  # title = div(
  #   style = "padding-bottom:0.25rem;",
  #   tags$p(style = "justify-content:center; text-align:center !important; font-size:20px; font-weight:500; margin:0 auto;",
  #          "Welcome to the EcoCAT RLE Assessment Tool"),
  #   tags$p(style = "justify-content:center; text-align:center !important; font-size:13px; color:#6b7280; margin:0 auto;",
  #          "An interactive tool for RLE ecosystem assessments")
  # ),
  # 
  tagList(
    tags$div(
      style = "padding:0.5rem 0.25rem;",
      
      tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; text-transform:uppercase; letter-spacing:0.05em; margin:0 0 0.9rem;",
             "To get started:"),
      
      # Step rows
      purrr::pmap(list(
        n     = 1:3,
        title = c("Upload your ecosystem map(s)",
                  "Review the ecosystem map and classification",
                  "Run the assessment and generate a report"),
        body  = c(
          "GeoTIFF (.tif) format via the sidebar. Upload multiple years to analyse changes in distribution (Criterion A), or try the built-in example dataset.",
          "Explore the distribution map, summary statistics, and ecosystem description in the Description tab.",
          "Use the Criterion A and Criterion B tabs, then download results and a full report from the Report & Data tab."
        )
      ), function(n, title, body) {
        tags$div(
          style = "display:flex; gap:1rem; align-items:flex-start; margin-bottom:1rem;",
          tags$div(
            style = "flex-shrink:0; width:40px; height:40px; border-radius:50%;
                     background:#CBD421; display:flex; align-items:center;
                     justify-content:center;",
            tags$span(style = "font-size:17px; font-weight:500; color:#275e37;", n)
          ),
          tags$div(
            style = "padding-top:6px;",
            tags$p(style = "margin:0 0 3px; font-size:15px; font-weight:500;", title),
            tags$p(style = "margin:0; font-size:13px; color:#6b7280; line-height:1.5;", body)
          )
        )
      }),
      
      # Tip box
              tags$p(
                style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
                tags$b("Tip:"), "Hover over info icons \U001F6C8\ufe0f throughout the app for additional guidance."
              ),
      # security warning
      tags$hr(),
      tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
              tags$b("Security notice:"), "No sign-in is required to access the tool. If an unexpected sign-in pop-up appears, please close it and do not enter any credentials, as it is not part of the tool. If in doubt, contact your IT support team.
")
    )
  ),
  easyClose = FALSE,
  footer = modalButton("Start")
)

# =============================================================================
# 2.  years modal 
# =============================================================================
years_modal <-  function(rows, ns){
  modalDialog(
    tags$div(
      style = "font-size:20px; font-weight:500; margin:0 0 0.2rem; text-align:center; padding-bottom:0.25rem; ",
      tags$img(src = "calendar.png", height = "22px", width = "22px"),
      "Classification years"
    ),
    tags$hr(),
    size      = "m",
    easyClose = FALSE,
    footer    = tagList(
      actionButton(ns("confirm_years"), "Confirm & Close",
                   class = "btn-primary")
    ),
    tags$div(
      style = "margin-bottom:12px; color:#34495e; font-size:14px;",
      "Years were auto-extracted from file names.",
      tagList(tags$strong(" Type or adjust any year below, then click Confirm & Close."),
              " Reopen anytime via the",icon("pencil-alt")," Edit button in section 2 of the sidebar.")
    ),
    tags$table(
      style = "width:100%; border-collapse:collapse; font-size:14px;",
      tags$thead(
        tags$tr(
          style = "background:#f5f6fa;",
          tags$th("Filename",
                  style = "text-align:left; padding:8px 10px;
                             border-bottom:2px solid #ddd;"),
          tags$th("Year",
                  style = "text-align:left; padding:8px 10px;
                             border-bottom:2px solid #ddd;")
        )
      ),
      tags$tbody(lapply(rows, identity))
    )
  )
}

# =============================================================================
# 2b.  upload modal 
# =============================================================================
upload_modal <- modalDialog(
  title = div(
    style = "text-align:center; padding-bottom:0.25rem; ",
    tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
           "Processing classification maps")
  ),

  tagList(
    div(style = "text-align:center;",
        tags$h4("Please wait... "),
        # tags$p("This will take approximately 15 seconds per classification map"),
        # tags$p(tags$b("Criteria A"), "results will be visible on the ",tags$b("'Criteria A'")," tab on completion"),
        tags$p("The ecosystem map and description will be available after completion"),
        
        withSpinner(uiOutput("dummy_spinner"), type = 5, "#669900"),  # CSS spinner
        tags$hr(),
        # Tip box
        tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
          tags$b("Tip:"), "Use map layers to explore changes in ecosystem distribution."
    )
        
    )
  ),
  easyClose = FALSE,
  footer = NULL
)

# =============================================================================
# 2c. draw instructions
# =============================================================================
draw_instructions_modal  <- modalDialog(
  title = div(
    style = "text-align:center; padding-bottom:0.25rem; ",
    tags$p(icon("draw-polygon"), " Refine assessment area (optional)"),
    tags$p(style = "font-size:13px; color:#6b7280; margin:0;",
           "You can restrict the assessment to a specific area using the drawing tools on the map:")
  ),
  tagList(
    tags$div(
      style = "padding:0.5rem 0.25rem;",
      
      tags$p(style = "font-size:12px; font-weight:500; color:#6b7280; text-transform:uppercase; letter-spacing:0.05em; margin:0 0 0.9rem;",
             "Instructions:"),
      
      # Step rows
      purrr::pmap(list(
        n     = 1:3,
        title = c("Find drawing tools",
                  "Draw the area(s) of interest",
                  "Apply area filter "),
        body  = list(
          # tagList mixes text and icon() inline
          tagList(
            "The drawing tools are available on the the left hand side of the map in the", tags$b("Description") ,"tab."
          ),
          tagList(
            "Use the", tags$b("polygon"), tags$span(style = "font-size:1.3em; line-height:1;", "\u2B1F"), " and/or", tags$b("rectangle"),  tags$i(class = "fas fa-square"),
            " tools to draw one or more areas. Drawn areas can be edited",  tags$i(class = "fas fa-pen-to-square"),"or deleted", tags$i(class = "fas fa-trash-alt"),"."
          ),
          tagList(
            "Click ",
            tags$i(class = "fas fa-crop"),
            tags$b("Apply area filter'"),
            " at the bottom of the sidebar panel to restrict assessement to the selected areas. ",
            "To restore the original extent, click '",
            tags$i(class = "fas fa-rotate-left"),
            tags$strong(" Revert to full extent'"), "."
          )
        )
       ), function(n, title, body) {
        tags$div(
          style = "display:flex; gap:1rem; align-items:flex-start; margin-bottom:1rem;",
          tags$div(
            style = "flex-shrink:0; width:40px; height:40px; border-radius:50%;
                     background:#CBD421; display:flex; align-items:center;
                     justify-content:center;",
            tags$span(style = "font-size:17px; font-weight:500; color:#275e37;", n)
          ),
          tags$div(
            style = "padding-top:6px;",
            tags$p(style = "margin:0 0 3px; font-size:15px; font-weight:500;", title),
            tags$p(style = "margin:0; font-size:13px; color:#6b7280; line-height:1.5;", body)
          )
        )
      })
    )
  ),
  tags$hr(),
  # Tip box
  tagList(
    tags$p(
      style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
      tags$b("Tip:"),
      "Click",
      tags$span(
        style = "display:inline-block; font-size:12px; text-decoration:none;
               border:1px solid #669900; border-radius:4px;
               padding:3px 8px; color:#669900;
               white-space:nowrap; vertical-align:middle;",
        bs_icon("pencil"), " How to refine"
      ),
      " at the top of the map or sidebar panel to see the instructions."
    )
  ),
  easyClose = FALSE,
  footer = modalButton("Got it")
)









# =============================================================================
# 3.  criterion a quick modal 
# =============================================================================
a_quick_modal <- modalDialog(
  title = div(
    style = "text-align:center; padding-bottom:0.25rem; ",
    tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
           "Criterion A - quick guide")
    ),
    
    # image of Criterion A criterion taken from the RLE guidelines
    tags$img(
      src = "a_table.png",
      width = "100%",
     style = "margin-top: 15px;margin-bottom: 50px"
    ),
  
  # link to RLE guidelines
  tags$p(
    tags$b("Source: "),
    tags$a(
      href = "https://portals.iucn.org/library/sites/library/files/documents/2024-021-En.pdf",
      "IUCN RLE Guidelines",
      target = "blank",
      style = "color: #2c7fb8;"
    )
  ),
    tags$hr(),
    # Tip box
    tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
            tags$b("Tip:"), "The lines of each legend item can be removed/shown by selecting them in the legend."
    ),
  easyClose = TRUE,
  footer = tagList(modalButton("Close")),
  size = "l"
)

# =============================================================================
# 4.  criterion b quick 
# =============================================================================
b_quick_modal <- modalDialog(
  title = div(
    style = "text-align:center; padding-bottom:0.25rem; ",
    tags$p(style = "font-size:20px; font-weight:500; margin:0 0 0.2rem;",
           "Criterion B - quick guide")
    # tags$p(style = "font-size:13px; color:#6b7280; margin:0;",
    #        "Quick guide")
  ),
  tagList(
    tags$p("Criterion B Assessement - Restricted geographic distribution"),
    tags$ul(
      tags$li(tags$b("B1 (EOO): "), "Click 'Calculate EOO - Subcriteria B1' to compute the minimum convex polygon area."),
      tags$li(tags$b("B2 (AOO): "), "Click 'Calculate AOO - Subcriteria B2' to compute the occupied grid cell area.")
    ),
    # Tip box
    tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
            tags$b("Tip:"), "Pan/zoom the map; results will appear in the boxes below.")
  ),
  easyClose = TRUE,
  footer = tagList(
    modalButton("Close")#,
    # actionButton(("open_help"), "More help")
  ),
  size = "m"
)

# =============================================================================
# 5.  criterion b more help
# =============================================================================
b_help_modal <- modalDialog(
  title = "Criterion B – quick guide",
  tagList(
    tags$p("Criterion B Assessement"),
    tags$ul(
      tags$li(tags$b("B1 (EOO): "), "Click 'Calculate EOO - Subcriteria B1' to compute the minimum convex polygon area."),
      tags$li(tags$b("B2 (AOO): "), "Click 'Calculate AOO - Subcriteria B2' to compute the occupied grid cell area.")
    ),
    # Tip box
    tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
            tags$b("Tip:"), "Pan/zoom the map; results will appear in the boxes below.")
  ),
  easyClose = TRUE,
  footer = tagList(
    modalButton("Close"),
    actionButton(("open_help"), "More help")
  ),
  size = "m"
)

# =============================================================================
# 6.  subcriterion b1
# =============================================================================
b1_modal <- modalDialog(
  title = "Subcriteria B1 Assessment",
  tagList(
    div(style = "text-align:center;",
        tags$h4("Please wait... "),
        tags$p("Computing the minimum convex polygon (MCP) area to measure the ecosystem Extent of Occurrence (EOO)"),
        withSpinner(uiOutput("dummy_spinner"), type = 5, color = "#669900"),  # CSS spinner
        # Tip box
        tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
                tags$b("Tip:"), "Map layers can be toggled on and off; assessment results will appear in the boxes below and downloadable in the report tab.")
    )
  ),
  easyClose = FALSE,
  footer = NULL
)

# =============================================================================
# 7.  subcriterion b2 
# =============================================================================
b2_modal <- modalDialog(
  title = "Subcriteria B2 Assessment",
  tagList(
    div(style = "text-align:center;",
        tags$h4("Please wait... "),
        tags$p("Computing the occupied grid cell area (of 10km grid cells) to measure the ecosystem Area of Occupancy (AOO)"),
        withSpinner(uiOutput("dummy_spinner"), type = 5, color = "#669900"),  # CSS spinner
        tags$p( style = "font-size: 13px; font-style: italic; color: #7f8c8d;",
                tags$b("Tip:"), "Map layers can be toggled on and off; assessment results will appear in the boxes below and downloadable in the report tab.")
        

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