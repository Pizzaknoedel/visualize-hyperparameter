#UI Data
VisualizationUI <- function(id, label = "Visualizaion") {
  ns <- NS(id)
  tagList(

  sidebarLayout(
    sidebarPanel( id = "visualization_panel",  width = 3,
      uiOutput(ns("tabPanelUi")),
      hr(style = "border-color: #3e3f3a;"),
      fluidRow( column(12, uiOutput(ns("choosePlots")))),
      fluidRow( column(12, actionButton(ns("startButton"), label = "Recalculate plots")))
    ),
    mainPanel( width = 9,
      uiOutput(ns("plots"))
    )
),
)
}
