library(shiny)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(mlr3pipelines)
library(mlr3)
library(mlr3learners)
library(VisHyp)
library(stringr)
library(DT)
library(data.table)
library(ggplot2)
library(plotly)

#Load UI and Server functions
server_files <- list.files(pattern = "*.R")
server_files <- paste0("", server_files)
server_files <- server_files[!server_files %in% c("app.R")]
for (i in seq_along(server_files)) {
  source(server_files[i], local = TRUE)
}

userhelp <- list(
"Task Creation Target" = "The target variable needs to be of type factor (categorical) or numeric (a number).",
"New Column Name" = "You exceeded 20 character limit for a new column name!",
"Duplicated Columns" = "The data set contains duplicated column names. For this reason the duplicated variable names have been renamed ",
"Minimum Column" = "You need at least 2 remaning columns to create a task.",
"No Plot Selected" = "Please choose at least one plot before pressing the recalculate button.",
"More Column Selected" = "More than one column has been selected for renaming. Please select only one variable!",
"Not enough Parameter" = "Please choose exactly two parameters for the display of a heatmap."
)

#APP UI
ui <- navbarPage(
    title = a("VisHyp", href = "https://github.com/Pizzaknoedel/visualize-hyperparameter", target = "_blank", style = "color: white;"),
    windowTitle = "VisHyp",
    id = "navbar",
    tabPanel("Data",
             DataUI("data"),
             id = 'data'
    ),
    tabPanel("Visualization",
              VisualizationUI("visualization"),
              id = 'visualization'
     )
  )

server <- function(input, output, session) {

  data <- DataServer("data")

  VisualizationServer("visualization", data)

}

shinyApp(ui, server)

