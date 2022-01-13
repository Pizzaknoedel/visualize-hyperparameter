#UI Data
DataUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(

  sidebarLayout(
    sidebarPanel(
      id = "data_panel",  width = 3,


                      # selectInput("foo", "Choose", width = '20%',
                      #             multiple = F, selected = "red1",
                      #             choices = list("red 2" = c("red1", "red2"),
                      #                            green = c("green1")),
                      #             selectize = T),

      selectInput(inputId = ns("dataSelect"), label = h5("Select Data"),
                   choices = list("Upload Data", "Test Data" = c("glmnet_ela", "smashy_super")),
                   selected = "glmnet_ela", multiple = FALSE, selectize = T),

      conditionalPanel(
        condition = "input.dataSelect == 'Upload Data'",
        ns = ns,
        selectInput(
        inputId = ns("dataType"), label = h5("Type"), choices = c("csv", "txt", "xlsx", "RData", "rds"),
        selected = "csv"
        ),
      # To be expanded, if more file formats are accepted
      conditionalPanel(
        condition = "input.dataType == 'csv' || input.dataType == 'txt'",
        ns = ns,
        fileInput(
          inputId = ns("dataCsv"), label = h5("Select a File"),
          accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain", "text*")
        ),
        checkboxInput(ns("dataHeader"), "Header", TRUE),
        selectInput(
          inputId = ns("dataSep"), label = h5("Separator"),
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " ", Vertical = "|"),
          selected = ","
        ),
        selectInput(
          inputId = ns("dataQuote"), label = h5("Quote"),
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'
        )
      ),
      conditionalPanel(
        condition = "input.dataType == 'xlsx' || input.dataType == 'xls'",
        ns = ns,
        fileInput(
          inputId = ns("dataXlsx"), label = h5("Select a File"),
          accept = c(".xlsx", ".xls")
        ),
        checkboxInput(ns("dataHeaderXlsx"), "Header", TRUE),
        numericInput(inputId = ns("dataSheet"), label = h5("Sheet"), value = 1)
      ),
      conditionalPanel( condition = "input.dataType == 'RData' || input.dataType == 'rds'",
        ns = ns,
        fileInput(ns("dataRdata"), label = h5("Select a File"),
        accept = c(".RData", ".rds")),
    ))),
    mainPanel(
    width = 9, style="text-align:justify;color:black;background-color:#F6F6F6;padding:15px;border-radius:10px",
    useShinyalert(),
    uiOutput(ns("dataManipulation")),
    div(DT::dataTableOutput(outputId = ns("dataView"))),
    uiOutput(ns("Task_target"))
)))
}
