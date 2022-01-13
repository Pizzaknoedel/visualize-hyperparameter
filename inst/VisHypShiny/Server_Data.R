DataServer <- function(id) {

  moduleServer(
    id,

    function(input, output, session) {

data <- reactiveValues(originalData = NULL, manipulateData = NULL, subsetData = NULL, selectedCol = NULL, droppedCol = NULL, target = NULL)

# originalData: Data without manipulation (important to reset the data if needed)
# manipulateData: Data with manipulations but without filters (This is important that the filter works in DT)
# subsetData: Data with manipulations and with filters (Final Data for the task)
# selectCol: is a NUMBER which identify the column in the df
# droppedCol: is a STRING for the names of the columns

##################################################################################################################
##################################################### UI #########################################################
##################################################################################################################

#UI for Data Manipulation
output$dataManipulation <- renderUI({

  ns <- session$ns
  req(data$manipulateData)
  req(data$subsetData)

  if(is.null(input$target))
    target <- colnames(data$subsetData)[1]
  else
    target <- data$target

  tagList(

    h5("Choose a Target for the Task", style = "font-size:15px;font-weight: bold;"),
    fluidRow(column(8, selectInput(ns("target"), label = NULL, choices = names(data$manipulateData),
                                   selected = target))),
    hr(style = "border-color: #3e3f3a;"),
    h5("Data Manipulation", style = "font-size:15px;font-weight: bold;"),
    fluidRow(column(2, h5("Select a Column: ")),
             column(4, selectizeInput(inputId = ns("selectColumn"), label = NULL,
                                      choices = names(data$manipulateData),
                                      multiple = FALSE)),
             column(2, actionButton(inputId = ns("Drop_column"), label = "Drop column")),
             column(2, actionButton(inputId = ns("resetData"), label = "Reset Data"), style = "float: right;")),
    fluidRow(column(2, h5("Rename Column: ")),
             column(4, h5(textInput(ns("new_column_name"), placeholder = "New column name...", label = NULL, value = NULL))),
             column(2, h5(actionButton(ns("column_rename"), label = "Rename column")))),
    fluidRow(column(2, h5("Filter Data: ")))
  )
})


#UI-OUTPUT DT
output$dataView <- renderDataTable({

  req(!is.null(data$manipulateData))

  data$manipulateData <- as.data.frame(data$manipulateData)
  n <- length(data$manipulateData)
  for (i in 1:n) {
    if(is.character(data$manipulateData[,i]))
      data$manipulateData[,i] <- as.factor(data$manipulateData[,i])
  }

  datatable(data$manipulateData, filter = 'top', rownames = FALSE, selection  = list(target = "column", mode = 'single'),
            options = list(pageLength = 5, paging = TRUE, bInfo = TRUE, scrollY = "200px", dom = 'tpir', autoWidth = F, ordering = FALSE, scrollX = "200px" ))

})

##################################################################################################################
######################################### observe and observerEvent ##############################################
##################################################################################################################

# current Task
observeEvent(input$target, {
  data$target <- input$target
})

# data upload or selection
observe({
  if ( input$dataSelect != "Upload Data") {
    data$originalData <- get(input$dataSelect)
  }

  else if (input$dataType == "csv" || input$dataType == "txt") {
    filepath <-  input$dataCsv$datapath
    if (!is.null(filepath) && (str_sub(filepath, -4, -1) == ".csv" || (str_sub(filepath, -4, -1) == ".txt" ))) {
      data$originalData <- read.csv(file = filepath, header = input$dataHeader,
                                    sep = input$dataSep, quote = input$dataQuote, stringsAsFactors = TRUE)
    }
  }
  else if (input$dataType == "RData" || input$dataType == "rds") {
    filepath <-  input$dataRdata$datapath
    if (!is.null(filepath) && (str_sub(filepath, -6, -1) == ".RData" || (str_sub(filepath, -4, -1) == ".rds" ))) {
      if(str_sub(filepath, -6, -1) == ".RData"){
        e = new.env()
        name <- load(filepath, envir = e)
        data$originalData <- e[[name]]
      }
      else if (str_sub(filepath, -4, -1) == ".rds") {
        data$originalData <- readRDS(filepath)
      }
    }
  }

  else if (input$dataType == "xlsx") {
    filepath <-  input$dataXlsx$datapath
    if (!is.null(filepath) && (str_sub(filepath, -5, -1) == ".xlsx")) {
      traintibble <- read_excel(path = filepath, col_names = input$dataHeaderXlsx,
                                sheet = input$dataSheet)
      traindf <- as.data.frame(traintibble)
      data$originalData <- modify_at(traindf,
                                     which(as.character(sapply(traindf, class)) == "character"),
                                     as.factor)
    }
  }

  data$manipulateData <- data$originalData
  data$subsetData <- data$originalData
})


#observe selectColumn selectizeInput
observeEvent(input$selectColumn, {
  data$selectedCol <- which(names(data$manipulateData) %in% input$selectColumn)
})

#observe clicks in DT (mark column which is selected)
observeEvent(input$dataView_cell_clicked$col, {
  data$selectedCol <- input$dataView_cell_clicked$col + 1
  updateSelectizeInput(session, "selectColumn", choices = names(data$manipulateData), selected = names(data$manipulateData)[data$selectedCol])
})

#observe the dropcolumn button
observeEvent(input$Drop_column, {

if(ncol(data$manipulateData) > 2)  {
  data$manipulateData <- as.data.frame(data$manipulateData)
  data$subsetData <- as.data.frame(data$subsetData)
  data$manipulateData <- data$manipulateData[,!names(data$manipulateData) %in% names(data$manipulateData)[data$selectedCol]]
  data$subsetData <- data$subsetData[,!names(data$subsetData) %in% names(data$manipulateData)[data$selectedCol]]
} else {
  shinyalert(title = "Minimum number of columns reached!", text = userhelp[["Minimum Column"]], closeOnClickOutside = TRUE, animation = FALSE)
}
})

# observe resetData button
observeEvent(input$resetData, {
 data$manipulateData <- data$originalData
 data$subsetData <- data$originalData
})

#observe rename button
observeEvent(input$column_rename, {

  req(data$manipulateData)
  req(data$selectedCol)

  validname = make.names(input$new_column_name)

  if(str_length(validname)>20) {
    newstring <- str_sub(validname, end=20)
    shinyalert(title = "New Column Name",
               text = userhelp[["New Column Name"]], closeOnClickOutside = TRUE, animation = FALSE)

    updateTextInput(session, "new_column_name", value = newstring)
  }
  else if(str_length(validname)<=20){
    colnames(data$manipulateData)[data$selectedCol] <- validname
    colnames(data$subsetData)[data$selectedCol] <- validname

    data$selectedCol <- NULL

  }

})

#watch for filters
observeEvent(input$dataView_rows_all, {
  req(input$dataView_rows_all)
  #index of selected rows
  index_data <- input$dataView_rows_all
  #subsetData
  data$subsetData <- as.data.frame(data$manipulateData[index_data,])
})


#if a new data set is chosen the data needs to be reseted
observeEvent(input$dataSelect, {
    data$originalData <- NULL
    data$subsetData <- NULL
    data$manipulateData <- NULL
    data$target <- NULL
  })

return(data)
    }
  )
}
