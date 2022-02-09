DataServer <- function(id) {

  moduleServer(
    id,

    function(input, output, session) {

data <- reactiveValues(originalData = NULL, manipulateData = NULL, subsetData = NULL, selectedCol = NULL, droppedCol = NULL, target = NULL, taskRdy = FALSE)

# originalData: data without manipulation (important to reset the data if needed)
# manipulateData: data with manipulations but without filters (this is important that the filter works in DT)
# subsetData: data with manipulations and with filters (final dataset for the task)
# selectCol: is a NUMBER which identify the column in the df
# droppedCol: is a STRING for the names of the columns

##################################################################################################################
##################################################### UI #########################################################
##################################################################################################################

# UI for data manipulation
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
                                      choices = names(data$manipulateData), multiple = TRUE)),
             column(2, actionButton(inputId = ns("Drop_column"), label = "Drop Column")),
             column(2, actionButton(inputId = ns("resetData"), label = "Reset Data"), style = "float: right;")),
    fluidRow(column(2, h5("Rename Column: ")),
             column(4, h5(textInput(ns("new_column_name"), placeholder = "New column name...", label = NULL, value = NULL))),
             column(2, h5(actionButton(ns("column_rename"), label = "Rename Column")))),
    fluidRow(column(2, h5("Filter Data: ")))
  )
})


# UI-OUTPUT DT
output$dataView <- renderDataTable({

  req(!is.null(data$manipulateData))

  data$manipulateData <- as.data.frame(data$manipulateData)
  n <- length(data$manipulateData)
  for (i in 1:n) {
    if(is.character(data$manipulateData[,i]))
      data$manipulateData[,i] <- as.factor(data$manipulateData[,i])
  }

  datatable(data$manipulateData, filter = 'top', rownames = FALSE, selection  = list(target = "column", mode = 'multiple'),
            options = list(pageLength = 5, paging = TRUE, bInfo = TRUE, scrollY = "200px", dom = 'tpir', autoWidth = F, ordering = FALSE, scrollX = "200px" ))

})

##################################################################################################################
######################################### observe and observerEvent ##############################################
##################################################################################################################

# current target of the task
observeEvent(input$target, {
  data$target <- input$target
})

# possibility to upload data or to select a test dataset
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


# observe selectColumn for furhter manipulations
observeEvent(input$selectColumn, {
  data$selectedCol <- which(names(data$manipulateData) %in% input$selectColumn)
})

# observe selected columns in DT (mark column which is selected)
observeEvent(input$dataView_columns_selected, {
  data$selectedCol <- input$dataView_columns_selected + 1 #input$dataView_cell_clicked$col + 1
  updateSelectizeInput(session, "selectColumn", choices = names(data$manipulateData), selected = names(data$manipulateData)[data$selectedCol])
})

# observe the dropColumn button
observeEvent(input$Drop_column, {

if(ncol(data$manipulateData)-length(data$selectedCol) >= 2)  {
  data$manipulateData <- as.data.frame(data$manipulateData)
  data$subsetData <- as.data.frame(data$subsetData)
  data$manipulateData <- data$manipulateData[,!names(data$manipulateData) %in% names(data$manipulateData)[data$selectedCol]]
  data$subsetData <- data$subsetData[,!names(data$subsetData) %in% names(data$manipulateData)[data$selectedCol]]
} else {
  shinyalert(title = "Minimum number of columns reached!", text = userhelp[["Minimum Column"]], closeOnClickOutside = TRUE, animation = FALSE)
}
})

# observe the resetData button
observeEvent(input$resetData, {
 data$manipulateData <- data$originalData
 data$subsetData <- data$originalData
})

# observe the renameColumn button
observeEvent(input$column_rename, {

  req(data$manipulateData)
  req(data$selectedCol)

  validname = make.names(input$new_column_name)

  if (length(data$selectedCol) > 1) {
    shinyalert(title = "More than 1 Column selected",
               text = userhelp[["More Column Selected"]], closeOnClickOutside = TRUE, animation = FALSE)
  }
  else if(str_length(validname)>20) {
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

# watch for filters to update the dataset for the task
observeEvent(input$dataView_rows_all, {
  req(input$dataView_rows_all)
  # index of selected rows
  index_data <- input$dataView_rows_all
  data$subsetData <- as.data.frame(data$manipulateData[index_data,])
})


# if a new data set is chosen the data needs to be reseted
observeEvent(input$dataSelect, {
    data$originalData <- NULL
    data$subsetData <- NULL
    data$manipulateData <- NULL
    data$target <- NULL
    data$taskRdy <- FALSE
  })

# prepare data for the task
observeEvent(data$subsetData, {
if (any(duplicated(names(data$subsetData)))){
  shinyalert(title = "Duplicated Columns",
             text = userhelp[["Duplicated Columns"]], closeOnClickOutside = TRUE, animation = FALSE)
  colNames <- names(data$subsetData)
  newNames <- make.unique(colNames)
  colnames(data$subsetData) <- newNames
  colnames(data$manipulateData) <- newNames
}
  data$subsetData <- as.data.frame(data$subsetData)
  n <- length(data$subsetData)
  for (i in 1:n) {
    if(is.logical(data$subsetData[,i]))
      data$subsetData[,i] <- as.factor(data$subsetData[,i])
    if(is.character(data$subsetData[,i]))
      data$subsetData[,i] <- as.factor(data$subsetData[,i])
  }

  data$taskRdy <- TRUE
})


return(data)
    }
  )
}
