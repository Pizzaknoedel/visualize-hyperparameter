VisualizationServer <- function(id, data) {

  moduleServer(
    id,

    function(input, output, session) {

      ns <- session$ns

      features_to_use <- reactiveValues(features = NULL)

      counter <- reactiveValues(plotNumber = 4, nameIndicator = NULL, plotType = NULL, plotType2 = NULL, plotType3 = NULL, plotType4 = NULL, chosenPlots = NULL)

      learner <- reactiveValues(model = NULL)

      finalPlots <- reactiveValues(plot1 = NULL, plot2 = NULL, plot3 = NULL, plot4 = NULL)
      #reactive values for task
      Task_properties <- reactiveValues(task = NULL, overview = NULL, target = NULL, featTypes = NULL, positive = NULL, tableOptions = NULL, plotRdy = FALSE, featureImputed = NULL)

      # first the plot types need to be initialized.
      # since it is not possible to plot 4 plots at the same time, 4 different initializations of the plot types are required.

      ##################################################################################################################
      ##################################################### UI #########################################################
      ##################################################################################################################



      # UI Plot 1
      plotTypes1 <-   reactive({

        if(is.null(input$selectPlot))
        plotName <- "Importance Plot"
        else
        plotName <- counter$plotType



         tabsetPanel(
             id = ns("FunctionChoice"),
             type = "hidden",
             header = selectInput(ns("selectPlot"), label = h5("Select Plot"),
                          choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                          selected = plotName),
             tabPanel("PDP",
                      fluidRow(column( 12, h5("Select Features"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP"), label = NULL,
                                                          choices = Task_properties$featureImputed,
                                                          multiple = TRUE,
                                                          selected = Task_properties$featureImputed[c(1)],
                                                          options = list(maxItems = 2)))),
                      fluidRow(column( 12, h5("Gridsize"))),
                      fluidRow(column( 12, numericInput(ns("gridsizePDP"), NULL, value = 15))),
                      fluidRow(column( 12, h5("Show Rug"))),
                      fluidRow(column( 12, radioButtons(ns("rugPDP"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = 2, inline = TRUE))),
                      conditionalPanel( condition = paste0("output['", ns("numberFeatures"), "'] == false"),
                      fluidRow(column( 12, h5("Show ICE Curves"))),
                      fluidRow(column( 12, radioButtons(ns("plotIce"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = 2, inline = TRUE))))
             ),
             tabPanel("PCP",
                      fluidRow(column( 12, h5("Select Features"))),
                      fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP"), label = NULL,
                                                       choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                       selected = Task_properties$featureImputed))),
                      fluidRow(column( 12, h5("Restrict Target Range"))),
                      fluidRow(column( 12, sliderInput(ns("constrainRange"), label = NULL, min = 0,
                                                       max = 1, value = c(0,1)))),
                      fluidRow(column( 12, h5("Label Side"))),
                      fluidRow(column( 12, radioButtons(ns("labelSide"), label = NULL,
                                                        choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                        selected = "Top", inline = TRUE))),
                      fluidRow(column( 12, h5("Show Target Name"))),
                      fluidRow(column( 12, radioButtons(ns("labelTarget"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = 1, inline = TRUE))),
                      fluidRow(column( 12, h5("Inverted Color Bar"))),
                      fluidRow(column( 12, radioButtons(ns("colbarReverse"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = 2, inline = TRUE))),
                      fluidRow(column( 12, h5("Automatic Sorting"))),
                      fluidRow(column( 12, radioButtons(ns("autoSort"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = 2, inline = TRUE))),
                      fluidRow(column( 12, h5("Label Angle"))),
                      fluidRow(column( 12, numericInput(ns("labelAngle"), NULL, value = 0))),

             ),
             tabPanel("Heatmap",
                      fluidRow(column( 12, h5("Select Features"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM"), label = NULL,
                                                          choices = Task_properties$featureImputed,
                                                          multiple = TRUE,
                                                          selected = Task_properties$featureImputed[c(1,2)],
                                                          options = list( maxItems = 2)))),
                      fluidRow(column( 12, h5("Choose Function"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction"), label = NULL,
                                                          choices = c("mean","sd"), selected = "mean"))),
                      fluidRow(column( 12, h5("Gridsize"))),
                      fluidRow(column( 12, numericInput(ns("gridsizeHM"), NULL, value = 20))),
                      fluidRow(column( 12, h5("Show Rug"))),
                      fluidRow(column( 12, radioButtons(ns("rugHM"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = 2, inline = TRUE))),
                      fluidRow(column( 12, h5("Show Plotpoints"))),
                      fluidRow(column( 12, radioButtons(ns("plotPoints"), label = NULL,
                                               choices = list("Yes" = 1, "No" = 2),
                                               selected = 2, inline = TRUE)))

             ),
             tabPanel("Importance Plot",
                      fluidRow(column( 12, h5("Select Loss Function"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction"), label = NULL,
                                                          choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                      "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                      "rse", "rrse", "smape"), selected = "mae"))),
             ), selected = plotName
           )
      })

      # UI Plot 2
      plotTypes2 <-   reactive({

        if(is.null(input$selectPlot2))
          plotName2 <- "PDP"
        else
          plotName2 <- counter$plotType2

        tabsetPanel(
          id = ns("FunctionChoice2"),
          type = "hidden",
          header = selectInput(ns("selectPlot2"), label = h5("Select Plot"),
                               choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                               selected = plotName2),
          tabPanel("PDP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP2"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = Task_properties$featureImputed[c(1)],
                                                       options = list(maxItems = 2)))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizePDP2"), NULL, value = 15))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugPDP2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   conditionalPanel( condition = paste0("output['", ns("numberFeatures2"), "'] == false"),
                                     fluidRow(column( 12, h5("Show ICE Curves"))),
                                     fluidRow(column( 12, radioButtons(ns("plotIce2"), label = NULL,
                                                                       choices = list("Yes" = 1, "No" = 2),
                                                                       selected = 2, inline = TRUE))))
          ),
          tabPanel("PCP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP2"), label = NULL,
                                                    choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                    selected = Task_properties$featureImputed))),
                   fluidRow(column( 12, h5("Restrict Target Range"))),
                   fluidRow(column( 12, sliderInput(ns("constrainRange2"), label = NULL, min = 0,
                                                    max = 1, value = c(0,1)))),
                   fluidRow(column( 12, h5("Label Side"))),
                   fluidRow(column( 12, radioButtons(ns("labelSide2"), label = NULL,
                                                     choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                     selected = "Top", inline = TRUE))),
                   fluidRow(column( 12, h5("Show Target Name"))),
                   fluidRow(column( 12, radioButtons(ns("labelTarget2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 1, inline = TRUE))),
                   fluidRow(column( 12, h5("Inverted Color Bar"))),
                   fluidRow(column( 12, radioButtons(ns("colbarReverse2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Automatic Sorting"))),
                   fluidRow(column( 12, radioButtons(ns("autoSort2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Label Angle"))),
                   fluidRow(column( 12, numericInput(ns("labelAngle2"), NULL, value = 0))),

          ),
          tabPanel("Heatmap",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM2"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = Task_properties$featureImputed[c(1,2)],
                                                       options = list( maxItems = 2)))),
                   fluidRow(column( 12, h5("Choose Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction2"), label = NULL,
                                                       choices = c("mean","sd"), selected = "mean"))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizeHM2"), NULL, value = 20))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugHM2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Plotpoints"))),
                   fluidRow(column( 12, radioButtons(ns("plotPoints2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE)))

          ),
          tabPanel("Importance Plot",
                   fluidRow(column( 12, h5("Select Loss Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction2"), label = NULL,
                                                       choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                   "rse", "rrse", "smape"), selected = "mae"))),
          ), selected = plotName2
        )
      })

      # UI Plot 3
      plotTypes3 <-   reactive({

        if(is.null(input$selectPlot3))
          plotName3 <- "PCP"
        else
          plotName3 <- counter$plotType3

        tabsetPanel(
          id = ns("FunctionChoice3"),
          type = "hidden",
          header = selectInput(ns("selectPlot3"), label = h5("Select Plot"),
                               choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                               selected = plotName3),
          tabPanel("PDP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP3"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = Task_properties$featureImputed[c(1)],
                                                       options = list(maxItems = 2)))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizePDP3"), NULL, value = 15))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugPDP3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   conditionalPanel( condition = paste0("output['", ns("numberFeatures3"), "'] == false"),
                                     fluidRow(column( 12, h5("Show ICE Curves"))),
                                     fluidRow(column( 12, radioButtons(ns("plotIce3"), label = NULL,
                                                                       choices = list("Yes" = 1, "No" = 2),
                                                                       selected = 2, inline = TRUE))))
          ),
          tabPanel("PCP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP3"), label = NULL,
                                                    choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                    selected = Task_properties$featureImputed))),
                   fluidRow(column( 12, h5("Restrict Target Range"))),
                   fluidRow(column( 12, sliderInput(ns("constrainRange3"), label = NULL, min = 0,
                                                    max = 1, value = c(0,1)))),
                   fluidRow(column( 12, h5("Label Side"))),
                   fluidRow(column( 12, radioButtons(ns("labelSide3"), label = NULL,
                                                     choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                     selected = "Top", inline = TRUE))),
                   fluidRow(column( 12, h5("Show Target Name"))),
                   fluidRow(column( 12, radioButtons(ns("labelTarget3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 1, inline = TRUE))),
                   fluidRow(column( 12, h5("Inverted Color Bar"))),
                   fluidRow(column( 12, radioButtons(ns("colbarReverse3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Automatic Sorting"))),
                   fluidRow(column( 12, radioButtons(ns("autoSort3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Label Angle"))),
                   fluidRow(column( 12, numericInput(ns("labelAngle3"), NULL, value = 0))),

          ),
          tabPanel("Heatmap",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM3"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = Task_properties$featureImputed[c(1,2)],
                                                       options = list( maxItems = 2)))),
                   fluidRow(column( 12, h5("Choose Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction3"), label = NULL,
                                                       choices = c("mean","sd"), selected = "mean"))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizeHM3"), NULL, value = 20))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugHM3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Plotpoints"))),
                   fluidRow(column( 12, radioButtons(ns("plotPoints3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE)))

          ),
          tabPanel("Importance Plot",
                   fluidRow(column( 12, h5("Select Loss Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction3"), label = NULL,
                                                       choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                   "rse", "rrse", "smape"), selected = "mae"))),
          ), selected = plotName3
        )
      })

      # UI Plot 4
      plotTypes4 <-   reactive({

        if(is.null(input$selectPlot4))
          plotName4 <- "Heatmap"
        else
          plotName4 <- counter$plotType4

        tabsetPanel(
          id = ns("FunctionChoice4"),
          type = "hidden",
          header = selectInput(ns("selectPlot4"), label = h5("Select Plot"),
                               choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                               selected = plotName4),
          tabPanel("PDP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP4"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = Task_properties$featureImputed[c(1)],
                                                       options = list(maxItems = 2)))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizePDP4"), NULL, value = 15))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugPDP4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   conditionalPanel( condition = paste0("output['", ns("numberFeatures4"), "'] == false"),
                                     fluidRow(column( 12, h5("Show ICE Curves"))),
                                     fluidRow(column( 12, radioButtons(ns("plotIce4"), label = NULL,
                                                                       choices = list("Yes" = 1, "No" = 2),
                                                                       selected = 2, inline = TRUE))))
          ),
          tabPanel("PCP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP4"), label = NULL,
                                                    choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                    selected = Task_properties$featureImputed))),
                   fluidRow(column( 12, h5("Restrict Target Range"))),
                   fluidRow(column( 12, sliderInput(ns("constrainRange4"), label = NULL, min = 0,
                                                    max = 1, value = c(0,1)))),
                   fluidRow(column( 12, h5("Label Side"))),
                   fluidRow(column( 12, radioButtons(ns("labelSide4"), label = NULL,
                                                     choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                     selected = "Top", inline = TRUE))),
                   fluidRow(column( 12, h5("Show Target Name"))),
                   fluidRow(column( 12, radioButtons(ns("labelTarget4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 1, inline = TRUE))),
                   fluidRow(column( 12, h5("Inverted Color Bar"))),
                   fluidRow(column( 12, radioButtons(ns("colbarReverse4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Automatic Sorting"))),
                   fluidRow(column( 12, radioButtons(ns("autoSort4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Label Angle"))),
                   fluidRow(column( 12, numericInput(ns("labelAngle4"), NULL, value = 0))),

          ),
          tabPanel("Heatmap",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM4"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = Task_properties$featureImputed[c(1,2)],
                                                       options = list( maxItems = 2)))),
                   fluidRow(column( 12, h5("Choose Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction4"), label = NULL,
                                                       choices = c("mean","sd"), selected = "mean"))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizeHM4"), NULL, value = 20))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugHM4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Plotpoints"))),
                   fluidRow(column( 12, radioButtons(ns("plotPoints4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = 2, inline = TRUE)))

          ),
          tabPanel("Importance Plot",
                   fluidRow(column( 12, h5("Select Loss Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction4"), label = NULL,
                                                       choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                   "rse", "rrse", "smape"), selected = "mae"))),
          ), selected = plotName4
        )
      })

      output$choosePlots <- renderUI({

      ns <- session$ns

      plotNames <- list("1st Plot" = 1, "2nd Plot" = 2, "3rd Plot" = 3, "4th Plot" = 4)

      if(!is.null(input$selectPlot) & !is.null(counter$plotType))
        names(plotNames)[1] <- counter$plotType
      if(!is.null(input$selectPlot2)  & !is.null(counter$plotType2))
        names(plotNames)[2] <- counter$plotType2
      if(!is.null(input$selectPlot3)  & !is.null(counter$plotType3))
        names(plotNames)[3] <- counter$plotType3
      if(!is.null(input$selectPlot4)  & !is.null(counter$plotType4))
        names(plotNames)[4] <- counter$plotType4


      checkboxGroupInput(ns("numberPlots"), label = h5("Choose which plots should be calculated and displayed"),
                         choices = plotNames,
                         selected = c(1,2,3,4), inline = TRUE)

      })

      # UI: condition for the ICE Curves
      # (1st plot)
      output$numberFeatures <- reactive({
        return <- length(input$plotFeaturesPDP) == 2
        return(return)
      })
      outputOptions(output, "numberFeatures", suspendWhenHidden = FALSE)

      # (2nd plot)
      output$numberFeatures2 <- reactive({
        return <- length(input$plotFeaturesPDP2) == 2
        return(return)
      })
      outputOptions(output, "numberFeatures2", suspendWhenHidden = FALSE)

      # (3rd plot)
      output$numberFeatures3 <- reactive({
        return <- length(input$plotFeaturesPDP3) == 2
        return(return)
      })
      outputOptions(output, "numberFeatures3", suspendWhenHidden = FALSE)

      # (4th plot)
      output$numberFeatures4 <- reactive({
        return <- length(input$plotFeaturesPDP4) == 2
        return(return)
      })
      outputOptions(output, "numberFeatures4", suspendWhenHidden = FALSE)


      #UI-OUTPUT TabsetPanel
      output$tabPanelUi <- renderUI({

        if(is.null(counter$nameIndicator))
          selectedPlot <- "Plot 1"
        else
          selectedPlot <- counter$nameIndicator

        tabsetPanel(type = "tabs",
                    id = ns("plotsPanel"),
                    tabPanel(counter$plotType, value = "Plot 1", plotTypes1()),
                    tabPanel(counter$plotType2, value = "Plot 2", plotTypes2()),
                    tabPanel(counter$plotType3, value = "Plot 3", plotTypes3()),
                    tabPanel(counter$plotType4, value = "Plot 4", plotTypes4()),
                    selected = selectedPlot)
      })



      # UI-Output: Plot 1 Calculation
      plot1 <- reactive({

        if(is.null(input$numberPlots))
          return()
        else if(min(input$numberPlots) == 1)
          renderPlotly(finalPlots$plot1)
        else if(min(input$numberPlots) == 2)
          renderPlotly(finalPlots$plot2)
        else if(min(input$numberPlots) == 3)
          renderPlotly(finalPlots$plot3)
        else
          renderPlotly(finalPlots$plot4)
      })

      # UI-Output: Plot 2 Calculation
      plot2 <- reactive({

        if(is.null(input$numberPlots))
          return()

        plot1 <- min(input$numberPlots)
        if(2 %in% input$numberPlots & 2 > plot1)
          renderPlotly(finalPlots$plot2)
        else if(3 %in% input$numberPlots & 3 > plot1)
          renderPlotly(finalPlots$plot3)
        else
          renderPlotly(finalPlots$plot4)
      })

      # UI-Output: Plot 3 Calculation
      plot3 <- reactive({

        if(is.null(input$numberPlots))
          return()

        plot1 <- min(input$numberPlots)
        plot2 <- input$numberPlots[2]
        if(3 %in% input$numberPlots & 3 > plot2)
          renderPlotly(finalPlots$plot3)
        else
          renderPlotly(finalPlots$plot4)
      })

      # UI-Output: Plot 4 Calculation
      plot4 <- reactive({
          renderPlotly(finalPlots$plot4)
      })



      #UI-OUTPUT Plot the output together!
      output$plots <- renderUI({

        req(!is.null(Task_properties$task))
        req(input$FunctionChoice)
        req(counter$plotNumber)
        req()

        if(counter$plotNumber == 1) {
          fluidRow(
            column(12, plot1())
          )

        } else if(counter$plotNumber == 2) {
          fluidPage(
            fluidRow(
              column(12, plot1())
            ),
            fluidRow(
              column(12, plot2())
            )
          )
        } else if(counter$plotNumber == 3) {
          fluidPage(
            fluidRow(
              column(6, plot1()),
              column(6, plot2())
            ),
            fluidRow(
              column(12, plot3())
            ),
          )
        } else if(counter$plotNumber == 4) {
          fluidPage(
            fluidRow(
              column(6, plot1()),
              column(6, plot2())
            ),
            fluidRow(
              column(6, plot3()),
              column(6, plot4())
            ),
          )
        }

})

      ##################################################################################################################
      ######################################### observe and observerEvent ##############################################
      ##################################################################################################################

      observeEvent(input$selectPlot, {
        counter$plotType <- input$selectPlot
        counter$nameIndicator <- "Plot 1"
      })
      observeEvent(input$selectPlot2, {
        counter$plotType2 <- input$selectPlot2
        counter$nameIndicator <- "Plot 2"
      })
      observeEvent(input$selectPlot3, {
        counter$plotType3 <- input$selectPlot3
        counter$nameIndicator <- "Plot 3"
      })
      observeEvent(input$selectPlot4, {
        counter$plotType4 <- input$selectPlot4
        counter$nameIndicator <- "Plot 4"
      })

      #create a classification or regression task
      observe ({

        req(!is.null(data$subsetData))
        req(!is.null(data$target))
        req(data$taskRdy == TRUE)


        Task_properties$target <- data$subsetData[[data$target]]

        if (is.numeric(Task_properties$target)) {
          #Warnung: Error in modifyList: is.list(val) is not TRUE, error because of R6 object into reactive value
          Task_properties$task <- TaskRegr$new(id = "current_task", backend = data$subsetData, target = data$target)

        } else if (is.factor(Task_properties$target)) {
          Task_properties$task <- TaskClassif$new(id = "current_task", backend = data$subsetData, target = data$target)

        } else if(!is.null(Task_properties$target)) {
          shinyalert(title = "Target Selection",
                     text = userhelp[["Task Creation Target"]], closeOnClickOutside = TRUE, animation = FALSE)
        }

        # make the task more robust for feature selection

        n <- length(data$subsetData)
        featureImputed <- c()
        for (i in 1:n) {

          if(is.numeric(data$subsetData[,i])) {
            if(length(unique(data$subsetData[,i])) > 2) {
              newName <- names(data$subsetData[i])
              featureImputed <- c(featureImputed, newName)
            }
          }
          else  {
            if(length(unique(data$subsetData[,i])) > 1) {
              newName <- names(data$subsetData[i])
              featureImputed <- c(featureImputed, newName)
            }
          }
        }

        Task_properties$featureImputed <- featureImputed[!featureImputed %in% data$target]

      })



      # observe the choice of the plot for each tab
      observe({
        req(!is.null(Task_properties$task))
        req(input$selectPlot)
        req(input$plotsPanel)

        updateTabsetPanel(inputId = "FunctionChoice", selected = input$selectPlot)
        updateTabsetPanel(inputId = "FunctionChoice2", selected = input$selectPlot2)
        updateTabsetPanel(inputId = "FunctionChoice3", selected = input$selectPlot3)
        updateTabsetPanel(inputId = "FunctionChoice4", selected = input$selectPlot4)

        Task_properties$plotRdy <- TRUE
      })


      #observeEvent for adding new plots to the tab panel
      observeEvent(input$numberPlots, {
        if(is.null(input$numberPlots))
          counter$plotNumber <- 0
        else
        counter$plotNumber <- length(input$numberPlots)

      })



      # obsereve startButton to recalculate the plots
      observeEvent(input$startButton, {

      req(Task_properties$plotRdy == TRUE)

      if(is.null(input$numberPlots))

      shinyalert(title = "No Plot Selected!", text = userhelp[["No Plot Selected!"]], closeOnClickOutside = TRUE, animation = FALSE)

      withProgress(message = 'Making Plots:', value = 0.0, {

      if(1 %in% input$numberPlots){

        # Increment the progress bar, and update the detail text.
        incProgress( detail = paste0("Calculating ", counter$plotType), amount = 0.2)

        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)

        #Plot 1
        #since the output is always characteristic we need to change every UI element by hand.
        #PDP
        rugPDP <- ifelse(input$rugPDP == 1, TRUE, FALSE)
        plotIce <- ifelse(input$plotIce == 1, TRUE, FALSE)

        #PCP
        autoSort <- ifelse(input$autoSort == 1, TRUE, FALSE)
        colbarReverse <- ifelse(input$colbarReverse == 1, TRUE, FALSE)
        labelTarget <- ifelse(input$labelTarget == 1, TRUE, FALSE)

        #Heatmap
        plotPoints <- ifelse(input$plotPoints == 1, TRUE, FALSE)
        rugHM <- ifelse(input$rugHM == 1, TRUE, FALSE)

        if(input$FunctionChoice == "PDP")
          finalPlots$plot1 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP, learner = learner$model, gridsize = input$gridsizePDP, rug = rugPDP, plotICE = plotIce)
        else if(input$FunctionChoice == "PCP")
          finalPlots$plot1 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP, autosort = autoSort, labelside = input$labelSide, labelangle = input$labelAngle,
                                                     constrainrange = input$constrainRange, labeltarget = labelTarget,  colbarreverse = colbarReverse)
        else if(input$FunctionChoice == "Heatmap")
          if(input$plotFunction == "mean")
            finalPlots$plot1 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM, fun = mean, gridsize = input$gridsizeHM, scatterplot = plotPoints, rug = rugHM)
        else
          finalPlots$plot1 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM, fun = sd, gridsize = input$gridsizeHM, scatterplot = plotPoints, rug = rugHM)
        else if(input$FunctionChoice == "Importance Plot")

          finalPlots$plot1 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction)
      }



        if(2 %in% input$numberPlots){

        # Increment the progress bar, and update the detail text.
        incProgress( detail = paste0("Calculating ", counter$plotType2), amount = 0.2)
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)

        #Plot 2
        #since the output is always characteristic we need to change every UI element by hand.
        #PDP
        rugPDP2 <- ifelse(input$rugPDP2 == 1, TRUE, FALSE)
        plotIce2 <- ifelse(input$plotIce2 == 1, TRUE, FALSE)

        #PCP
        autoSort2 <- ifelse(input$autoSort2 == 1, TRUE, FALSE)
        colbarReverse2 <- ifelse(input$colbarReverse2 == 1, TRUE, FALSE)
        labelTarget2 <- ifelse(input$labelTarget2 == 1, TRUE, FALSE)

        #Heatmap
        plotPoints2 <- ifelse(input$plotPoints2 == 1, TRUE, FALSE)
        rugHM2 <- ifelse(input$rugHM2 == 1, TRUE, FALSE)


        if(input$FunctionChoice2 == "PDP")
          finalPlots$plot2 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP2, learner = learner$model, gridsize = input$gridsizePDP2, rug = rugPDP2, plotICE = plotIce2)
        else if(input$FunctionChoice2 == "PCP")
          finalPlots$plot2 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP2, autosort = autoSort2, labelside = input$labelSide2, labelangle = input$labelAngle2,
                                                     constrainrange = input$constrainRange2, labeltarget = labelTarget2,  colbarreverse = colbarReverse2)
        else if(input$FunctionChoice2 == "Heatmap")
          if(input$plotFunction2 == "mean")
            finalPlots$plot2 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM2, fun = mean, gridsize = input$gridsizeHM2, scatterplot = plotPoints2, rug = rugHM2)
        else
          finalPlots$plot2 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM2, fun = sd, gridsize = input$gridsizeHM2, scatterplot = plotPoints2, rug = rugHM2)
        else if(input$FunctionChoice2 == "Importance Plot")

          finalPlots$plot2 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction2)
        }

        if(3 %in% input$numberPlots){

        # Increment the progress bar, and update the detail text.
        incProgress( detail = paste0("Calculating ", counter$plotType3), amount = 0.2)

        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)

        #Plot 3
        #since the output is always characteristic we need to change every UI element by hand.
        #PDP
        rugPDP3 <- ifelse(input$rugPDP3 == 1, TRUE, FALSE)
        plotIce3 <- ifelse(input$plotIce3 == 1, TRUE, FALSE)

        #PCP
        autoSort3 <- ifelse(input$autoSort3 == 1, TRUE, FALSE)
        colbarReverse3 <- ifelse(input$colbarReverse3 == 1, TRUE, FALSE)
        labelTarget3 <- ifelse(input$labelTarget3 == 1, TRUE, FALSE)

        #Heatmap
        plotPoints3 <- ifelse(input$plotPoints3 == 1, TRUE, FALSE)
        rugHM3 <- ifelse(input$rugHM3 == 1, TRUE, FALSE)


        if(input$FunctionChoice3 == "PDP")
          if(length(input$plotFeaturesPDP3) == 2)
            finalPlots$plot3 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP3, learner = learner$model, gridsize = input$gridsizePDP3, rug = rugPDP3)
        else
          finalPlots$plot3 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP3, learner = learner$model, gridsize = input$gridsizePDP3, rug = rugPDP3, plotICE = plotIce3)

        else if(input$FunctionChoice3 == "PCP")
          finalPlots$plot3 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP3, autosort = autoSort3, labelside = input$labelSide3, labelangle = input$labelAngle3,
                                                     constrainrange = input$constrainRange3, labeltarget = labelTarget3,  colbarreverse = colbarReverse3)
        else if(input$FunctionChoice3 == "Heatmap")
          if(input$plotFunction3 == "mean")
            finalPlots$plot3 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM3, fun = mean, gridsize = input$gridsizeHM3, scatterplot = plotPoints3, rug = rugHM3)
        else
          finalPlots$plot3 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM3, fun = sd, gridsize = input$gridsizeHM3, scatterplot = plotPoints3, rug = rugHM3)
        else if(input$FunctionChoice3 == "Importance Plot")
          finalPlots$plot3 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction3)
        }


        if(4 %in% input$numberPlots){

        # Increment the progress bar, and update the detail text.
        incProgress( detail = paste0("Calculating ", counter$plotType4), amount = 0.2)

        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)

        #Plot 4
        #since the output is always characteristic we need to change every UI element by hand.
        #PDP
        rugPDP4 <- ifelse(input$rugPDP4 == 1, TRUE, FALSE)
        plotIce4 <- ifelse(input$plotIce4 == 1, TRUE, FALSE)

        #PCP
        autoSort4 <- ifelse(input$autoSort4 == 1, TRUE, FALSE)
        colbarReverse4 <- ifelse(input$colbarReverse4 == 1, TRUE, FALSE)
        labelTarget4 <- ifelse(input$labelTarget4 == 1, TRUE, FALSE)

        #Heatmap
        plotPoints4 <- ifelse(input$plotPoints4 == 1, TRUE, FALSE)
        rugHM4 <- ifelse(input$rugHM4 == 1, TRUE, FALSE)

        if(input$FunctionChoice4 == "PDP")
          finalPlots$plot4 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP4, learner = learner$model, gridsize = input$gridsizePDP4, rug = rugPDP4, plotICE = plotIce4)

        else if(input$FunctionChoice4 == "PCP")
          finalPlots$plot4 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP4, autosort = autoSort4, labelside = input$labelSide4, labelangle = input$labelAngle4,
                                                     constrainrange = input$constrainRange4, labeltarget = labelTarget4,  colbarreverse = colbarReverse4)
        else if(input$FunctionChoice4 == "Heatmap")
          if(input$plotFunction4 == "mean")
            finalPlots$plot4 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM4, fun = mean, gridsize = input$gridsizeHM4, scatterplot = plotPoints4, rug = rugHM4)
        else
          finalPlots$plot4 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM4, fun = sd, gridsize = input$gridsizeHM4, scatterplot = plotPoints4, rug = rugHM4)

        else if(input$FunctionChoice4 == "Importance Plot")
          finalPlots$plot4 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction4)
        }

        incProgress( detail = "Rendering Plots", amount = 0.2)

      })
      })

      observe({

      req(!is.null(Task_properties$task))

      if(Task_properties$task$task_type == "regr") {
      learner$model = lrn("regr.ranger")
      } else {
      learner$model = lrn("classif.ranger")
      }

      })


      # #if the data get changed, reset the reactive values of the current task
      observeEvent({data$originalData
        data$subsetData
        data$manipulateData}, {
          Task_properties$task <- NULL
          Task_properties$overview <- NULL
          Task_properties$target <- NULL
          Task_properties$featTypes <- NULL
          Task_properties$positive <- NULL
          Task_properties$tableOptions <- NULL
          features_to_use$features <- NULL
          Task_properties$plotRdy <- FALSE
          Task_properties$featureImputed <- NULL
        })

      # observe the task and filter clean it if necessary. This code block is mostly taken from mlr3Shiny.
      observe({

        req(!is.null(Task_properties$task))

        # get bad features
        allfeat <- Task_properties$task$feature_types
        bad <- c("POSIXct", "complex", "Date")
        badfeat <- allfeat[which(allfeat[, 2]$type %in% bad), ]$id
        features_to_use$features <- allfeat[!badfeat,]$id
        # deactivate unwanted features
        Task_properties$task$select(cols = features_to_use$features)

        if (length(badfeat)) {
          shinyalert(title = "Features Dropped", text = userhelp[["Features Dropped"]], closeOnClickOutside = TRUE, animation = FALSE)
        }

        ### mlr task is R6 Object, Shiny cannot see, when this object's state changes cause its modified in place
        ### to ensure that the table still updates when the features are removed later on, assign it an extra reactive value
        Task_properties$featTypes <- Task_properties$task$feature_types
        if (!identical(Task_properties$task$properties, character(0)) && Task_properties$task$properties == "twoclass") {
          Task_properties$positive <- Task_properties$task$positive
        }
        # add positive label if twoclass
        Task_properties$overview <- list(
          task_id <- Task_properties$task$id,
          task_property = Task_properties$task$properties,
          task_type = Task_properties$task$task_type,
          cols = Task_properties$task$ncol,
          observations = Task_properties$task$nrow,
          target = c(Task_properties$task$target_names),
          features = Task_properties$featTypes
        )
      })







    }
  )
}



