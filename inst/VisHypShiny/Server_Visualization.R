VisualizationServer <- function(id, data) {

  moduleServer(
    id,

    function(input, output, session) {

      ns <- session$ns

      #reactive values for task
      Task_properties <- reactiveValues(task = NULL, overview = NULL, target = NULL, featTypes = NULL, positive = NULL, tableOptions = NULL, plotRdy = FALSE, featureImputed = NULL)

      features_to_use <- reactiveValues(features = NULL)

      learner <- reactiveValues(model = NULL)

      #count plots + additional information
      counter <- reactiveValues(plotNumber = 4, nameIndicator = NULL, plotType = NULL, plotType2 = NULL, plotType3 = NULL, plotType4 = NULL, chosenPlots = NULL)

      #final plot settings: ggplot or plotly object
      finalPlots <- reactiveValues(plot1 = NULL, plot2 = NULL, plot3 = NULL, plot4 = NULL)

      #plot 1 Initialize values
      settingPlot1 <- reactiveValues(plotName = "Importance Plot", featuresPDP = NULL, featuresHM = NULL, featuresPCP = NULL, gridsize = 15, rug = 2, ICE = 2, targetRange = c(0,1), labelSide = "Top",
                                     labelTarget = 1, colbarReverse = 2, autoSort = 2, labelAngle = 0, plotFunction = "mean", plotPoints = 2, lossFunction = "mae")
      #plot 2 Initialize values
      settingPlot2 <- reactiveValues(plotName = "PDP", featuresPDP = NULL, featuresHM = NULL, featuresPCP = NULL, gridsize = 15, rug = 2, ICE = 2, targetRange = c(0,1), labelSide = "Top",
                                     labelTarget = 1, colbarReverse = 2, autoSort = 2, labelAngle = 0, plotFunction = "mean", plotPoints = 2, lossFunction = "mae")
      #plot 3 Initialize values
      settingPlot3 <- reactiveValues(plotName = "PCP", featuresPDP = NULL, featuresHM = NULL, featuresPCP = NULL, gridsize = 15, rug = 2, ICE = 2, targetRange = c(0,1), labelSide = "Top",
                                     labelTarget = 1, colbarReverse = 2, autoSort = 2, labelAngle = 0, plotFunction = "mean", plotPoints = 2, lossFunction = "mae")
      #plot 4 Initialize values
      settingPlot4 <- reactiveValues(plotName = "Heatmap", featuresPDP = NULL, featuresHM = NULL, featuresPCP = NULL, gridsize = 15, rug = 2, ICE = 2, targetRange = c(0,1), labelSide = "Top",
                                     labelTarget = 1, colbarReverse = 2, autoSort = 2, labelAngle = 0, plotFunction = "mean", plotPoints = 2, lossFunction = "mae")

      ##################################################################################################################
      ##################################################### UI #########################################################
      ##################################################################################################################

      # first the plot types need to be initialized.
      # since it is not possible to plot 4 plots at the same time, 4 different initializations of the plot types are required.


      # UI Plot 1
      plotTypes1 <- reactive({

        # initialize selected values for the 1st plot and always use current settings!
        if(is.null(counter$plotType))
          settingPlot1$plotName <- settingPlot1$plotName
        else if(!is.null(input$selectPlot))
          settingPlot1$plotName <- counter$plotType

        if(settingPlot1$plotName == "PDP") {
          if(is.null(input$plotFeaturesPDP))
            settingPlot1$featuresPDP <- Task_properties$featureImputed[c(1)]
          else settingPlot1$featuresPDP <- input$plotFeaturesPDP
          if(!is.null(input$gridsizePDP))
            settingPlot1$gridsize <- input$gridsizePDP
          if(!is.null(input$rugPDP))
            settingPlot1$rug <- input$rugPDP
        }
        else if(settingPlot1$plotName == "PCP") {
          if(is.null(input$plotFeaturesPCP))
            settingPlot1$featuresPCP <- Task_properties$featureImputed
          else settingPlot1$featuresPCP <- input$plotFeaturesPCP
        }
        else if(settingPlot1$plotName == "Heatmap") {
          if(is.null(input$plotFeaturesHM))
            settingPlot1$featuresHM <- Task_properties$featureImputed[c(1,2)]
          else settingPlot1$featuresHM <- input$plotFeaturesHM
          if(!is.null(input$gridsizeHM))
            settingPlot1$gridsize <- input$gridsizeHM
          if(!is.null(input$rugHM))
            settingPlot1$rug <- input$rugHM
        }

        if(!is.null(input$plotIce))
         settingPlot1$ICE <- input$plotIce

        if(!is.null(input$constrainRange))
          settingPlot1$targetRange <- input$constrainRange

        if(is.null(input$labelSide))
          settingPlot1$labelSide <- input$labelSide

        if(!is.null(input$labelTarget))
          settingPlot1$labelTarget <- input$labelTarget

        if(!is.null(input$colbarReverse))
          settingPlot1$colbarReverse <- input$colbarReverse

        if(!is.null(input$autoSort))
          settingPlot1$autoSort <- input$autoSort

        if(!is.null(input$labelAngle))
          settingPlot1$labelAngle <- input$labelAngle

        if(!is.null(input$plotFunction))
          settingPlot1$plotFunction <- input$plotFunction

        if(!is.null(input$plotPoints))
          settingPlot1$plotPoints <- input$plotPoints

        if(!is.null(input$lossFunction))
          settingPlot1$lossFunction <- input$lossFunction

        # UI for the first plot
          tabsetPanel(
             id = ns("FunctionChoice"),
             type = "hidden",
             header = selectInput(ns("selectPlot"), label = h5("Select Plot"),
                          choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                          selected = settingPlot1$plotName),
             tabPanel("PDP",
                      fluidRow(column( 12, h5("Select Features"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP"), label = NULL,
                                                          choices = Task_properties$featureImputed,
                                                          multiple = TRUE,
                                                          selected = settingPlot1$featuresPDP,
                                                          options = list(maxItems = 2)))),
                      fluidRow(column( 12, h5("Gridsize"))),
                      fluidRow(column( 12, numericInput(ns("gridsizePDP"), NULL, value = settingPlot1$gridsize))),
                      fluidRow(column( 12, h5("Show Rug"))),
                      fluidRow(column( 12, radioButtons(ns("rugPDP"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = settingPlot1$rug, inline = TRUE))),
                      conditionalPanel( condition = paste0("output['", ns("numberFeatures"), "'] == false"),
                      fluidRow(column( 12, h5("Show ICE Curves"))),
                      fluidRow(column( 12, radioButtons(ns("plotIce"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = settingPlot1$ICE, inline = TRUE))))
             ),
             tabPanel("PCP",
                      fluidRow(column( 12, h5("Select Features"))),
                      fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP"), label = NULL,
                                                       choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                       selected = settingPlot1$featuresPCP))),
                      fluidRow(column( 12, h5("Restrict Target Range"))),
                      fluidRow(column( 12, sliderInput(ns("constrainRange"), label = NULL, min = 0,
                                                       max = 1, value = settingPlot1$targetRange))),
                      fluidRow(column( 12, h5("Label Side"))),
                      fluidRow(column( 12, radioButtons(ns("labelSide"), label = NULL,
                                                        choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                        selected = settingPlot1$labelSide, inline = TRUE))),
                      fluidRow(column( 12, h5("Show Target Name"))),
                      fluidRow(column( 12, radioButtons(ns("labelTarget"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = settingPlot1$labelTarget, inline = TRUE))),
                      fluidRow(column( 12, h5("Inverted Color Bar"))),
                      fluidRow(column( 12, radioButtons(ns("colbarReverse"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = settingPlot1$colbarReverse, inline = TRUE))),
                      fluidRow(column( 12, h5("Automatic Sorting"))),
                      fluidRow(column( 12, radioButtons(ns("autoSort"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = settingPlot1$autoSort, inline = TRUE))),
                      fluidRow(column( 12, h5("Label Angle"))),
                      fluidRow(column( 12, numericInput(ns("labelAngle"), NULL, value = settingPlot1$labelAngle))),

             ),
             tabPanel("Heatmap",
                      fluidRow(column( 12, h5("Select Features"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM"), label = NULL,
                                                          choices = Task_properties$featureImputed,
                                                          multiple = TRUE,
                                                          selected = settingPlot1$featuresHM,
                                                          options = list( maxItems = 2)))),
                      fluidRow(column( 12, h5("Choose Function"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction"), label = NULL,
                                                          choices = c("mean","sd"), selected = settingPlot1$plotFunction))),
                      fluidRow(column( 12, h5("Gridsize"))),
                      fluidRow(column( 12, numericInput(ns("gridsizeHM"), NULL, value = settingPlot1$gridsize))),
                      fluidRow(column( 12, h5("Show Rug"))),
                      fluidRow(column( 12, radioButtons(ns("rugHM"), label = NULL,
                                                        choices = list("Yes" = 1, "No" = 2),
                                                        selected = settingPlot1$rug, inline = TRUE))),
                      fluidRow(column( 12, h5("Show Plotpoints"))),
                      fluidRow(column( 12, radioButtons(ns("plotPoints"), label = NULL,
                                               choices = list("Yes" = 1, "No" = 2),
                                               selected = settingPlot1$plotPoints, inline = TRUE)))

             ),
             tabPanel("Importance Plot",
                      fluidRow(column( 12, h5("Select Loss Function"))),
                      fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction"), label = NULL,
                                                          choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                      "mdae", "msle", "percent_bias", "rae", "rmsle",
                                                                      "rse", "rrse", "smape"), selected = settingPlot1$lossFunction))),
             ), selected = settingPlot1$plotName
           )
      })

      # UI Plot 2

      plotTypes2 <-   reactive({

        # initialize selected values for the 2nd plot and always use current settings!
        if(is.null(counter$plotType2))
          settingPlot2$plotName <- settingPlot2$plotName
        else if(!is.null(input$selectPlot2))
          settingPlot2$plotName <- counter$plotType2


        if(settingPlot2$plotName == "PDP") {
          if(is.null(input$plotFeaturesPDP2))
            settingPlot2$featuresPDP <- Task_properties$featureImputed[c(1)]
          else settingPlot2$featuresPDP <- input$plotFeaturesPDP2
          if(!is.null(input$gridsizePDP2))
            settingPlot2$gridsize <- input$gridsizePDP2
          if(!is.null(input$rugPDP2))
            settingPlot2$rug <- input$rugPDP2
        }
        else if(settingPlot2$plotName == "PCP") {
          if(is.null(input$plotFeaturesPCP2))
            settingPlot2$featuresPCP <- Task_properties$featureImputed
          else settingPlot2$featuresPCP <- input$plotFeaturesPCP2
        }
        else if(settingPlot2$plotName == "Heatmap") {
          if(is.null(input$plotFeaturesHM2))
            settingPlot2$featuresHM <- Task_properties$featureImputed[c(1,2)]
          else settingPlot2$featuresHM <- input$plotFeaturesHM2
          if(!is.null(input$gridsizeHM2))
            settingPlot2$gridsize <- input$gridsizeHM2
          if(!is.null(input$rugHM2))
            settingPlot2$rug <- input$rugHM2
        }

        if(!is.null(input$plotIce2))
          settingPlot2$ICE <- input$plotIce2

        if(!is.null(input$constrainRange2))
          settingPlot2$targetRange <- input$constrainRange2

        if(is.null(input$labelSide2))
          settingPlot2$labelSide <- input$labelSide2

        if(!is.null(input$labelTarget2))
          settingPlot2$labelTarget <- input$labelTarget2

        if(!is.null(input$colbarReverse2))
          settingPlot2$colbarReverse <- input$colbarReverse2

        if(!is.null(input$autoSort2))
          settingPlot2$autoSort <- input$autoSort2

        if(!is.null(input$labelAngle2))
          settingPlot2$labelAngle <- input$labelAngle2

        if(!is.null(input$plotFunction2))
          settingPlot2$plotFunction <- input$plotFunction2

        if(!is.null(input$plotPoints2))
          settingPlot2$plotPoints <- input$plotPoints2

        if(!is.null(input$lossFunction2))
          settingPlot2$lossFunction <- input$lossFunction2

        # UI for the second plot
        tabsetPanel(
          id = ns("FunctionChoice2"),
          type = "hidden",
          header = selectInput(ns("selectPlot2"), label = h5("Select Plot"),
                               choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                               selected = settingPlot2$plotName),
          tabPanel("PDP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP2"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = settingPlot2$featuresPDP,
                                                       options = list(maxItems = 2)))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizePDP2"), NULL, value = settingPlot2$gridsize))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugPDP2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot2$rug, inline = TRUE))),
                   conditionalPanel( condition = paste0("output['", ns("numberFeatures2"), "'] == false"),
                                     fluidRow(column( 12, h5("Show ICE Curves"))),
                                     fluidRow(column( 12, radioButtons(ns("plotIce2"), label = NULL,
                                                                       choices = list("Yes" = 1, "No" = 2),
                                                                       selected = settingPlot2$ICE, inline = TRUE))))
          ),
          tabPanel("PCP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP2"), label = NULL,
                                                    choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                    selected = settingPlot2$featuresPCP))),
                   fluidRow(column( 12, h5("Restrict Target Range"))),
                   fluidRow(column( 12, sliderInput(ns("constrainRange2"), label = NULL, min = 0,
                                                    max = 1, value = settingPlot2$targetRange))),
                   fluidRow(column( 12, h5("Label Side"))),
                   fluidRow(column( 12, radioButtons(ns("labelSide2"), label = NULL,
                                                     choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                     selected = settingPlot2$labelSide, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Target Name"))),
                   fluidRow(column( 12, radioButtons(ns("labelTarget2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot2$labelTarget, inline = TRUE))),
                   fluidRow(column( 12, h5("Inverted Color Bar"))),
                   fluidRow(column( 12, radioButtons(ns("colbarReverse2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot2$colbarReverse, inline = TRUE))),
                   fluidRow(column( 12, h5("Automatic Sorting"))),
                   fluidRow(column( 12, radioButtons(ns("autoSort2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot2$autoSort, inline = TRUE))),
                   fluidRow(column( 12, h5("Label Angle"))),
                   fluidRow(column( 12, numericInput(ns("labelAngle2"), NULL, value = settingPlot2$labelAngle))),

          ),
          tabPanel("Heatmap",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM2"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = settingPlot2$featuresHM,
                                                       options = list( maxItems = 2)))),
                   fluidRow(column( 12, h5("Choose Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction2"), label = NULL,
                                                       choices = c("mean","sd"), selected = settingPlot2$plotFunction))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizeHM2"), NULL, value = settingPlot2$gridsize))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugHM2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot2$rug, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Plotpoints"))),
                   fluidRow(column( 12, radioButtons(ns("plotPoints2"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot2$plotPoints, inline = TRUE)))

          ),
          tabPanel("Importance Plot",
                   fluidRow(column( 12, h5("Select Loss Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction2"), label = NULL,
                                                       choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                   "rse", "rrse", "smape"), selected = settingPlot2$lossFunction))),
          ), selected = settingPlot2$plotName
        )
      })

      # UI Plot 3
      plotTypes3 <-   reactive({

      # initialize selected values for the 3rd plot and always use current settings!
        if(is.null(counter$plotType3))
          settingPlot3$plotName <- settingPlot3$plotName
        else if(!is.null(input$selectPlot3))
          settingPlot3$plotName <- counter$plotType3
        if(settingPlot3$plotName == "PDP") {
          if(is.null(input$plotFeaturesPDP3))
            settingPlot3$featuresPDP <- Task_properties$featureImputed[c(1)]
          else settingPlot3$featuresPDP <- input$plotFeaturesPDP3
          if(!is.null(input$gridsizePDP3))
            settingPlot3$gridsize <- input$gridsizePDP3
          if(!is.null(input$rugPDP3))
            settingPlot3$rug <- input$rugPDP3
        }
        else if(settingPlot3$plotName == "PCP") {
          if(is.null(input$plotFeaturesPCP3))
            settingPlot3$featuresPCP <- Task_properties$featureImputed
          else settingPlot3$featuresPCP <- input$plotFeaturesPCP3
        }
        else if(settingPlot3$plotName == "Heatmap") {
          if(is.null(input$plotFeaturesHM3))
            settingPlot3$featuresHM <- Task_properties$featureImputed[c(1,2)]
          else settingPlot3$featuresHM <- input$plotFeaturesHM3
          if(!is.null(input$gridsizeHM3))
            settingPlot3$gridsize <- input$gridsizeHM3
          if(!is.null(input$rugHM3))
            settingPlot3$rug <- input$rugHM3
        }

        if(!is.null(input$plotIce3))
          settingPlot3$ICE <- input$plotIce3

        if(!is.null(input$constrainRange3))
          settingPlot3$targetRange <- input$constrainRange3

        if(is.null(input$labelSide3))
          settingPlot3$labelSide <- input$labelSide3

        if(!is.null(input$labelTarget3))
          settingPlot3$labelTarget <- input$labelTarget3

        if(!is.null(input$colbarReverse3))
          settingPlot3$colbarReverse <- input$colbarReverse3

        if(!is.null(input$autoSort3))
          settingPlot3$autoSort <- input$autoSort3

        if(!is.null(input$labelAngle3))
          settingPlot3$labelAngle <- input$labelAngle3

        if(!is.null(input$plotFunction3))
          settingPlot3$plotFunction <- input$plotFunction3

        if(!is.null(input$plotPoints3))
          settingPlot3$plotPoints <- input$plotPoints3

        if(!is.null(input$lossFunction3))
          settingPlot3$lossFunction <- input$lossFunction3

        # UI for the third plot
        tabsetPanel(
          id = ns("FunctionChoice3"),
          type = "hidden",
          header = selectInput(ns("selectPlot3"), label = h5("Select Plot"),
                               choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                               selected = settingPlot3$plotName),
          tabPanel("PDP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP3"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = settingPlot3$featuresPDP,
                                                       options = list(maxItems = 2)))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizePDP3"), NULL, value = settingPlot3$gridsize))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugPDP3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot3$rug, inline = TRUE))),
                   conditionalPanel( condition = paste0("output['", ns("numberFeatures3"), "'] == false"),
                                     fluidRow(column( 12, h5("Show ICE Curves"))),
                                     fluidRow(column( 12, radioButtons(ns("plotIce3"), label = NULL,
                                                                       choices = list("Yes" = 1, "No" = 2),
                                                                       selected = settingPlot3$ICE, inline = TRUE))))
          ),
          tabPanel("PCP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP3"), label = NULL,
                                                    choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                    selected = settingPlot3$featuresPCP))),
                   fluidRow(column( 12, h5("Restrict Target Range"))),
                   fluidRow(column( 12, sliderInput(ns("constrainRange3"), label = NULL, min = 0,
                                                    max = 1, value = settingPlot3$targetRange))),
                   fluidRow(column( 12, h5("Label Side"))),
                   fluidRow(column( 12, radioButtons(ns("labelSide3"), label = NULL,
                                                     choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                     selected = settingPlot3$labelSide, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Target Name"))),
                   fluidRow(column( 12, radioButtons(ns("labelTarget3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot3$labelTarget, inline = TRUE))),
                   fluidRow(column( 12, h5("Inverted Color Bar"))),
                   fluidRow(column( 12, radioButtons(ns("colbarReverse3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot3$colbarReverse, inline = TRUE))),
                   fluidRow(column( 12, h5("Automatic Sorting"))),
                   fluidRow(column( 12, radioButtons(ns("autoSort3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot3$autoSort, inline = TRUE))),
                   fluidRow(column( 12, h5("Label Angle"))),
                   fluidRow(column( 12, numericInput(ns("labelAngle3"), NULL, value = settingPlot3$labelAngle))),

          ),
          tabPanel("Heatmap",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM3"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = settingPlot3$featuresHM,
                                                       options = list( maxItems = 2)))),
                   fluidRow(column( 12, h5("Choose Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction3"), label = NULL,
                                                       choices = c("mean","sd"), selected = settingPlot3$plotFunction))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizeHM3"), NULL, value = settingPlot3$gridsize))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugHM3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot3$rug, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Plotpoints"))),
                   fluidRow(column( 12, radioButtons(ns("plotPoints3"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot3$plotPoints, inline = TRUE)))

          ),
          tabPanel("Importance Plot",
                   fluidRow(column( 12, h5("Select Loss Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction3"), label = NULL,
                                                       choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                   "rse", "rrse", "smape"), selected = settingPlot3$lossFunction))),
          ), selected = settingPlot3$plotName
        )
      })

      # UI Plot 4
      plotTypes4 <-   reactive({

        # initialize selected values for the 4th plotand always use current settings!
        if(is.null(counter$plotType4))
          settingPlot4$plotName <- settingPlot4$plotName
        else if(!is.null(input$selectPlot4))
          settingPlot4$plotName <- counter$plotType4
        if(settingPlot4$plotName == "PDP") {
          if(is.null(input$plotFeaturesPDP4))
            settingPlot4$featuresPDP <- Task_properties$featureImputed[c(1)]
          else settingPlot4$featuresPDP <- input$plotFeaturesPDP4
          if(!is.null(input$gridsizePDP4))
            settingPlot4$gridsize <- input$gridsizePDP4
          if(!is.null(input$rugPDP4))
            settingPlot4$rug <- input$rugPDP4
        }
        else if(settingPlot4$plotName == "PCP") {
          if(is.null(input$plotFeaturesPCP4))
            settingPlot4$featuresPCP <- Task_properties$featureImputed
          else settingPlot4$featuresPCP <- input$plotFeaturesPCP4
        }
        else if(settingPlot4$plotName == "Heatmap") {
          if(is.null(input$plotFeaturesHM4))
            settingPlot4$featuresHM <- Task_properties$featureImputed[c(1,2)]
          else settingPlot4$featuresHM <- input$plotFeaturesHM4
          if(!is.null(input$gridsizeHM4))
            settingPlot4$gridsize <- input$gridsizeHM4
          if(!is.null(input$rugHM4))
            settingPlot4$rug <- input$rugHM4
        }

        if(!is.null(input$plotIce4))
          settingPlot4$ICE <- input$plotIce4

        if(!is.null(input$constrainRange4))
          settingPlot4$targetRange <- input$constrainRange4

        if(is.null(input$labelSide4))
          settingPlot4$labelSide <- input$labelSide4

        if(!is.null(input$labelTarget4))
          settingPlot4$labelTarget <- input$labelTarget4

        if(!is.null(input$colbarReverse4))
          settingPlot4$colbarReverse <- input$colbarReverse4

        if(!is.null(input$autoSort4))
          settingPlot4$autoSort <- input$autoSort4

        if(!is.null(input$labelAngle4))
          settingPlot4$labelAngle <- input$labelAngle4

        if(!is.null(input$plotFunction4))
          settingPlot4$plotFunction <- input$plotFunction4

        if(!is.null(input$plotPoints4))
          settingPlot4$plotPoints <- input$plotPoints4

        if(!is.null(input$lossFunction4))
          settingPlot4$lossFunction <- input$lossFunction4

        # UI for the third plot
        tabsetPanel(
          id = ns("FunctionChoice4"),
          type = "hidden",
          header = selectInput(ns("selectPlot4"), label = h5("Select Plot"),
                               choices = list("PDP", "PCP", "Heatmap","Importance Plot"),
                               selected = settingPlot4$plotName),
          tabPanel("PDP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesPDP4"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = settingPlot4$featuresPDP,
                                                       options = list(maxItems = 2)))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizePDP4"), NULL, value = settingPlot4$gridsize))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugPDP4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot4$rug, inline = TRUE))),
                   conditionalPanel( condition = paste0("output['", ns("numberFeatures4"), "'] == false"),
                                     fluidRow(column( 12, h5("Show ICE Curves"))),
                                     fluidRow(column( 12, radioButtons(ns("plotIce4"), label = NULL,
                                                                       choices = list("Yes" = 1, "No" = 2),
                                                                       selected = settingPlot4$ICE, inline = TRUE))))
          ),
          tabPanel("PCP",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, pickerInput(ns("plotFeaturesPCP4"), label = NULL,
                                                    choices = Task_properties$featureImputed, options = list(`actions-box` = TRUE), multiple = T,
                                                    selected = settingPlot4$featuresPCP))),
                   fluidRow(column( 12, h5("Restrict Target Range"))),
                   fluidRow(column( 12, sliderInput(ns("constrainRange4"), label = NULL, min = 0,
                                                    max = 1, value = settingPlot4$targetRange))),
                   fluidRow(column( 12, h5("Label Side"))),
                   fluidRow(column( 12, radioButtons(ns("labelSide4"), label = NULL,
                                                     choices = list("Top" = "Top", "Bottom" = "Bottom"),
                                                     selected = settingPlot4$labelSide, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Target Name"))),
                   fluidRow(column( 12, radioButtons(ns("labelTarget4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot4$labelTarget, inline = TRUE))),
                   fluidRow(column( 12, h5("Inverted Color Bar"))),
                   fluidRow(column( 12, radioButtons(ns("colbarReverse4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot4$colbarReverse, inline = TRUE))),
                   fluidRow(column( 12, h5("Automatic Sorting"))),
                   fluidRow(column( 12, radioButtons(ns("autoSort4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot4$autoSort, inline = TRUE))),
                   fluidRow(column( 12, h5("Label Angle"))),
                   fluidRow(column( 12, numericInput(ns("labelAngle4"), NULL, value = settingPlot4$labelAngle))),

          ),
          tabPanel("Heatmap",
                   fluidRow(column( 12, h5("Select Features"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFeaturesHM4"), label = NULL,
                                                       choices = Task_properties$featureImputed,
                                                       multiple = TRUE,
                                                       selected = settingPlot4$featuresHM,
                                                       options = list( maxItems = 2)))),
                   fluidRow(column( 12, h5("Choose Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("plotFunction4"), label = NULL,
                                                       choices = c("mean","sd"), selected = settingPlot4$plotFunction))),
                   fluidRow(column( 12, h5("Gridsize"))),
                   fluidRow(column( 12, numericInput(ns("gridsizeHM4"), NULL, value = settingPlot4$gridsize))),
                   fluidRow(column( 12, h5("Show Rug"))),
                   fluidRow(column( 12, radioButtons(ns("rugHM4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot4$rug, inline = TRUE))),
                   fluidRow(column( 12, h5("Show Plotpoints"))),
                   fluidRow(column( 12, radioButtons(ns("plotPoints4"), label = NULL,
                                                     choices = list("Yes" = 1, "No" = 2),
                                                     selected = settingPlot4$plotPoints, inline = TRUE)))

          ),
          tabPanel("Importance Plot",
                   fluidRow(column( 12, h5("Select Loss Function"))),
                   fluidRow(column( 12, selectizeInput(inputId = ns("lossFunction4"), label = NULL,
                                                       choices = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
                                                                   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle",
                                                                   "rse", "rrse", "smape"), selected = settingPlot4$lossFunction))),
          ), selected = settingPlot4$plotName
        )
      })

      # UI-Output: choose Plots section
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


      # UI-Output: TabsetPanel
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



      # UI-Output: Plot 1 rendering
      plot1 <- reactive({

        if(is.null(input$numberPlots))
          return()
        else if(min(input$numberPlots) == 1)
          if(is.ggplot(finalPlots$plot1))
            renderPlot(finalPlots$plot1)
          else
            renderPlotly(finalPlots$plot1)
        else if(min(input$numberPlots) == 2)
          if(is.ggplot(finalPlots$plot2))
            renderPlot(finalPlots$plot2)
          else
            renderPlotly(finalPlots$plot2)
        else if(min(input$numberPlots) == 3)
          if(is.ggplot(finalPlots$plot3))
            renderPlot(finalPlots$plot3)
          else
            renderPlotly(finalPlots$plot3)
        else
          if(is.ggplot(finalPlots$plot4))
            renderPlot(finalPlots$plot4)
          else
            renderPlotly(finalPlots$plot4)

      })

      # UI-Output: Plot 2 rendering
      plot2 <- reactive({

        if(is.null(input$numberPlots))
          return()

        plot1 <- min(input$numberPlots)
        if(2 %in% input$numberPlots & 2 > plot1)
          if(is.ggplot(finalPlots$plot2))
            renderPlot(finalPlots$plot2)
          else
            renderPlotly(finalPlots$plot2)
        else if(3 %in% input$numberPlots & 3 > plot1)
          if(is.ggplot(finalPlots$plot3))
            renderPlot(finalPlots$plot3)
          else
            renderPlotly(finalPlots$plot3)
        else
          if(is.ggplot(finalPlots$plot4))
            renderPlot(finalPlots$plot4)
        else
            renderPlotly(finalPlots$plot4)
      })

      # UI-Output: Plot 3 rendering
      plot3 <- reactive({

        if(is.null(input$numberPlots))
          return()

        plot1 <- min(input$numberPlots)
        plot2 <- input$numberPlots[2]
        if(3 %in% input$numberPlots & 3 > plot2)
          if(is.ggplot(finalPlots$plot3))
            renderPlot(finalPlots$plot3)
          else
            renderPlotly(finalPlots$plot3)
        else
          if(is.ggplot(finalPlots$plot4))
            renderPlot(finalPlots$plot4)
          else
            renderPlotly(finalPlots$plot4)
      })

      # UI-Output: Plot 4 rendering
      plot4 <- reactive({
        if(is.ggplot(finalPlots$plot4))
          renderPlot(finalPlots$plot4)
        else
          renderPlotly(finalPlots$plot4)
      })



      # UI-Output: Plot the output together!
      output$plots <- renderUI({

        req(!is.null(Task_properties$task))
        req(input$FunctionChoice)
        req(counter$plotNumber)

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
        data$subsetData <- as.data.frame(data$subsetData)
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

      #create names for the tab panels
      observeEvent(input$selectPlot, {
        counter$plotType <- input$selectPlot
      })
      observeEvent(input$selectPlot2, {
        counter$plotType2 <- input$selectPlot2
      })
      observeEvent(input$selectPlot3, {
        counter$plotType3 <- input$selectPlot3
      })
      observeEvent(input$selectPlot4, {
        counter$plotType4 <- input$selectPlot4
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


      # number of plots for the output
      observeEvent(input$numberPlots, {
        if(is.null(input$numberPlots))
          counter$plotNumber <- 0
        else
        counter$plotNumber <- length(input$numberPlots)

      })



      # observeEvent startButton to recalculate the plots
      observeEvent(input$startButton, {

      req(Task_properties$plotRdy == TRUE)

      if(is.null(input$numberPlots))

      shinyalert(title = "No Plot Selected!", text = userhelp[["No Plot Selected!"]], closeOnClickOutside = TRUE, animation = FALSE)

      withProgress(message = 'Making Plots:', value = 0.0, {

      if(1 %in% input$numberPlots){

        # Increase the progress bar, and update the detail text.
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
        title <- ifelse(abs(input$labelAngle) > 10, FALSE, TRUE)

        #Heatmap
        plotPoints <- ifelse(input$plotPoints == 1, TRUE, FALSE)
        rugHM <- ifelse(input$rugHM == 1, TRUE, FALSE)

        if(input$FunctionChoice == "PDP")
          finalPlots$plot1 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP, learner = learner$model, gridsize = input$gridsizePDP, rug = rugPDP, plotICE = plotIce)
        else if(input$FunctionChoice == "PCP")
          finalPlots$plot1 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP, autosort = autoSort, labelside = input$labelSide, labelangle = input$labelAngle,
                                                     constrainrange = input$constrainRange, labeltarget = labelTarget,  colbarreverse = colbarReverse, title = title, titleheight = 0.95)
        else if(input$FunctionChoice == "Heatmap")
          if(length(input$plotFeaturesHM) != 2) {
            shinyalert(title = "please select two parameters!", text = userhelp[["Not enough Parameter"]], closeOnClickOutside = TRUE, animation = FALSE)
            finalPlots$plot1 <- NULL }
          else if(input$plotFunction == "mean")
            finalPlots$plot1 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM, fun = mean, gridsize = input$gridsizeHM, scatterplot = plotPoints, rug = rugHM)
          else
            finalPlots$plot1 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM, fun = sd, gridsize = input$gridsizeHM, scatterplot = plotPoints, rug = rugHM)
        else if(input$FunctionChoice == "Importance Plot")

          finalPlots$plot1 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction)
      }


        if(2 %in% input$numberPlots){

        # Increase the progress bar, and update the detail text.
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
        title2 <- ifelse(abs(input$labelAngle2) > 10, FALSE, TRUE)

        #Heatmap
        plotPoints2 <- ifelse(input$plotPoints2 == 1, TRUE, FALSE)
        rugHM2 <- ifelse(input$rugHM2 == 1, TRUE, FALSE)


        if(input$FunctionChoice2 == "PDP")
          finalPlots$plot2 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP2, learner = learner$model, gridsize = input$gridsizePDP2, rug = rugPDP2, plotICE = plotIce2)
        else if(input$FunctionChoice2 == "PCP")
          finalPlots$plot2 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP2, autosort = autoSort2, labelside = input$labelSide2, labelangle = input$labelAngle2,
                                                     constrainrange = input$constrainRange2, labeltarget = labelTarget2,  colbarreverse = colbarReverse2, title = title2, titleheight = 0.95)
        else if(input$FunctionChoice2 == "Heatmap")
          if(length(input$plotFeaturesHM2) != 2) {
            shinyalert(title = "please select two parameters!", text = userhelp[["Not enough Parameter"]], closeOnClickOutside = TRUE, animation = FALSE)
            finalPlots$plot2 <- NULL }
          else if(input$plotFunction2 == "mean")
            finalPlots$plot2 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM2, fun = mean, gridsize = input$gridsizeHM2, scatterplot = plotPoints2, rug = rugHM2)
          else
            finalPlots$plot2 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM2, fun = sd, gridsize = input$gridsizeHM2, scatterplot = plotPoints2, rug = rugHM2)
        else if(input$FunctionChoice2 == "Importance Plot")

          finalPlots$plot2 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction2)
        }

        if(3 %in% input$numberPlots){

        # Increase the progress bar, and update the detail text.
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
        title3 <- ifelse(abs(input$labelAngle3) > 10, FALSE, TRUE)

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
                                                     constrainrange = input$constrainRange3, labeltarget = labelTarget3,  colbarreverse = colbarReverse3, title = title3, titleheight = 0.95)
        else if(input$FunctionChoice3 == "Heatmap")
          if(length(input$plotFeaturesHM3) != 2) {
            shinyalert(title = "please select two parameters!", text = userhelp[["Not enough Parameter"]], closeOnClickOutside = TRUE, animation = FALSE)
            finalPlots$plot3 <- NULL }
          else if(input$plotFunction3 == "mean")
              finalPlots$plot3 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM3, fun = mean, gridsize = input$gridsizeHM3, scatterplot = plotPoints3, rug = rugHM3)
          else
            finalPlots$plot3 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM3, fun = sd, gridsize = input$gridsizeHM3, scatterplot = plotPoints3, rug = rugHM3)
        else if(input$FunctionChoice3 == "Importance Plot")
          finalPlots$plot3 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction3)
        }

        if(4 %in% input$numberPlots){

        # Increase the progress bar, and update the detail text.
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
        title4 <- ifelse(abs(input$labelAngle4) > 10, FALSE, TRUE)

        #Heatmap
        plotPoints4 <- ifelse(input$plotPoints4 == 1, TRUE, FALSE)
        rugHM4 <- ifelse(input$rugHM4 == 1, TRUE, FALSE)

        if(input$FunctionChoice4 == "PDP")
          finalPlots$plot4 <- plotPartialDependence(task = Task_properties$task, features = input$plotFeaturesPDP4, learner = learner$model, gridsize = input$gridsizePDP4, rug = rugPDP4, plotICE = plotIce4)

        else if(input$FunctionChoice4 == "PCP")
          finalPlots$plot4 <- plotParallelCoordinate(task = Task_properties$task, features = input$plotFeaturesPCP4, autosort = autoSort4, labelside = input$labelSide4, labelangle = input$labelAngle4,
                                                     constrainrange = input$constrainRange4, labeltarget = labelTarget4,  colbarreverse = colbarReverse4, title = title4, titleheight = 0.95)
        else if(input$FunctionChoice4 == "Heatmap")
          if(length(input$plotFeaturesHM4) != 2) {
            shinyalert(title = "please select two parameters!", text = userhelp[["Not enough Parameter"]], closeOnClickOutside = TRUE, animation = FALSE)
            finalPlots$plot4 <- NULL }
          else if(input$plotFunction4 == "mean")
              finalPlots$plot4 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM4, fun = mean, gridsize = input$gridsizeHM4, scatterplot = plotPoints4, rug = rugHM4)
          else
            finalPlots$plot4 <- plotHeatmap(task = Task_properties$task, features = input$plotFeaturesHM4, fun = sd, gridsize = input$gridsizeHM4, scatterplot = plotPoints4, rug = rugHM4)

        else if(input$FunctionChoice4 == "Importance Plot")
          finalPlots$plot4 <- plotImportance(task = Task_properties$task, learner = learner$model, loss = input$lossFunction4)
        }

        incProgress( detail = "Rendering Plots", amount = 0.2)

      })
      })

      # set learner
      observe({

      req(!is.null(Task_properties$task))

      if(Task_properties$task$task_type == "regr") {
      learner$model = lrn("regr.ranger")
      } else {
      learner$model = lrn("classif.ranger")
      }

      })


      # when the data is changed, the reactive values of the current task are reset
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

      # reset plots if the original data change
      observeEvent({data$originalData},{

        #reset plot 1 Initialize values
        counter$plotType <- "Importance Plot"
        updateSelectizeInput(session,"plotFeaturesHM", selected = Task_properties$featureImputed[c(1,2)])
        updateSelectizeInput(session,"plotFeaturesPCP", selected = Task_properties$featureImputed)
        updateSelectizeInput(session,"plotFeaturesPDP", selected = Task_properties$featureImputed[c(1)])
        updateNumericInput(session,"gridsizePDP", value = 15)
        updateNumericInput(session,"gridsizeHM", value = 15)
        updateRadioButtons(session,"rugHM", selected = 2)
        updateRadioButtons(session,"rugPDP", selected = 2)
        updateRadioButtons(session,"plotIce", selected = 2)
        updateSliderInput(session,"constrainRange", value = c(0,1))
        updateRadioButtons(session,"labelSide", selected = "Top")
        updateRadioButtons(session,"labelTarget", selected = 1)
        updateRadioButtons(session,"colbarReverse", selected = 2)
        updateRadioButtons(session,"autoSort", selected = 2)
        updateNumericInput(session,"labelAngle", value = 0)
        updateSelectizeInput(session,"plotFunction", selected = "mean")
        updateRadioButtons(session,"plotPoints", selected = 2)
        updateSelectizeInput(session, "lossFunction", selected = "mae")

        #reset plot 2 Initialize values
        counter$plotType2 <- "PDP"
        updateSelectizeInput(session,"plotFeaturesHM2", selected = Task_properties$featureImputed[c(1,2)])
        updateSelectizeInput(session,"plotFeaturesPCP2", selected = Task_properties$featureImputed)
        updateSelectizeInput(session,"plotFeaturesPDP2", selected = Task_properties$featureImputed[c(1)])
        updateNumericInput(session,"gridsizePDP2", value = 15)
        updateNumericInput(session,"gridsizeHM2", value = 15)
        updateRadioButtons(session,"rugHM2", selected = 2)
        updateRadioButtons(session,"rugPDP2", selected = 2)
        updateRadioButtons(session,"plotIce2", selected = 2)
        updateSliderInput(session,"constrainRange2", value = c(0,1))
        updateRadioButtons(session,"labelSide2", selected = "Top")
        updateRadioButtons(session,"labelTarget2", selected = 1)
        updateRadioButtons(session,"colbarReverse2", selected = 2)
        updateRadioButtons(session,"autoSort2", selected = 2)
        updateNumericInput(session,"labelAngle2", value = 0)
        updateSelectizeInput(session,"plotFunction2", selected = "mean")
        updateRadioButtons(session,"plotPoints2", selected = 2)
        updateSelectizeInput(session, "lossFunction2", selected = "mae")

        #reset plot 3 Initialize values
        counter$plotType3 <- "PCP"
        updateSelectizeInput(session,"plotFeaturesHM3", selected = Task_properties$featureImputed[c(1,2)])
        updateSelectizeInput(session,"plotFeaturesPCP3", selected = Task_properties$featureImputed)
        updateSelectizeInput(session,"plotFeaturesPDP3", selected = Task_properties$featureImputed[c(1)])
        updateNumericInput(session,"gridsizePDP3", value = 15)
        updateNumericInput(session,"gridsizeHM3", value = 15)
        updateRadioButtons(session,"rugHM3", selected = 2)
        updateRadioButtons(session,"rugPDP3", selected = 2)
        updateRadioButtons(session,"plotIce3", selected = 2)
        updateSliderInput(session,"constrainRange3", value = c(0,1))
        updateRadioButtons(session,"labelSide3", selected = "Top")
        updateRadioButtons(session,"labelTarget3", selected = 1)
        updateRadioButtons(session,"colbarReverse3", selected = 2)
        updateRadioButtons(session,"autoSort3", selected = 2)
        updateNumericInput(session,"labelAngle3", value = 0)
        updateSelectizeInput(session,"plotFunction3", selected = "mean")
        updateRadioButtons(session,"plotPoints3", selected = 2)
        updateSelectizeInput(session, "lossFunction3", selected = "mae")

        #reset plot 4 Initialize values
        counter$plotType4 <- "Heatmap"
        updateSelectizeInput(session,"plotFeaturesHM4", selected = Task_properties$featureImputed[c(1,2)])
        updateSelectizeInput(session,"plotFeaturesPCP4", selected = Task_properties$featureImputed)
        updateSelectizeInput(session,"plotFeaturesPDP4", selected = Task_properties$featureImputed[c(1)])
        updateNumericInput(session,"gridsizePDP4", value = 15)
        updateNumericInput(session,"gridsizeHM4", value = 15)
        updateRadioButtons(session,"rugHM4", selected = 2)
        updateRadioButtons(session,"rugPDP4", selected = 2)
        updateRadioButtons(session,"plotIce4", selected = 2)
        updateSliderInput(session,"constrainRange4", value = c(0,1))
        updateRadioButtons(session,"labelSide4", selected = "Top")
        updateRadioButtons(session,"labelTarget4", selected = 1)
        updateRadioButtons(session,"colbarReverse4", selected = 2)
        updateRadioButtons(session,"autoSort4", selected = 2)
        updateNumericInput(session,"labelAngle4", value = 0)
        updateSelectizeInput(session,"plotFunction4", selected = "mean")
        updateRadioButtons(session,"plotPoints4", selected = 2)
        updateSelectizeInput(session, "lossFunction4", selected = "mae")

          finalPlots$plot1 <- NULL
          finalPlots$plot2 <- NULL
          finalPlots$plot3 <- NULL
          finalPlots$plot4 <- NULL
          plot1()
          plot2()
          plot3()
          plot4()
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

      # always give feedback when the user changed something in plot 1
      observeEvent({
        input$selectPlot
        input$plotFeaturesPDP
        input$gridsizePDP
        input$rugPDP
        input$plotFeaturesPCP
        input$plotFeaturesHM
        input$gridsizeHM
        input$rugHM
        input$plotIce
        input$constrainRange
        input$labelSide
        input$labelTarget
        input$colbarReverse
        input$autoSort
        input$labelAngle
        input$plotFunction
        input$plotPoints
        input$lossFunction}, {
          counter$nameIndicator <- "Plot 1"
        })

      # always give feedback when the user changed something in plot 2
      observeEvent({
        input$selectPlot2
        input$plotFeaturesPDP2
        input$gridsizePDP2
        input$rugPDP2
        input$plotFeaturesPCP2
        input$plotFeaturesHM2
        input$gridsizeHM2
        input$rugHM2
        input$plotIce2
        input$constrainRange2
        input$labelSide2
        input$labelTarget2
        input$colbarReverse2
        input$autoSort2
        input$labelAngle2
        input$plotFunction2
        input$plotPoints2
        input$lossFunction2}, {
          counter$nameIndicator <- "Plot 2"
        })

      # always give feedback when the user changed something in plot 3
      observeEvent({
        input$selectPlot3
        input$plotFeaturesPDP3
        input$gridsizePDP3
        input$rugPDP3
        input$plotFeaturesPCP3
        input$plotFeaturesHM3
        input$gridsizeHM3
        input$rugHM3
        input$plotIce3
        input$constrainRange3
        input$labelSide3
        input$labelTarget3
        input$colbarReverse3
        input$autoSort3
        input$labelAngle3
        input$plotFunction3
        input$plotPoints3
        input$lossFunction3}, {
          counter$nameIndicator <- "Plot 3"
        })

      # always give feedback when the user changed something in plot 4
      observeEvent({
        input$selectPlot4
        input$plotFeaturesPDP4
        input$gridsizePDP4
        input$rugPDP4
        input$plotFeaturesPCP4
        input$plotFeaturesHM4
        input$gridsizeHM4
        input$rugHM4
        input$plotIce4
        input$constrainRange4
        input$labelSide4
        input$labelTarget4
        input$colbarReverse4
        input$autoSort4
        input$labelAngle4
        input$plotFunction4
        input$plotPoints4
        input$lossFunction4}, {
          counter$nameIndicator <- "Plot 4"
        })

    }
  )
}



