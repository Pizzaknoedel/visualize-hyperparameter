#' @title Heatmap
#' @description Visualization of a heatmap from a [mlr3::Task] object.
#'
#' @param task ([mlr3::Task] object) \cr
#'   A task for which the plot should be generated.
#' @param features (`character(2)`) \cr
#'   The names of the two columns to be plotted. If NULL, the first two feature columns will be plotted. The default is NULL.
#' @param fun (`function`) \cr
#'   A summary function used in the rectangles of the grid. The default is [mean] for a regression task and
#'   majority vote for a classification task.
#' @param gridsize (`numeric(1)`) \cr
#'   The number of rectangles per axis. The default value is 20.
#' @param scatterplot (`logical`) \cr
#'   If TRUE, a scatterplot will be plotted. The default is FALSE.
#' @param rug (`logical`) \cr
#'   If TRUE, a rug will be plotted. The default is TRUE.
#' @param title (`logical`) \cr
#'   If TRUE, the title will be plotted. The default is TRUE.
#' @param numericNA (`character(1)` \cr
#'   If "Max", the NA values of the column are displayed as maximum value. If "Min", the NA values are displayed as
#'   minimum value. The default is "Max".
#'
#' @return A [plotly] object.
#'
#' @seealso [plotParallelCoordinate] [plotPartialDependence] [plotImportance]
#'
#' @examples
#' library(mlr3)
#' data(glmnet_ela)
#' task_glmnet_ela = TaskRegr$new(id = "task_glmnet", backend = glmnet_ela, target = "logloss")
#' plotHeatmap(task_glmnet_ela)
#'
#' @export


plotHeatmap <- function(task, features = NULL, fun = NULL, gridsize = 20, scatterplot = FALSE, rug = TRUE, title = TRUE, numericNA = "Max") {

  # take the first two features if no features are selected yet
  if(is.null(features))
  features <- task$feature_names[c(1,2)]

  if(is.null(fun)) {

    if (task$task_type == "regr") {
      # mean
      fun <- mean
    } else {
      # majority vote
      fun <- function(majority) {
      names(which.max(table(majority)))}
    }
  }

  # check input
  assertVector(features, len = 2, unique = TRUE)
  assert_choice(features[1], task$feature_names)
  assert_choice(features[2], task$feature_names)
  assert_task(task)
  assert_numeric(gridsize, len = 1, lower = 1)
  assert_function(fun)
  assert_choice(numericNA, c("Max","Min", "Remove"))
  assert_logical(scatterplot)
  assert_logical(rug)
  assert_logical(title)

  # make the task more robust
  po =  po("fixfactors") %>>% po("removeconstants")
  task = po$train(task)[[1]]

  # receive data and target name from the task
  data <- task$data()
  df <- as.data.frame(data)

  # show NA's as an own factor level (if class = factor) or as maximum (if class = numeric)
  n <- length(df)
  for (i in 1:n) {

    if (is.logical(df[,i]))
      df[,i] <- as.factor(df[,i])
    if (is.character(df[,i]))
      df[,i] <- as.factor(df[,i])

    if (is.factor(df[,i])) {
      if (sum(is.na(df[,i])) > 0) {
        levels(df[,i]) <- c(levels(df[,i]),"NA")
        df[,i][is.na(df[,i])] <- "NA"
      }
    }
    else if (is.numeric(df[,i])) {
      if (numericNA == "Max")
        df[,i][is.na(df[,i])] <- max(df[,i], na.rm = TRUE)
      else if (numericNA == "Min")
        df[,i][is.na(df[,i])] <- min(df[,i], na.rm = TRUE)
    }
  }

  targetName <- task$target_names

  legend <- paste0(targetName)

  # create a heatmap with ggplot2
  plotData <-  ggplot(df, aes_string(x = features[1], y = features[2], z = targetName)) +
      geom_tile(stat = "summary_2d", fun = fun, bins = gridsize) +
      labs(fill = legend)

  # create a title
  if (title == TRUE)
  plotData <- plotData +
      labs(title = "Heatmap")

  # create a rug
  if (rug == TRUE)
      plotData <- plotData +
      geom_rug(alpha = 0.2, sides = "bl")

  # show a scatterplot
  if (scatterplot == TRUE)
      plotData  <- plotData +
      geom_point()

  # create a plotly object
  if (task$task_type == "regr")
  plotData <- ggplotly(plotData)

  plotData

}
