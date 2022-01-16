#' @title Heatmap
#' @description Visualize a heatmap from a [mlr3::Task] object.
#'
#' @param task ([mlr3::Task] object) \cr
#'   A task for which the plot should be generated.
#' @param features (`character(2)`) \cr
#'   Names of two columns to plot. If NULL, then the first two feature columns will be plotted. Default is NULL.
#' @param fun (`function`) \cr
#'   A summary function used in the rectangles of the grid. The default summary function is [mean].
#' @param gridsize (`numeric(1)`) \cr
#'   The number of rectangles per axis. Default value is 20.
#' @param scatterplot (`logical`) \cr
#'   If TRUE, then a scatterplot will be plotted. Default is FALSE.
#' @param rug (`logical`) \cr
#'   If TRUE, then a rug will be plotted. Default is TRUE.
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


plotHeatmap <- function(task, features = NULL, fun = mean, gridsize = 20, scatterplot = FALSE, rug = TRUE) {

  # take the first two features if no features are selected yet
  if(is.null(features))
  features <- task$feature_names[c(1,2)]

  # check input
  assertVector(features, len = 2)
  assert_choice(features[1], task$feature_names)
  assert_choice(features[2], task$feature_names)
  assert_task(task)
  assert_numeric(gridsize, len = 1, lower = 1)
  assert_function(fun)
  assert_logical(scatterplot)
  assert_logical(rug)

  # make the task more robust
  po = po("imputehist") %>>% po("fixfactors") %>>% po("imputeoor")  %>>% po("removeconstants")
  task = po$train(task)[[1]]

  # receive data and target name from the task
  data <- task$data()
  targetName <- task$target_names

  legend <- paste0(targetName, " (", as.character(substitute(fun)), ")")

  # create a heatmap with ggplot2
  plotData <-  ggplot(data, aes_string(x = features[1], y = features[2], z = targetName)) +
      geom_tile(stat = "summary_2d", fun = fun, bins = gridsize) +
      labs(title = "Heatmap", fill = legend)

  if (rug == TRUE)
      plotData <- plotData +
      geom_rug(alpha = 0.2, sides = "bl")

  if (scatterplot == TRUE)
      plotData  <- plotData +
      geom_point()

  # create a plotly object
  heatmap <- ggplotly(plotData)

  heatmap

}
