#' @title Importance Plot
#'
#' @description Visualize an Importance Plot from a [mlr3::Task] object and a [mlr3::Learner]
#'
#' @param task [mlr3::Task] object \cr
#'   A task for which the plot should be generated.
#' @param learner [mlr3::Learner] object \cr
#'   A Learner for which the plot should be generated.
#'   If NUll, then a Random Forest for regression or classification will be used. Default is NULL.
#' @param loss (`character(1)` \cr
#'   The loss function to be used. It can be chosen between "ce", "f1", "logLoss", "mae", "mse", "rmse", "mape",
#'   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape". Default is "mae".
#' @param title (`logical`) \cr
#'   If TRUE, then a title will be plotted. Default is TRUE.
#'
#' @return A [plotly] object.
#'
#' @seealso [plotParallelCoordinate] [plotHeatmap] [plotPartialDependence]
#'
#' @examples
#' library(mlr3)
#' data(glmnet_ela)
#' task_glmnet_ela = TaskRegr$new(id = "task_glmnet", backend = glmnet_ela, target = "logloss")
#' plotImportance(task_glmnet_ela)
#'
#' @export


plotImportance <- function(task, learner = NULL, loss = "mae", title = TRUE) {

  # take a Random Forest if no Learner is specified
  if (is.null(learner)) {
    if (task$task_type == "regr") {
      learner = lrn("regr.ranger", num.trees = 100)
    } else if (task$task_type == "classif") {
      learner = lrn("classif.ranger")
    }
  }

  # input check
  assert_task(task)
  assert_learner(learner)
  assert_choice(loss, c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape"))

  # make the task more robust
  po = po("imputehist") %>>% po("imputeoor") %>>% po("fixfactors", droplevels = FALSE) %>>%
    po("removeconstants") %>>% po("imputesample", affect_columns = selector_type("logical"))
  task = po$train(task)[[1]]

  # Train the learner on a set of observations of the provided task i.e. fit the selected model
  learner <- learner
  learner$train(task)
  targetName <- task$target_names
  df <- task$data()
  targetVector <-  df[[targetName]]
  df <- df[which(names(df) != targetName)]

  n <- length(df)
  for (i in 1:n) {
    if(is.logical(df[,i]))
      df[,i]=as.factor(df[,i])
    if(is.character(df[,i]))
      df[,i] <- as.factor(df[,i])
  }

  # for the iml importance plot the leaner and data need to be wrapped in a Predictor object
  model <- iml::Predictor$new(learner, data = df, y = targetVector)

  # calculate the importance of every feature and store the results
  importancePlot <- iml::FeatureImp$new(model, loss = loss)

  # prepare the ggplot2 object
  importancePlot <- plot(importancePlot)

  # create a title
  if (title == TRUE)
    importancePlot <- importancePlot +
    labs(title = "Importance Plot")

  #replace name x-axis
  importancePlot$scales$scales[[1]]$name <- (sprintf("Parameter Importance (loss: %s)", loss))

  # create a plotly object
    importancePlot <- ggplotly(importancePlot)

  importancePlot
}
