#' @title Importance Plot
#'
#' @description Visualization of an importance plot from a [mlr3::Task] object and a [mlr3::Learner].
#'
#' @param task [mlr3::Task] object \cr
#'   A task for which the plot should be generated.
#' @param learner [mlr3::Learner] object \cr
#'   A learner for which the plot should be generated.
#'   If NULL, a random forest for regression or classification will be used. The default is "regr.ranger" for regression
#'   tasks and "classif.ranger" for classification tasks.
#' @param loss (`character(1)` \cr
#'   The loss function to be used. It can be chosen between "ce", "f1", "mae", "mse", "rmse", "mape",
#'   "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape". The default is "mae" for a Regression
#'   task. For for a classification task only "ce" is available.
#' @param title (`logical`) \cr
#'   If TRUE, the title will be plotted. The default is TRUE.
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

  # take a random forest if no learner is specified
  if (is.null(learner)) {
    if (task$task_type == "regr")
      learner = lrn("regr.ranger", num.trees = 100)
    else
      learner = lrn("classif.ranger", num.trees = 100, predict_type = "prob")
  }

  # in a classification task only the "ce" loss is available
  if (task$task_type != "regr")
    loss <- "ce"

  # input check
  assert_task(task)
  assert_learner(learner)
  assert_choice(loss, c("ce", "f1", "mae", "mse", "rmse", "mape", "mdae", "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape"))
  assert_logical(title)

  # make the task more robust
  po = po("imputehist") %>>% po("imputeoor") %>>% po("fixfactors", droplevels = FALSE) %>>%
    po("removeconstants") %>>% po("imputesample", affect_columns = selector_type("logical"))
  task = po$train(task)[[1]]

  # train the learner on a set of observations of the provided task, i.e. fit the selected model
  learner <- learner
  learner$train(task)
  targetName <- task$target_names
  df <- task$data()
  df <- as.data.frame(df)
  targetVector <-  df[[targetName]]
  index <- which(names(df) == targetName)

  n <- length(df)
  for (i in 1:n) {
    if(index != i) {
      if(is.logical(df[,i]))
        df[,i] <- as.factor(df[,i])
      if(is.character(df[,i]))
        df[,i] <- as.factor(df[,i])
    }
  }

  # for the iml importance plot the learner and data need to be wrapped in a predictor object
  model <- iml::Predictor$new(learner, data = df, y = targetVector)

  # calculate the importance of every feature and store the results
  importancePlot <- iml::FeatureImp$new(model, loss = loss)

  # prepare the ggplot2 object
  importancePlot <- plot(importancePlot)

  # create a title
  if (title == TRUE)
    importancePlot <- importancePlot +
    labs(title = "Importance Plot")

  # replace the name of the x-axis
  importancePlot$scales$scales[[1]]$name <- (sprintf("Parameter importance (loss: %s)", loss))

  # create a plotly object
    importancePlot <- ggplotly(importancePlot)

  importancePlot
}
