#' @title Partial Dependence Plot (PDP)
#'
#' @description Visualize a Partial Dependence Plot from a [mlr3::Task] object and a [mlr3::Learner]
#'
#' @param task [mlr3::Task] object \cr
#'   A task for which the plot should be generated.
#' @param features (`character(1)` | `character(2)`) \cr
#'   The name of the features for which the partial dependence should be calculated.
#'   If no features are specified, the first 2 features of the Task object will be plotted. Default is NULL.
#' @param learner [mlr3::Learner] object \cr
#'   A Learner for which the plot should be generated.
#'   If NUll, then a Random Forest for regression or classification will be used. Default is NULL.
#' @param gridsize (`numeric(1)`) \cr
#'   The number of rectangles/ sections per axis. Default is 20.
#' @param rug (`logical`) \cr
#'   If TRUE, then a rug will be plotted. Default is TRUE.
#' @param plotICE (`logical`) \cr
#'   If TRUE, then the individual conditional expectations (ICE) will be also plotted. Only available for one feature.
#'   Default is TRUE.
#' @param title (`logical`) \cr
#'   If TRUE, then a title will be plotted. Default is TRUE.
#'
#' @return A [plotly] object.
#'
#' @seealso [plotParallelCoordinate] [plotHeatmap] [plotImportance]
#'
#' @examples
#' library(mlr3)
#' data(glmnet_ela)
#' task_glmnet_ela = TaskRegr$new(id = "task_glmnet", backend = glmnet_ela, target = "logloss")
#' plotPartialDependence(task_glmnet_ela)
#'
#' @export


plotPartialDependence <- function(task, features = NULL, learner = NULL, gridsize = 20, rug = TRUE, plotICE = TRUE, title = TRUE) {
  # take the first feature if no features are specified
  if(is.null(features))
    features <- task$feature_names[c(1)]
  else
    features <- features

  # take a Random Forest if no Learner is specified
  if (is.null(learner)) {
    if (task$task_type == "regr") {
      learner = lrn("regr.ranger", num.trees = 100)
    } else if (task$task_type == "classif") {
      learner = lrn("classif.ranger")
    }
  }

  # input check
  assertVector(features, min.len = 1, max.len = 2)
  assert_choice(features[1], task$feature_names)
  if(length(features)==2)
  assert_choice(features[2], task$feature_names)
  assert_learner(learner)
  assert_task(task)
  assert_numeric(gridsize, len = 1, lower = 1)
  assert_logical(rug)
  assert_logical(plotICE)

  # make the task more robust
  po = po("imputehist") %>>%  po("imputeoor") %>>% po("fixfactors", droplevels = FALSE) %>>%
    po("removeconstants") %>>% po("imputesample", affect_columns = selector_type("logical"))
  task = po$train(task)[[1]]

  # get the data out of the task
  df <- task$data()
  df <- as.data.frame(df)

  # Train the learner on a set of observations of the provided task i.e. fit the selected model
  targetName <- task$target_names
  targetVector <-  df[[targetName]]
  index <- which(names(df) != targetName)
  df <- df[index]

  n <- length(df)
  for (i in 1:n) {
    if(is.logical(df[,i]))
      df[,i] <- as.factor(df[,i])
    if(is.character(df[,i]))
      df[,i] <- as.factor(df[,i])
  }

  # Train the learner on a set of observations of the provided task i.e. fit the selected model
  learner <- learner
  learner$train(task)

  # for the iml plot the leaner and data need to be wrapped in a Predictor object
  model <- iml::Predictor$new(learner, data = df, y = targetVector)

  # calculate the partial feature effect for 1 or 2 features and store the results
  if(length(features) == 1)
    pdp <- iml::FeatureEffect$new(model, feature = features, method = ifelse( plotICE == TRUE, "pdp+ice", "pdp"), grid.size = gridsize)

  else if(length(features) == 2)
    pdp <- iml::FeatureEffect$new(model, feature = features, method = "pdp", grid.size = gridsize)

  # prepare ggplot2 object
  pdp <- plot(pdp, rug = rug)

  # create a title
  if (title == TRUE)
    pdp <- pdp +
    ggtitle("Partial Dependence Plot")

  # create a plotly object
  if (length(features) == 2) {
    featureVector1 <- df[[features[1]]]
    featureVector2 <- df[[features[2]]]

    if(is.numeric(featureVector1) & is.numeric(featureVector2)) {
       pdp <- pdp + xlab(features[1])

    }
    else if(is.numeric(featureVector1) & is.factor(featureVector2) | is.factor(featureVector1) & is.numeric(featureVector2))
      return(pdp)
  }
  pdp <- ggplotly(pdp)
  pdp
}
