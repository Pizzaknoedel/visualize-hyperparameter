#' @title Partial Dependence Plot (PDP)
#'
#' @description Visualization of a partial dependence plot from a [mlr3::Task] object and a [mlr3::Learner].
#'
#' @param task [mlr3::Task] object \cr
#'   A task for which the plot should be generated.
#' @param features (`character(1)` | `character(2)`) \cr
#'   The name of the features for which the partial dependence should be calculated.
#'   If no features are specified, the first two features of the task object will be plotted. The default is NULL.
#' @param learner [mlr3::Learner] object \cr
#'   A learner for which the plot should be generated.
#'   If NULL, a random forest for regression or classification will be used. The default is NULL.
#' @param gridsize (`numeric(1)`) \cr
#'   The number of rectangles/sections per axis. The default is 20.
#' @param rug (`logical`) \cr
#'   If TRUE, a rug will be plotted. The default is TRUE.
#' @param plotICE (`logical`) \cr
#'   If TRUE, the individual conditional expectations (ICE) will be also plotted. It is only available if only one
#'   feature is chosen. The default is TRUE.
#' @param title (`logical`) \cr
#'   If TRUE, the title will be plotted. The default is TRUE.
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

  # take a random forest if no learner is specified
  if (is.null(learner)) {
    if (task$task_type == "regr") {
      learner = lrn("regr.ranger", num.trees = 100)
    } else if (task$task_type == "classif") {
      learner = lrn("classif.ranger", num.trees = 100, predict_type = "prob")
    }
  }

  # input check
  assertVector(features, min.len = 1, max.len = 2, unique = TRUE)
  assert_choice(features[1], task$feature_names)
  if(length(features)==2)
  assert_choice(features[2], task$feature_names)
  assert_learner(learner)
  assert_task(task)
  assert_numeric(gridsize, len = 1, lower = 1)
  assert_logical(rug)
  assert_logical(plotICE)
  assert_logical(title)

  # get the data out of the task
  data <- task$data()
  df <- as.data.frame(data)

  # create a new task with only factor and numeric values
  targetName <- task$target_names
  targetVector <-  df[[targetName]]
  index <- which(names(df) == targetName)

  n <- length(df)
  for (i in 1:n) {
    if(index != i) {
      if(is.logical(df[,i]))
        df[,i] <- as.factor(df[,i])
      if(is.character(df[,i]))
        df[,i] <- as.factor(df[,i])
      if(is.factor(df[,i]))
      {
        levels(df[,i]) <- c(levels(df[,i]),"NA")
        df[,i][is.na(df[,i])] <- "NA"
      }
    }
  }

  if (task$task_type == "regr")
  task = TaskRegr$new(id = "task", backend = df, target = targetName)
  else
  task = TaskClassif$new(id = "task", backend = df, target = targetName)

  # make the task more robust
  po = po("imputehist") %>>% po("imputeoor") %>>% po("fixfactors", droplevels = FALSE) %>>%
    po("removeconstants") %>>% po("imputesample", affect_columns = selector_type("logical"))
  task = po$train(task)[[1]]
  df <- task$data()

  # train the learner on a set of observations of the provided task i.e. fit the selected model
  learner <- learner
  learner$train(task)

  # for the iml plot the learner and data need to be wrapped in a predictor object
  model <- iml::Predictor$new(learner, data = df, y = targetVector)

  # calculate the partial feature effect for one or two features and store the results
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

  # change y-axis
  if (length(features) == 1)
    pdp$scales$scales[[1]]$name <- (paste0("predicted ", targetName))

  # create a plotly object
  if (length(features) == 2) {
    featureVector1 <- df[[features[1]]]
    featureVector2 <- df[[features[2]]]

    if(is.numeric(featureVector1) & is.numeric(featureVector2)) {
       pdp <- pdp + xlab(features[1])
    }
    else if(is.numeric(featureVector1) & is.factor(featureVector2) | is.factor(featureVector1) & is.numeric(featureVector2)) {
      pdp$scales$scales[[1]]$name <- (paste0("predicted ", targetName))
      return(pdp)
    }
  }
  pdp <- ggplotly(pdp)
  pdp
}
