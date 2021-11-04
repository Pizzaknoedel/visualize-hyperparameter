#' @title partial Dependence plot (PDP)
#'
#' Calculate the partial Dependence for 1 or 2 Hyperparameter.
#'
#' @param task task object
#' @param learner learner object
#' @param features the name of the Hyperparmaeter for which the partial dependence should be calculated
#' @param gridsize only important for 2 Hyperparameter. default is 20
#' @param rug logicaL: should a rug be plotted? default is FALSE
#'
#' @return ggplot object
#'
#'
#' @export


plotPartialDependence <- function(task, learner, features, gridsize = 20, rug = TRUE) {
  mlr3::assert_learner(learner)
  mlr3::assert_task(task)
  checkmate::assert_numeric(gridsize, min.len = 1)
  checkmate::assert_character(features)
  checkmate::assert_logical(rug)

  learner <- learner
  #Train the learner on a set of observations of the provided task i.e. fit the selected model
  learner$train(task)
  #retrieve the df stored inside the task
  df <- task$data()
  #for the plot leaner and data needs to be wrapped in a Predictor object
  model <- iml::Predictor$new(learner, data = df)

  if(length(features) == 1)
    #calculate the partial feature effect for 1 Hyperparameter and store the results
    pdp <- iml::FeatureEffect$new(model, feature = features, method = "pdp", grid.size = gridsize)

  if(length(features) == 2)
    #calculate the partial features effect for 2 Hyperparameter and store the results
    pdp <- iml::FeatureEffect$new(model, feature = features, method = "pdp", grid.size = gridsize)

  #plot the results with ggplot2
  plot(pdp, rug = rug) +
    # Adds a title
    ggplot2::ggtitle("Partial dependence")


}
