#' logloss Performance for two hyperparameter.
#'
#' A dataset containing the `logloss` performance measure and the two hyperparameters `s` and `alpha`.
#' The dataset is from the [glmnet]{https://github.com/cran/glmnet} package.
#'
#' @format \code{glmnet_ela} is a data frame with 49 rows and 3 variables:
#' \describe{
#' \item{\code{logloss}}{performance meassure}
#' \item{\code{alpha}}{hyperparameter}
#' \item{\code{s}}{hyperparameter}}

"glmnet_ela"

#' logloss Performance for two hyperparameters.
#'
#' A dataset containing the `yval` performance measure and 11 hyperparameters.
#' The dataset is from Moosbauer, Julia, et al. "Automated Benchmark-Driven Design and Explanation of Hyperparameter Optimizers."
#' arXiv preprint arXiv:2111.14756 (2021).
#'
#' @format \code{smashy_super} is a data frame with 200 rows and 12 variables:
#' \describe{
#' \item{\code{yval}}{performance meassure}}

"smashy_super"
