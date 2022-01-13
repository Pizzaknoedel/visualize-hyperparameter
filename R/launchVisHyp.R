#' Title Launch VisHyp
#'
#' @description
#' Launch the shiny application for the VisHyp package.
#'
#' @return None
#' @examples
#' if (interactive()) {
#'    launchVisHyp()
#' }
#'
#' @export

launchVisHyp <- function() {
  appDir <- system.file("VisHypShiny", package = "VisHyp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `VisHyp`.", call. = FALSE)
  }
  else {
    shiny::runApp(appDir, display.mode = 'normal')
  }

}
