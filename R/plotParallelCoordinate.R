#' @title Parallel Coordinate Plot (PCP)
#' @description Visualize a Parallel Coordinate Plot from a [mlr3::Task] object.
#'
#' @param task ([mlr3::Task] object) \cr
#'   A task for which the plot should be generated.
#' @param features (`character`) \cr
#'   Names of columns to plot. If NULL, then each column will be plotted. Default is NULL.
#' @param labelside (`character(1)` \cr
#'   The side of the labels for the scales. It can be chosen between "Top" and "Bottom". Default is "Top".
#' @param labelangle (`numeric(1)`) \cr
#'   The angle for the labels. Default is 0.
#' @param colbarrange (`numeric(2)`) \cr
#'   A vector with two numeric values. Its distance specifies the range of the color bar for the labeled target.
#'   If NULL, then the color bar between minimum and maximum of the target variable is displayed. Default is NULL.
#' @param constrainrange (`numeric(2)`) \cr
#'   A vector with two numeric values between 0 and 1. The vector restrict each column to the values of a range
#'   for which they are within the quantile of the target variable. If NULL, then no restriction is plotted. Default is NULL.
#' @param labeltarget (`logical`) \cr
#'   If TRUE, the target will be labeled. Default is TRUE.
#' @param colbarreverse (`logical`) \cr
#'   If TRUE, The colorbar will be reversed. Default is FALSE.
#' @param autosort (`logical`) \cr
#'   If True, The columns will be ordered. Default is TRUE.
#' @param numericNA (`character(1)` \cr
#'   If "Max", the NA values of the column are displayed as maximum value. If "Min", then the NA values are displayed as minimum value.
#'   Default is "Max".
#'
#' @return A [plotly] object.
#'
#' @seealso [plotHeatmap] [plotPartialDependence] [plotImportance]
#'
#' @examples
#' library(mlr3)
#' data(glmnet_ela)
#' task_glmnet_ela = TaskRegr$new(id = "task_glmnet", backend = glmnet_ela, target = "logloss")
#' plotParallelCoordinate(task = task_glmnet_ela)
#'
#' @export


plotParallelCoordinate <- function(task, features = NULL, labelside = "Top", labelangle = 0, colbarrange = NULL, constrainrange = NULL,
                                   labeltarget = TRUE,  colbarreverse = FALSE, autosort = TRUE, numericNA = "Max") {

  # check input
  assert_task(task)
  assert(checkNull(features), checkVector(features), combine = "or")
  assert_choice(labelside, c("Top","Bottom"))
  assert_choice(numericNA, c("Max","Min"))
  assert_numeric(labelangle, len = 1)
  assert(checkNumeric(colbarrange, len = 2), checkNull(colbarrange))
  assert(checkNumeric(constrainrange, len = 2, lower = 0, upper = 1), checkNull(constrainrange))
  assert_logical(labeltarget)
  assert_logical(colbarreverse)
  assert_logical(autosort)

  labelside <- ifelse(labelside == "Top", "top", "bottom")

  # make the task more robust
  po =  po("removeconstants") %>>% po("fixfactors")
  task = po$train(task)[[1]]

  # retrieve the target and features variables which are stored inside the task
  df <- as.data.frame(task$data(), stringsAsFactors = TRUE)


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
  if (is.null(features))
    featureNames <- task$feature_names
  else
    featureNames <- features

  # transform the df object into a vector
  targetVector <-  df[[targetName]]

  # constrain range in the target variable
  if (is.null(constrainrange) == FALSE && !all(constrainrange %in% c(0,1))) {
    targetOrdered <- targetVector[order(targetVector, decreasing = FALSE)]
    indexRange <- round(length(targetOrdered)*constrainrange)
    targetSubset <- targetOrdered[min(indexRange):max(indexRange)]
    targetindex <- match(targetName,names(df))
    dfSubset <- subset(df, df[, targetindex] >= min(targetSubset) & df[, targetindex] <= max(targetSubset))
  }


  # prepare the colorbar for the target
  if (is.null(colbarrange))
    colbarrange <- targetVector

  # retrieve the feature columns from the data.frame
  featureVar <- match(featureNames, names(df))

  # initialize an empty list for the features
  hyperparam <- list()

  # If automatic sorting is enabled, the column names are sorted by their correlation
  # retrieve the columns of the characteristics from the data.frame
  if (autosort == TRUE) {
    featuresDf <- df[featureVar]
    featuresDf <- as.data.frame(lapply(featuresDf, function(x) if (is.character(x) || is.logical(x)) as.factor(x) else x))
    featuresDf <- as.data.frame(lapply(featuresDf, as.numeric))
  corr <- stats::cor(featuresDf)
  featuresDf <- corrplot::corrMatOrder(corr, order = 'FPC')
  featuresDf <- corr[featuresDf, featuresDf]
  featureVar <- match(colnames(featuresDf),names(df))
  }

  # each hyperparameter gets an own list with an own scale and label
  for (i in featureVar) {
    paramVector <- df[,i]

    # prepare the list for non-numeric features
    if (!is.numeric(paramVector)) {
      paramVector <- if(!is.factor(paramVector)) as.factor(paramVector) else paramVector
      tickvals <- 1:nlevels(paramVector)
      ticktext <- levels(paramVector)
      numVector <- as.numeric(paramVector)
      paramList <- list(tickvals = tickvals, ticktext = ticktext,
                label = names(df[i]), values = numVector)

      # constrain range in the feature variables
      if (is.null(constrainrange) == FALSE &&  !all(constrainrange %in% c(0,1))) {
        dfSubset[,i] <- as.factor(dfSubset[,i])
        remainingLvl <- levels(droplevels(dfSubset[,i]))
        indexLvl <- match(remainingLvl, levels(paramVector))
        paramList <- c(paramList, constraintrange = list(c(min(indexLvl),max(indexLvl))))
      }

    # prepare the list for numeric features
    } else {
    paramList <- list(range = c(min(paramVector), max(paramVector)),
              label = names(df[i]), values = paramVector)

      # constrain range in the feature variables
      if (is.null(constrainrange) == FALSE &&  !all(constrainrange %in% c(0,1)))
        paramList <- c(paramList, constraintrange = list(c(min(dfSubset[,i]), max(dfSubset[,i]))))
    }

    # add the selected hyperparameter to the list of hyperparameters to be displayed.
    hyperparam <- c(hyperparam, list(paramList))
  }

  # create a plotly object
  pcp <- df %>%
    plot_ly() %>% add_trace(type = "parcoords", labelangle = labelangle, labelside = labelside,
                            line = list(color = ~targetVector,
                                        colorscale = 'Jet',
                                        reversescale = colbarreverse,
                                        cmin = min(colbarrange),
                                        cmax = max(colbarrange),
                                        colorbar = list(title = list(text = ifelse(labeltarget == TRUE, targetName, ""), side = labelside))),
                            dimensions = hyperparam)

  pcp

}
