#' @title Parallel Coordinate plot


plotParallelCoordinate <- function(task, labeltarget = TRUE, labelside = "top", labelangel = 0, colbarrange = NULL,
                                   colbarreverse = FALSE, constrainrange = NULL, autoorder = TRUE) {

  #retrieve the target and features variables which are stored inside the task
  df <- as.data.frame(task$data())
  target <- task$target_names
  features <- task$feature_names

  #transform the df object into a vector
  target_vector <-  df[[target]]

  #constrain range
  if(is.null(constrainrange) == FALSE)
  {
    target_ordered <- target_vector[order(target_vector, decreasing = FALSE)]
    index <- round(length(target_ordered)*constrainrange)
    target_subset <- target_ordered[min(index):max(index)]
    targetindex <- match(target,names(df))
    df_subset <- subset(df, df[,targetindex] >= min(target_subset) & df[,targetindex] <= max(target_subset))
  }


  #prepare the colorbar for the target
  if(is.null(colbarrange))
    colbarrange <- target_vector

  #get the rows of the features in the data.frame
  variable <- match(features,names(df))

  #initialize an empty list for the features
  hyperparm <- list()

  if(autoorder == TRUE){
    test <- df[variable]
    test <- as.data.frame(lapply(test, function(x) if(is.character(x)||is.logical(x)) as.factor(x) else x))
    test <- as.data.frame(lapply(test, as.numeric))
  corr <- cor(test)
  invisible(capture.output(test <- corReorder(corr, order = ("chain"))))
  variable <- match(colnames(test),names(df))
  }

  #each hyperparameter gets an own list with an own scale and label
  for(i in variable){
    vector <- df[,i]
    if(!is.numeric(vector)){
      vector <- if(!is.factor(vector)) as.factor(vector) else vector
      tickvals <- 1:nlevels(vector)
      ticktext = levels(vector)
      vectornum <- as.numeric(vector)
      x <- list(tickvals = tickvals, ticktext = ticktext,
                label = names(df[i]), values = vectornum)
      if(is.null(constrainrange) == FALSE){
      remaininglvl <- levels(droplevels(df_subset[,i]))
      indexlvl <- match(remaininglvl, levels(vector))
      x <- c(x, constraintrange = list(c(min(indexlvl),max(indexlvl))))
      }
    }
    else {
    x <- list(range = c(min(vector), max(vector)),
              label = names(df[i]), values = vector)
    if(is.null(constrainrange) == FALSE)
    x <- c(x, constraintrange = list(c(min(df_subset[,i]), max(df_subset[,i]))))
    }
    hyperparm <- c(hyperparm, list(x))
  }

  #plot a plotly object
  fig <- df %>%
    plot_ly() %>% add_trace(type = "parcoords", labelangle = labelangel, labelside = labelside,
                            line = list(color = ~target_vector,
                                        colorscale = 'Jet',
                                        reversescale = colbarreverse,
                                        cmin = min(colbarrange),
                                        cmax = max(colbarrange),
                                        colorbar = list(title = list(text = ifelse(labeltarget == labeltarget,
                                                                                   target, ""), side = labelside))),
                            dimensions = hyperparm)



  fig

}
