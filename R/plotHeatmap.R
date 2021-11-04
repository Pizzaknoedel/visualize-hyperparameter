#' @title Heatmap


plotHeatmap <- function(task, features, gridsize = 20) {

  #aufpassen auf unterschiedlichen datainput (kategorial, numerisch)

  library(ggplot2)
  task <- task
  feature <- c("mnth", "season")
  features2 <- task$feature_names
  target <- task$target_names

  df <- task$data()

  #transform the df object into a vector
  f <- df[[feature[1]]]
  f2 <- df[[feature[2]]]
  t <-  df[[target]]


    ggplot(df, aes(x = f, y = f2, z = t)) +
      geom_tile(stat = "summary_2d", fun = mean, bins = gridsize) +
      labs(title = "Heatmap",
           fill = "logloss")+
      geom_rug(alpha = 0.2, sides = "bl",
      position = position_jitter(width = 0.07, height = 0.07))
}
