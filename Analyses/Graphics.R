library(VisHyp)
library(mlr3)
library(plotly)
library(patchwork)
library(ggpubr)
library(rstudioapi)
library(mlr3pipelines)
toBibtex(citation("iml"))

data <- read.csv("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Data/iaml_glmnet.csv")
str(data)
summary(as.factor(data$task_id))
testData <- data[data$task_id == 1489,]
testData <- testData[, names(testData) %in% c("f1", "rammodel", "ias", "mmce", "alpha", "nf", "logloss")]
task = TaskRegr$new(id = "task_glmnet", backend = testData, target = "logloss")
po = po("imputehist") %>>% po("fixfactors") %>>% po("imputeoor")  %>>% po("removeconstants")
task = po$train(task)[[1]]
df <- task$data()
learner = lrn("regr.ranger", num.trees = 100)
learner$train(task)
groups = list(
  alpha = c("alpha"),
  f1 = c("f1"),
  alpha_f1 = c("f1", "alpha")
)
model <- iml::Predictor$new(learner, data = df, y = "logloss")
importancePlot <- iml::FeatureImp$new(model, loss = "mae", features = c("f1", "alpha"))
plot(importancePlot)

# Heatmap
Heatmap_Theory <- plotHeatmap(task, c("nf","alpha"), gridsize = 10, rug = FALSE)
Heatmap_Theory
save_image(Heatmap_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Heatmap_Theory.png",
           width = 700, height = 500, scale = 2 )

# PCP
plotParallelCoordinate_Theory <- plotParallelCoordinate(task, labelangle = 0, colbarreverse = TRUE)
plotParallelCoordinate_Theory
save_image(plotParallelCoordinate_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/plotParallelCoordinate_Theory.png",
           width = 700, height = 500, scale = 2 )

#Scatterplot
scatter1 <- ggplot(data, aes(x=mmce,y=logloss)) + geom_point()
scatter1
scatter2 <- ggplot( data, aes(x=mmce,y=ias)) + geom_point(aes(color = logloss))
scatter2
scatter1 + scatter2
savePlotAsImage("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Scatter_Theory.png", format = "png", width = 700, height = 500)


# PDP
PDP_Theory <- plotPartialDependence(task, "rammodel", plotICE = FALSE)
PDP_Theory
save_image(PDP_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_Theory.png",
           width = 700, height = 500, scale = 2 )


# Importance Plot
Importance_Theory <- plotImportance(task)
Importance_Theory
save_image(Importance_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Importance_Theory.png",
           width = 700, height = 500, scale = 2 )

# ICE plot
testData2 <- data[, names(data) %in% c("f1", "rammodel", "ias", "mmce", "alpha", "nf", "ramtrain", "logloss")]
testsample <- sample(1:nrow(testData2), 1000, replace = TRUE)
testData2 <- testData2[testsample, ]
task2 = TaskRegr$new(id = "task_glmnet", backend = testData2, target = "logloss")
ICE_Theory <- plotPartialDependence(task2, "ramtrain", plotICE = TRUE, rug = FALSE)
ICE_Theory <- ICE_Theory +
  ggplot2::ggtitle("PDP + ICE")

# ALE
learner = lrn("regr.ranger", num.trees = 100)
features <- "ramtrain"

po = po("imputehist") %>>% po("fixfactors") %>>% po("imputeoor")  %>>% po("removeconstants")
task2 = po$train(task2)[[1]]
learner$train(task2)
df <- task2$data()
df <- as.data.frame(df)
targetName <- task2$target_names
targetVector <-  df[[targetName]]
index <- which(names(df) != targetName)
df <- df[index]
model <- iml::Predictor$new(learner, data = df, y = targetVector)
pdp <- iml::FeatureEffect$new(model, feature = features, method = "ale", grid.size = 20)
ALE_Theory <- plot(pdp, rug = FALSE) +
  ggplot2::ggtitle("ALE Plot")

ICE_ALE_Theory <- ICE_Theory + ALE_Theory
ICE_ALE_Theory
savePlotAsImage("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/ICE_ALE_Theory.png", format = "png", width = 700, height = 500)


?save_image
subplot(Heatmap_Theory, PDP_Theory,nrows = 2)
?subplot


# PDP vs PDP + ICE
testData3 <- read_csv("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Data/data/iaml_ranger/data.csv")
str(testData3)
testsample2 <- sample(1:nrow(testData3), 1000, replace = TRUE)
testData3 <- testData3[testsample2, ]
testData3 <- as.data.frame(testData3)
n <- length(testData3)
for (i in 1:n) {
  if(is.logical(testData3[,i]))
    testData3[,i] <- as.factor(testData3[,i])
  if(is.character(testData3[,i]))
    testData3[,i] <- as.factor(testData3[,i])
}
task3 = TaskRegr$new(id = "task_glmnet", backend = testData3, target = "logloss")
plotPartialDependence(task3, c("splitrule","rampredict"))
ICE_Categorical1 <- plotPartialDependence(task3, c("splitrule"), plotICE = FALSE,rug = FALSE)
ICE_Categorical2 <- plotPartialDependence(task3, c("splitrule"), plotICE = TRUE, rug = FALSE)
PDP2_Theory <- ICE_Categorical1 + ICE_Categorical2
PDP2_Theory
savePlotAsImage("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP2_Theory.png", format = "png", width = 700, height = 500)


### Auswertung
#lcbench
PlotImportance_lcbench <- plotImportance(lcbenchTask)
save_image(PlotImportance_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PlotImportance_lcbench.png",
           width = 700, height = 500, scale = 2 )

PDP_samples_lcbench <- plotPartialDependence(lcbenchTask, features = c("sample"), rug = FALSE, plotICE = FALSE)
save_image(PDP_samples_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_samples_lcbench.png",
           width = 700, height = 500, scale = 2 )

PDP_survival_fraction_lcbench <- plotPartialDependence(bohbSubset, features = c("survival_fraction"), rug = TRUE, plotICE = TRUE)
PDP_survival_fraction_lcbench
save_image(PDP_survival_fraction_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_survival_fraction_lcbench.png",
           width = 700, height = 500, scale = 2 )


p1 <- plotPartialDependence(knn1Subset, "random_interleave_fraction", plotICE = FALSE)
p2 <- plotPartialDependence(knn7Subset, "random_interleave_fraction", plotICE = FALSE)
p3 <- plotPartialDependence(bohblrnSubset, "random_interleave_fraction", plotICE = FALSE)
p4 <- plotPartialDependence(rangerSubset, "random_interleave_fraction", plotICE = FALSE)
PDP_random_interleave_fraction_lcbench <- (p1 | p2 ) / (p3 | p4 )
#save_image(PDP_random_interleave_fraction_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_random_interleave_fraction_lcbench.png",
#           width = 700, height = 500, scale = 2 )
#
# PCP_surrogate_learner_lcbench <- plotParallelCoordinate(knn1BestTaskBest, labelangle = 10)
# save_image(PCP_surrogate_learner_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PCP_surrogate_learner_lcbench.png",
#            width = 700, height = 500, scale = 2)

#super
PDP1_survival_fraction_super <- plotPartialDependence(superTask, features = c("survival_fraction"), rug = TRUE, plotICE = FALSE)
PDP2_survival_fraction_super <- plotPartialDependence(superTaskBest, features = c("survival_fraction"), rug = TRUE, plotICE = FALSE)
PDP3_survival_fraction_super <- PDP1_survival_fraction_super + PDP2_survival_fraction_super
PDP3_survival_fraction_super
savePlotAsImage("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_survival_fraction_super.png", format = "png", width = 700, height = 500)


#test
test1 <- plotParallelCoordinate(task)
test2 <- plotParallelCoordinate(task)
test1 + test2
