library(VisHyp)
library(mlr3)
library(plotly)
library(patchwork)
library(ggpubr)
library(rstudioapi)
library(mlr3pipelines)
library(iml)
toBibtex(citation("iml"))

smashy_super <- smashy_super
smashy_super <- as.data.frame(smashy_super)
n <- length(smashy_super)
for (i in 1:n) {
  if(is.logical(smashy_super[,i]))
    smashy_super[,i] <- as.factor(smashy_super[,i])
  if(is.character(smashy_super[,i]))
    smashy_super[,i] <- as.factor(smashy_super[,i])
}
# Produced graphics as an example for the bachelor thesis
task = TaskRegr$new(id = "smashy_super", backend = smashy_super, target = "yval")
plotPartialDependence(task, features = c("survival_fraction", "random_interleave_random"), rug = FALSE)
plotPartialDependence(task, features = c("filter_algorithm", "budget_log_step"), rug = FALSE)
#plotPartialDependence(task, features = c("filter_factor_last", "budget_log_step"), rug = FALSE)
plotPartialDependence(task, features = c("filter_factor_last", "filter_with_max_budget"), rug = FALSE)
# make the task more robust
po = po("imputehist") %>>%  po("imputeoor") %>>% po("fixfactors", droplevels = FALSE) %>>%
  po("removeconstants") %>>% po("imputesample")#, affect_columns = selector_type("logical"))
task = po$train(task)[[1]]
learner = lrn("regr.ranger", num.trees = 100)
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
plot(iml::FeatureEffect$new(model, feature = features, method = "pdp", grid.size = 10))
featureVector1 <- df[[features[1]]]
featureVector2 <- df[[features[2]]]
is.numeric(featureVector1) & is.factor(featureVector2) | is.factor(featureVector1) & is.numeric(featureVector2)
launchVisHyp()

data <- read_csv("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Data/iaml_rpart.csv")
str(data)
summary(as.factor(data$task_id))
summary(as.factor(data$trainsize))
set.seed(232234)
testsample <- sample(1:nrow(testData), 2000, replace = TRUE)
testData <- data[data$task_id == 1489,]
testData <- testData[testData$trainsize == 1,]
testData <- testData[testsample, ]

#testData <- testData[, names(testData) %in% c("num.trees", "replace", "sample.fraction", "mtry.ratio", "respect.unordered.factors", "min.node.size","splitrule", "logloss")]
df <- testData[, names(testData) %in% c("cp", "maxdepth", "minbucket", "minsplit", "logloss")]
#library(rpart)
#?rpart.control
task = TaskRegr$new(id = "task_glmnet", backend = df, target = "logloss")
po = po("imputehist") %>>% po("fixfactors") %>>% po("imputeoor")  %>>% po("removeconstants")
task = po$train(task)[[1]]
df <- task$data()
learner = lrn("regr.ranger", num.trees = 100)
learner$train(task)
df <- as.data.frame(df)
n <- length(df)
for (i in 1:n) {
  if(is.logical(df[,i]))
    df[,i] <- as.factor(df[,i])
  if(is.character(df[,i]))
    df[,i] <- as.factor(df[,i])
}
str(df)
model <- Predictor$new(learner, data = df, y = "logloss")

# Heatmap
Heatmap_Theory <- plotHeatmap(task, c("maxdepth","minbucket"), gridsize = 10, rug = FALSE)
Heatmap_Theory
save_image(Heatmap_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Heatmap_Theory.png",
           width = 700, height = 500, scale = 2 )

# PCP
plotParallelCoordinate_Theory <- plotParallelCoordinate(task, labelangle = 0, colbarreverse = TRUE,colbarrange = c(0.37, 0.51), autosort = FALSE)
plotParallelCoordinate_Theory
save_image(plotParallelCoordinate_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/plotParallelCoordinate_Theory.png",
           width = 700, height = 500, scale = 2 )

#Scatterplot with complexity parameter
scatter1 <- ggplot(df, aes(x=minbucket,y=logloss)) + geom_point()
scatter1 #"cp", "maxdepth", "minbucket", "minsplit"
scatter2 <- ggplot( df, aes(x=minbucket,y=minsplit)) + geom_point(aes(color = logloss))
scatter2
scatter1 + scatter2
savePlotAsImage("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Scatter_Theory.png", format = "png", width = 700, height = 500)


# PDP
PDP_Theory <- plotPartialDependence(task, "maxdepth", plotICE = FALSE, rug = FALSE)
PDP_Theory
save_image(PDP_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_Theory.png",
           width = 700, height = 500, scale = 2 )


# Importance Plot
Importance_Theory <- plotImportance(task)
Importance_Theory
save_image(Importance_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Importance_Theory.png",
           width = 700, height = 500, scale = 2 )


# ALE
features <- "maxdepth"
targetName <- task$target_names
targetVector <-  df[[targetName]]
index <- which(names(df) != targetName)
df <- df[index]
model <- iml::Predictor$new(learner, data = df, y = targetVector)
ALE <- iml::FeatureEffect$new(model, feature = features, method = "ale", grid.size = 20)
ALE_Theory <- plot(ALE, rug = FALSE) +
  ggplot2::ggtitle("ALE Plot")
ALE_Theory

# ICE plot
ICE <- iml::FeatureEffect$new(model, feature = features, method = "pdp+ice", grid.size = 20)
ICE_Theory <- plot(ICE, rug = FALSE) +
  ggplot2::ggtitle("PDP + ICE")
ICE_Theory

ICE_ALE_Theory <- ICE_Theory + ALE_Theory
ICE_ALE_Theory
savePlotAsImage("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/ICE_ALE_Theory.png", format = "png", width = 700, height = 500)



# PDP vs PDP + ICE
data2 <- read_csv("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Data/iaml_ranger.csv")
str(data2)
testsample2 <- sample(1:nrow(data2), 2000, replace = TRUE)
data2 <- data2[testsample2, ]
testData3 <- as.data.frame(data2)
n <- length(testData3)
for (i in 1:n) {
  if(is.logical(testData3[,i]))
    testData3[,i] <- as.factor(testData3[,i])
  if(is.character(testData3[,i]))
    testData3[,i] <- as.factor(testData3[,i])
}
task3 = TaskRegr$new(id = "task_glmnet", backend = testData3, target = "logloss")
plotPartialDependence(task3, c("splitrule","sample.fraction"))
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



#Git
n <- length(smashy_super)
for (i in 1:n) {
  if(is.logical(smashy_super[,i]))
    smashy_super[,i] <- as.factor(smashy_super[,i])
  if(is.character(smashy_super[,i]))
    smashy_super[,i] <- as.factor(smashy_super[,i])
}

task = TaskRegr$new(id = "task_glmnet", backend = smashy_super, target = "yval")
learner = lrn("regr.ranger", num.trees = 100)
learner$train(task)
model <- iml::Predictor$new(learner, data = smashy_super, y = "yval")
p1 <- iml::FeatureEffect$new(model, feature = "surrogate_learner", method = "pdp+ice", grid.size = 10)
p1 <- plot(p1, rug = FALSE) +
  ggtitle("Partial Dependence Plot")
p1

p2 <- iml::FeatureEffect$new(model, feature = "random_interleave_fraction", method = "pdp+ice", grid.size = 10)
p2 <- plot(p2, rug = TRUE) +
  ggtitle("Partial Dependence Plot")
p2

legend <- paste0("yval", " (", as.character(substitute(mean)), ")")
p3 <- ggplot(smashy_super, aes_string(x = "sample", y = "surrogate_learner", z = "yval")) +
  geom_tile(stat = "summary_2d", fun = mean, bins = 10) +
  labs(title = "Heatmap", fill = legend)
p3

p4 <- iml::FeatureImp$new(model, loss = "mae")
p4 <- plot(p4) +
  scale_x_continuous(sprintf("Parameter Importance (loss: %s)", "mae")) +
  labs(title = "Importance Plot")
p4
plots <- (p1 | p2 ) / (p3 | p4 )
plots
