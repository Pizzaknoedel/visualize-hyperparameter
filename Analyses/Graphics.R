library(VisHyp)
library(mlr3)
library(plotly)
library(patchwork)
library(ggpubr)
library(rstudioapi)
library(mlr3pipelines)
library(iml)
toBibtex(citation("iml"))
library(glmnet)
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

iaml_rpart <- read_csv("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Data/iaml_rpart.csv")
data <- iaml_rpart
str(data)
summary(as.factor(data$task_id))
summary(as.factor(data$trainsize))
set.seed(232234)
testData <- data[data$task_id == 1489,]
testsample <- sample(1:nrow(testData), 2000, replace = TRUE)
testData <- testData[testData$trainsize == 1,]
testData <- as.data.frame(testData)
testData <- testData[testsample, ]

#testData <- testData[, names(testData) %in% c("num.trees", "replace", "sample.fraction", "mtry.ratio", "respect.unordered.factors", "min.node.size","splitrule", "logloss")]
df <- testData[, names(testData) %in% c("cp", "maxdepth", "minbucket", "minsplit", "logloss")]
task = TaskRegr$new(id = "task_glmnet", backend = df, target = "logloss")
po = po("imputehist") %>>% po("fixfactors") %>>% po("imputeoor")  %>>% po("removeconstants")
task = po$train(task)[[1]]
df <- task$data()
df <- as.data.frame(df)
n <- length(df)
for (i in 1:n) {
  if(is.logical(df[,i]))
    df[,i] <- as.factor(df[,i])
  if(is.character(df[,i]))
    df[,i] <- as.factor(df[,i])
}
str(df)
learner = lrn("regr.ranger", num.trees = 100)
learner$train(task)
model <- Predictor$new(learner, data = df, y = "logloss")

# Heatmap
Heatmap_Theory <- plotHeatmap(task, c("maxdepth","minbucket"), gridsize = 10, rug = FALSE, title = FALSE)
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
scatter2 <- ggplotly(scatter2)
scatterplot <- subplot(scatter1,scatter2, margin = 0.06, titleX = TRUE, titleY = TRUE)
scatterplot
save_image(scatterplot, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Scatter_Theory.png",
           width = 700, height = 400, scale = 2 )

# PDP
PDP_Theory <- plotPartialDependence(task, "maxdepth", plotICE = FALSE, rug = FALSE, title = FALSE)
PDP_Theory
save_image(PDP_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_Theory.png",
           width = 700, height = 500, scale = 2 )


# Importance Plot
Importance_Theory <- plotImportance(task, title = FALSE)
Importance_Theory
save_image(Importance_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/Importance_Theory.png",
           width = 700, height = 450, scale = 2 )


# ALE
features <- "maxdepth"
targetName <- task$target_names
targetVector <-  df[[targetName]]
index <- which(names(df) != targetName)
df <- df[index]
model <- iml::Predictor$new(learner, data = df, y = targetVector)
ALE <- iml::FeatureEffect$new(model, feature = features, method = "ale", grid.size = 20)
ALE_Theory <- plot(ALE, rug = FALSE) #+
  #ggplot2::ggtitle("ALE Plot")
ALE_Theory

# ICE plot
ICE <- iml::FeatureEffect$new(model, feature = features, method = "pdp+ice", grid.size = 20)
ICE_Theory <- plot(ICE, rug = FALSE) #+
  #ggplot2::ggtitle("PDP + ICE")
ICE_Theory

ICE_ALE_Theory <- subplot(ALE_Theory, ICE_Theory, margin = 0.06,  titleX = TRUE, titleY = TRUE)
ICE_ALE_Theory
save_image(ICE_ALE_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/ICE_ALE_Theory.png",
           width = 700, height = 500, scale = 2 )



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
plotPartialDependence(task3, c("splitrule","sample.fraction"), title = FALSE)
ICE_Categorical1 <- plotPartialDependence(task3, c("splitrule"), plotICE = FALSE,rug = FALSE, title = FALSE)
ICE_Categorical2 <- plotPartialDependence(task3, c("splitrule"), plotICE = TRUE, rug = FALSE, title = FALSE)
PDP2_Theory <- subplot(ICE_Categorical1,ICE_Categorical2, margin = 0.06,  titleX = TRUE, titleY = TRUE)
PDP2_Theory
save_image(PDP2_Theory, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP2_Theory.png",
           width = 700, height = 400, scale = 2 )

### Auswertung
#lcbench
PlotImportance_lcbench <- plotImportance(lcbenchTask, title = FALSE)
PlotImportance_lcbench
save_image(PlotImportance_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PlotImportance_lcbench.png",
           width = 700, height = 500, scale = 2 )

PDP_samples_lcbench <- plotPartialDependence(lcbenchTask, features = c("sample"), rug = FALSE, plotICE = FALSE, title = FALSE)
PDP_samples_lcbench
save_image(PDP_samples_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_samples_lcbench.png",
           width = 700, height = 500, scale = 2 )

lcbenchSmashy <- readRDS("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Data/smashy_lcbench.rds")
bohb <- lcbenchSmashy[lcbenchSmashy$sample == "bohb",]
bohbTask <- TaskRegr$new(id = "task_bohb", backend = bohb, target = "yval")
PDP_survival_fraction_lcbench <- plotPartialDependence(bohbTask, features = c("survival_fraction"), rug = TRUE, plotICE = TRUE, title = FALSE)
PDP_survival_fraction_lcbench
save_image(PDP_survival_fraction_lcbench, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_survival_fraction_lcbench.png",
           width = 700, height = 500, scale = 2 )


knn1 <- bohb[bohb$surrogate_learner == "knn1",]
knn7 <- bohb[bohb$surrogate_learner == "knn7",]
bohblrn <- bohb[bohb$surrogate_learner == "bohblrn",]
ranger <- bohb[bohb$surrogate_learner == "ranger",]

knn1Task <- TaskRegr$new(id = "knn1Task", backend = knn1, target = "yval")
knn7Task <- TaskRegr$new(id = "knn7Task", backend = knn7, target = "yval")
bohblrnTask <- TaskRegr$new(id = "bohblrnTask", backend = bohblrn, target = "yval")
rangerTask <- TaskRegr$new(id = "rangerTask", backend = ranger, target = "yval")

p1 <- plotPartialDependence(knn1Task, "random_interleave_fraction", plotICE = FALSE, title = FALSE)
p2 <- plotPartialDependence(knn7Task, "random_interleave_fraction", plotICE = FALSE, title = FALSE)
p3 <- plotPartialDependence(bohblrnTask, "random_interleave_fraction", plotICE = FALSE, title = FALSE)
p4 <- plotPartialDependence(rangerTask, "random_interleave_fraction", plotICE = FALSE, title = FALSE)
PDP_four <- subplot(p1,p2, p3,p4, nrows = 2, margin = 0.06,  titleY = TRUE)
PDP_four <- PDP_four %>% layout(annotations = list(
  list(x = 0.05 , y = -0.09, text = "random_interleave_fraction", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.05 , y = 0.49, text = "random_interleave_fraction", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.95 , y = -0.09, text = "random_interleave_fraction", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.95 , y = 0.49, text = "random_interleave_fraction", showarrow = F, xref='paper', yref='paper'))
)

PDP_four <- subplot(p1,p2, p3,p4, nrows = 2, margin = c(0.04,0.06,0.06,0.06))
PDP_four <- PDP_four %>% layout(annotations = list(
  list(x = 0.5 , y = -0.10, text = "random_interleave_fraction", showarrow = F, xref='paper', yref='paper'),
  list(x = -0.09 , y = 0.52, text = "Predicted. y", showarrow = F, xref='paper', yref='paper',textangle = -90),
  list(x = 0.05 , y = 1.05, text = "knn1", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.65 , y = 1.05, text = "knn7", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.05 , y = 0.47, text = "bohb", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.65 , y = 0.47, text = "ranger", showarrow = F, xref='paper', yref='paper'))
)
save_image(PDP_four, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_random_interleave_fraction_lcbench.png",
           width = 700, height = 500, scale = 2 )


#super
superSmashy <- readRDS("D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/package_VisHyp/data-raw/smashy_super.rds")
superTask <- TaskRegr$new(id = "superSmashy", backend = superSmashy, target = "yval")

PlotImportance_super <- plotImportance(task = superTask)
PlotImportance_super
save_image(PlotImportance_super, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PlotImportance_super.png",
           width = 700, height = 500, scale = 2 )

superBest <- superSmashy[superSmashy$yval >= quantile(superSmashy$yval, 0.8),]
superBestTask <- TaskRegr$new(id = "bestTask", backend = superBest, target = "yval")
PDP1_survival_fraction_super <- plotPartialDependence(superTask, features = c("survival_fraction"), rug = TRUE, plotICE = FALSE, title = FALSE)
PDP2_survival_fraction_super <- plotPartialDependence(superBestTask, features = c("survival_fraction"), rug = TRUE, plotICE = FALSE, title = FALSE)
PDP3_survival_fraction_super <- subplot(PDP1_survival_fraction_super, PDP2_survival_fraction_super, margin = 0.06,  titleX = TRUE, titleY = TRUE)
PDP3_survival_fraction_super
save_image(PDP3_survival_fraction_super, file = "D:/Simon/Desktop/Studium/6. Semester/Bachelorarbeit/Latex/Grafiken/PDP_survival_fraction_super.png",
           width = 700, height = 500, scale = 2 )




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
