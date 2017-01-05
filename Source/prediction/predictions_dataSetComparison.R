if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"ggvis" %in% installed.packages()) install.packages("ggvis")
require(mlr)
require(ggvis)

# different underlying data
baseline = readRDS("data/final/baselineDataset.rds") # no datacleaning
undifferentiated = readRDS("data/final/undifferentiatedDataset.rds") # imputation done with factors and numericals combined
cleansed = readRDS("data/final/cleansedDataset.rds") # our datacleaning approach

# work with 20k samples for speedup
# test on 10k samples
# first compare all three datasets on a basic random forest with 100 trees (for quick speed)

# sample 20k for training and 10k for testing
set.seed(1234)
samp = sample(1:nrow(baseline), 30000)

# process baseline dataset, drop factors with more than 53 levels
remove = numeric(0)
for (i in 1:ncol(baseline)) {
  if (is.factor(baseline[,i]) & length(levels(baseline[,i])) > 53) remove = c(remove, i)
}
baseline = baseline[,-remove]

# initially, combine the three approaches to justify our data cleaning approach
task_baseline = makeClassifTask(id = "baseline", data = baseline[samp[1:20000],], target = "target", positive="1")
task_undifferentiated = makeClassifTask(id = "undifferentiated", data = undifferentiated[samp[1:20000],], target = "target", positive="1")
task_cleansed = makeClassifTask(id = "cleansed", data = cleansed[samp[1:20000],], target = "target", positive="1")


learner_rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
learner_rf = setHyperPars(learner_rf, ntree = 100)
model = train(learner_rf, task_baseline)
pred = predict(model, newdata = baseline[samp[20001:30000],])
pred$data = pred$data[!is.na(pred$data$response),]
perf = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))

p = getPlotAUC(list(baseline = pred))
p2 = addLinesToAUCPlot(p2, pred, 2)
