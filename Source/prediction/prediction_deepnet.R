if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"deepnet" %in% installed.packages()) install.packages("deepnet")
require(mlr)
require(deepnet)

data = readRDS("data/final/cleansedDataset_subset.rds") # change to final when done
# deepnet only takes numerical values
actualNumericals = readRDS("data/numericalData_withoutCor1.rds")
target = data[,ncol(data)]
factors = readRDS("data/factorPotentials.rds")
factors = rownames(factors)
data = data[,colnames(data) %in% colnames(actualNumericals)]
data = data[,!colnames(data) %in% factors]
remove(actualNumericals)
data = cbind(data, target = target)

set.seed(1234)
samp = sample(1:nrow(data), 30000)
task = makeClassifTask(id = "deepnet", data = data[samp[1:20000],], target = "target", positive="1")
learner_deepnet = makeLearner("classif.dbnDNN", predict.type = "prob", fix.factors.prediction = TRUE)
learner_deepnet = setHyperPars(learner_deepnet, numepochs = 50)
learner_deepnet = setHyperPars(learner_deepnet, hidden = 50)
model = train(learner_deepnet, task)

pred = predict(model, newdata = data[samp[20001:30000],])
sum(is.na(pred$data$response)) #77
pred$data = pred$data[!is.na(pred$data$response),]

saveRDS(pred, "data/predictions/pred_deepnet_cleansedDataset_subset.rds")

source("source/prediction/prediction_helperFunctions.R")
p = getPlotAUC(list(deepnet = pred))

task = makeClassifTask(id = "blah", data = data[samp[1:20000],], target = "target", positive="1")
learner_deepnet = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
learner_deepnet = setHyperPars(learner_deepnet, ntree = 100)
model = train(learner_deepnet, task)
pred = predict(model, newdata = data[samp[20001:30000],])
a = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
plotROCCurves(a)
p = getPlotAUC(list(deepnet = pred))
