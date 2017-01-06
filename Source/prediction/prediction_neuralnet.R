if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"nnet" %in% installed.packages()) install.packages("nnet")
require(mlr)
require(nnet)

data = readRDS("data/final/cleansedDataset_subset.rds") # change to final when done
data = mydata
set.seed(1234)
samp = sample(1:nrow(data), 30000)
task = makeClassifTask(id = "nn", data = data[samp[1:20000],], target = "target", positive="1")
learner_nn = makeLearner("classif.avNNet", predict.type = "prob", fix.factors.prediction = TRUE)
learner_nn = setHyperPars(learner_nn, MaxNWts = 35011)
learner_nn = setHyperPars(learner_nn, size = 10) #100 does not work
model = train(learner_nn, task)

pred = predict(model, newdata = data[samp[20001:30000],])
mlr::performance(pred,auc)
sum(is.na(pred$data$response)) #77
pred$data = pred$data[!is.na(pred$data$response),]

getPlotAUC(list(nnet = pred))

perf = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
plotROCCurves(perf)
