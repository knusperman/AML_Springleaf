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
<<<<<<< HEAD:Source/prediction/prediction_nnet.R
learner_nn = setHyperPars(learner_nn, MaxNWts = 999999)
learner_nn = setHyperPars(learner_nn, size = 10) # cannot use size = 100 apparently (error message cannot allocate vector of size <some> kbs)
=======
learner_nn = setHyperPars(learner_nn, MaxNWts = 35011)
learner_nn = setHyperPars(learner_nn, size = 10) #100 does not work
>>>>>>> 15fdf27a75265ecd7817c4d7c1c4d9e310209bb1:Source/prediction/prediction_neuralnet.R
model = train(learner_nn, task)

pred = predict(model, newdata = data[samp[20001:30000],])
mlr::performance(pred,auc)
sum(is.na(pred$data$response)) #77
pred$data = pred$data[!is.na(pred$data$response),]

<<<<<<< HEAD:Source/prediction/prediction_nnet.R
saveRDS(pred, "data/predictions/pred_nnet_cleansedDataset_subset.rds")
=======
getPlotAUC(list(nnet = pred))

perf = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
plotROCCurves(perf)
>>>>>>> 15fdf27a75265ecd7817c4d7c1c4d9e310209bb1:Source/prediction/prediction_neuralnet.R
