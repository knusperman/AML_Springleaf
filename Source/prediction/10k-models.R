source("Source/prediction/prediction_initialize.R")
#build 10k subset
train.set <- train.set[1:9000]
test.set <- test.set[1:1000]
mydata <- mydata[c(train.set,test.set),]
classif.task.10k <-  makeClassifTask(id = "mtc10k", data = mydata, target = "target", positive="1")

#build each model

#RF
rf10k <- buildRF(classif.task.10k, train.set, test.set)
print("RF: AUC -- TIME ")
paste(rf10k$auc, "--", rf10k$model$time/60, " min")
saveRDS(rf10k,"rf10k.rds")
#XGB
xg10k <- buildXG(classif.task.10k, train.set, test.set,list(nrounds=20)) #1 round default makes no sense... so to be fair lets say 20
print("XGB: AUC -- TIME ")
paste(xg10k$auc, "--", xg10k$model$time/60, " min")
saveRDS(xg10k,"xg10k.rds")
#RPART
rpart10k <- buildRPART(classif.task.10k, train.set, test.set)
print("RPART: AUC -- TIME ")
paste(rpart10k$auc, "--", rpart10k$model$time/60, " min")
saveRDS(rpart10k,"rpart10k.rds")
#SVM
classif.lrn.SVM = makeLearner("classif.svm", predict.type = "prob", fix.factors.prediction = TRUE)
svm10k = train(classif.lrn.SVM, classif.task.10k, subset = train.set)
svm10kpred = predict(svm10k, task = classif.task.10k, subset = test.set)
print("SVM: AUC -- TIME ")
paste(mlr::performance(svm10kpred, auc), "--", svm10k$time/60, " min")
saveRDS(svm10k, "svm10k.rds")
