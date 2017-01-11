library(xgboost)
library(parallelMap)
library(rpart)
library(mlr)
library(e1071)
library(ROCR)
library(doSNOW)
library(randomForest)
library(foreach)

source("Source/prediction/prediction_functions.R")

mydata_fullsample <- buildCombinedDataSet() 
trainrows <- mydata_fullsample$trainrownames
testrows <- mydata_fullsample$testrownames
trainindices <- 1:145231 #length train
testindices <- 145232:290463 #length test
#different to rownames, which are strings, but can also be used to subset. But not for mlR task.
mydata_fullsample <- mydata_fullsample$data

###we do this here to ensure right factor levels before task is created
for(i in which(sapply(mydata_fullsample, class) %in%c("character","logical"))){
  mydata_fullsample[,i] = as.factor(mydata_fullsample[,i])
}
#final factor level print out for checking
#sapply(mydata_fullsample[,which(sapply(mydata_fullsample, class) %in% c("factor"))],levels)

classif_task_full = makeClassifTask(id = "mtcfull", data = mydata_fullsample, target = "target", positive="1")

#optimal xg paramters
xgparam <- list(nrounds =1, eta =0.02,max_depth=14,colsample=0.6,subsample=0.8)
set.seed(1234)
xg_tuned_submission <- buildXG(classif_task_full, trainindices, testindices,pars = xgparam)


