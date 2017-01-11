library(mlr)
library(xgboost)
source("source/prediction/prediction_functions.R")

data = buildDataSet(c(1,2,3), TRUE, TRUE)
set.seed(1234)

classif_task_full = makeClassifTask(id = "mtc", data = data, target = "target", positive="1")

n = getTaskSize(classif_task_full) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

nrounds = 1000 
eta = 0.02
max_depth = 14
colsample = 0.6
subsample = 0.8
params = list(nrounds = nrounds, eta = eta, max_depth = max_depth, colsample_bytree = colsample, subsample = subsample)
full_xg_model = buildXG(classif_task_full, train.set,test.set, params) # fit on full train set

saveRDS(full_xg_model, "full_train_xg_model.rds")


mydata_fullsample <- buildCombinedDataSet() 
trainrows <- mydata_fullsample$trainrownames
testrows <- mydata_fullsample$testrownames
trainindices <- 1:145231 #length train
testindices <- 145232:290463 #length test
#different to rownames, which are strings, but can also be used to subset. But not for mlR task.
mydata_fullsample <- mydata_fullsample$data
#final factor level print out for checking
#sapply(mydata_fullsample[,which(sapply(mydata_fullsample, class) %in% c("factor"))],levels)

classif_task_full = makeClassifTask(id = "mtcfull", data = mydata_fullsample, target = "target", positive="1")

#optimal xg paramters
xgparam <- list(nrounds =500, eta =0.02,max_depth=14,colsample_bytree=0.6,subsample=0.8)
set.seed(1234)
xg_tuned_submission <- buildXG(classif_task_full, trainindices, testindices,pars = xgparam)
saveRDS(xg_tuned_submission, "finalXGmodel.rds")
