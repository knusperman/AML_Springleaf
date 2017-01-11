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

nrounds = 1000 #MC
eta = 0.02
max_depth = 14
colsample = 0.6
subsample = 0.8
params = list(nrounds = nrdouns, eta = eta, max_depth = max_depth, colsample_bytree = colsample, subsample = subsample)
tuneResults11 = buildXG(classif_task, train.set,test.set, params)