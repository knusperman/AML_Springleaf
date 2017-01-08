

if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"e1071" %in% installed.packages()) install.packages("e1071")
if (!"devtools" %in% installed.packages()) install.packages("devtools")
if (!"ROCR" %in% installed.packages()) install.packages("ROCR")
if(!"doSNOW" %in% installed.packages()) install.packages("doSNOW")
if(!"foreach" %in% installed.packages()) install.packages("foreach")
if(!"randomForest" %in% installed.packages()) install.packages("randomForest")
if(!"rpart" %in% installed.packages()) install.packages("rpart")
if(!"parallelMap" %in% installed.packages()) install.packages("parallelMap")
library(devtools)
if ("xgboost" %in% installed.packages()){
  if(packageVersion("xgboost")!="0.4.4"){
    remove.packages("xgboost")
    install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org") #necessary to run with mlr
  }
}else if(!"xgboost" %in% installed.packages()){
  install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org") #necessary to run with mlr
}

library(xgboost)
library(parallelMap)
library(rpart)
library(mlr)
library(e1071)
library(ROCR)
library(doSNOW)
library(randomForest)
library(foreach)
baselineData = readRDS("data/final/baselineDataset.rds")

colnames(mydata)[ncol(mydata)]="target"

baselineData_sample = baselineData[sample(1:nrow(baselineData), 45000),]


classif_task_baseline = makeClassifTask(id = "mtc", data = baselineData_sample, target = "target", positive="1")

set.seed(1234)
n = getTaskSize(classif_task_baseline) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]
