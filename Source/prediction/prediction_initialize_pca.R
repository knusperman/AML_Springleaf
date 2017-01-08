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
train_transformed = readRDS("data/PCA_transformed_set.rds")

data_factors = as.data.frame(readRDS("data/final/factorAttributes_FINAL.rds"))[rownames(train_transformed),] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(train_transformed),]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(train_transformed),] #f
data_boolean = as.data.frame(readRDS("data/final/booleanAttributes_FINAL.rds"))[rownames(train_transformed),]
data_target  = as.data.frame(readRDS("data/target.rds"))[rownames(train_transformed),] #f #full train records
mydata <- cbind(train_transformed,data_factors,data_strings,data_dates,data_boolean, data_target)
colnames(mydata)[ncol(mydata)]="target"

set.seed(1234)
mydata_sample = mydata[sample(1:nrow(mydata), 45000),]


classif_task_pca = makeClassifTask(id = "mtc", data = mydata_sample, target = "target", positive="1")

n = getTaskSize(classif_task_pca) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]
