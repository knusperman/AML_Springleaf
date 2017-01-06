

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

source("Source/prediction/data_functions.r")
data_numeric = buildNumericData(c(3)) #imputed sample

data_factors = as.data.frame(readRDS("data/final/factorAttributes.rds"))[rownames(data_numeric),] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(data_numeric),]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(data_numeric),] #f
data_target  = as.data.frame(read.csv("data/target.csv"))[rownames(data_numeric),] #f #full train records



mydata <- cbind(data_numeric,data_factors,data_strings,data_dates, data_target)

colnames(mydata)[ncol(mydata)]="target"
mydata$target <- as.factor(mydata$target)

dim(mydata)
#cleaning the environment
remove(data_numeric)
remove(data_factors)
remove(data_strings)
remove(data_dates)
remove(data_target)

#remove cols you do not want right now
#mydata <- mydata[,-which(colnames(mydata) %in% collist$cols_dates)]

classif.task = makeClassifTask(id = "mtc", data = mydata, target = "target", positive="1")
set.seed(1234)
n = getTaskSize(classif.task) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

listLearners(classif.task)

