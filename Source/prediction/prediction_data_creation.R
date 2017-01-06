<<<<<<< HEAD
####################################################################################
#######OLD #########################################################################
#numericalDataSample <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")
#numericalDataSample <- as.data.frame(numericalDataSample)
#data_numeric1 = as.data.frame(readRDS("data/numeric imputations/imp10k.rds"))
#data_numeric2 = as.data.frame(readRDS("data/numeric imputations/imp10-30k.rds"))
#data_numeric3 = as.data.frame(readRDS("data/numeric imputations/imp30-40k.rds"))
#data_numeric4 = as.data.frame(readRDS("data/numeric imputations/imp40-50k.rds"))
#data_numeric5 = as.data.frame(readRDS("data/numeric imputations/imp50-70k.rds"))
#data_numeric6 = as.data.frame(readRDS("data/numeric imputations/imp70-90k.rds"))
#data_numeric7 = as.data.frame(readRDS("data/numeric imputations/imp90-110k.rds"))
#data_numeric8 = as.data.frame(readRDS("data/numeric imputations/imp110-130k.rds"))
#data_numeric9 = as.data.frame(readRDS("data/numeric imputations/imp130k+.rds"))
#data_numeric = rbind(data_numeric1,data_numeric2,data_numeric3,data_numeric4,data_numeric5,data_numeric6,data_numeric7, data_numeric8,data_numeric9)
#data_numeric = data_numeric[-c(130001),]
####################################################################################
####### END OLD ####################################################################

s <- readRDS("data/sample.rds") #1 = 1:50000, 2 = 500001:100000, 3 = 100001:145231 in train set

=======

if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"e1071" %in% installed.packages()) install.packages("e1071")
if (!"devtools" %in% installed.packages()) install.packages("devtools")
if (!"ROCR" %in% installed.packages()) install.packages("ROCR")
if(!"doSNOW" %in% installed.packages()) install.packages("doSNOW")
if(!"foreach" %in% installed.packages()) install.packages("foreach")
if(!"randomForest" %in% installed.packages()) install.packages("randomForest")
if(!"rpart" %in% installed.packages()) install.packages("rpart")
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
library(rpart)
library(mlr)
library(e1071)
library(ROCR)
library(doSNOW)
library(randomForest)
library(foreach)

#data_numeric1a = as.data.frame(readRDS("data/numeric imputations/impsplit1_done1.rds"))
#data_numeric1b = as.data.frame(readRDS("data/numeric imputations/impsplit1_done2.rds"))
#data_numeric1 = rbind(data_numeric1a,data_numeric1b)
#remove(data_numeric1a,data_numeric1b)
#data_numeric2a = as.data.frame(readRDS("data/numeric imputations/impsplit2_done1.rds"))
#data_numeric2b = as.data.frame(readRDS("data/numeric imputations/impsplit2_done2.rds"))
#data_numeric2 = rbind(data_numeric2a,data_numeric2b)
#remove(data_numeric2a,data_numeric2b)
>>>>>>> 7b9765dd5315627bb41fcb98596cae663bc246cd
data_numeric3a = as.data.frame(readRDS("data/numeric imputations/impsplit3_done1.rds"))
data_numeric3b = as.data.frame(readRDS("data/numeric imputations/impsplit3_done2.rds"))
data_numeric3 = rbind(data_numeric3a,data_numeric3b)
remove(data_numeric3a,data_numeric3b)
#data_numeric = rbind(data_numeric1,data_numeric2,data_numeric3)
data_numeric = data_numeric3 #imputed sample
remove(data_numeric1,data_numeric2,data_numeric3)
#check if rows correspond to sample, so that merge with factors etc. is correct:
s <- readRDS("data/sample.rds") #1 = 1:50000, 2 = 500001:100000, 3 = 100001:145231 in train set
#s[c(1:5,50001:50005,100001:100005)] == rownames(data_numeric)[c(1:5,50001:50005,100001:100005)]

data_factors = as.data.frame(readRDS("data/final/factorAttributes.rds"))[rownames(data_numeric),] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(data_numeric),]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(data_numeric),] #f
data_target  = as.data.frame(read.csv("data/target.csv"))[rownames(data_numeric),] #f #full train records


#collist = list("cols_numeric"=colnames(data_numeric),"cols_factors"=colnames(data_factors),"cols_strings" = colnames(data_strings), "cols_dates" = colnames(data_dates))
#saveRDS(collist, "data/collist.rds")

NAstatistics <- readRDS("data/NAstatistics.rds") #since data has no more information about amout of imputation in a column
collist <- readRDS("data/collist.rds") #some data for selecting the right columns for predicting

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
<<<<<<< HEAD
mydata <- mydata[,-which(colnames(mydata) %in% collist$cols_dates)]
=======
#mydata <- mydata[,-which(colnames(mydata) %in% collist$cols_dates)]

classif.task = makeClassifTask(id = "mtc", data = mydata, target = "target", positive="1")
set.seed(1234)
n = getTaskSize(classif.task) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

listLearners(classif.task)

#im_feat <- generateFilterValuesData(classif.task, method = c("information.gain","chi.squared"))
#plotFilterValues(im_feat,n.show = 20)



>>>>>>> 7b9765dd5315627bb41fcb98596cae663bc246cd
