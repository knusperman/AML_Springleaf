# In this file, results based on different subsets (divided by different attribute types) are tested
library(xgboost)
library(mlr)

mydata <- buildDataSet(c(3)) # 3 indicates the third data sample part, which is used for training (45k records)

xgtune_high3 <- readRDS("models/imputed/xgtune_high3.rds") # results of xg_tuningplan.R
source("source/prediction/prediction_functions.R")

#build different sets
#only factors, only numerical, only date, only string, only boolean
attributes = readRDS("data/attributes2.rds")
attributes = attributes[rownames(attributes) %in% colnames(mydata),]
factors = readRDS("data/factorPotentials.rds")
factorSet = mydata[,colnames(mydata) %in% rownames(factors)]
numericalAttributes = attributes[attributes[,4],4]
dateAttributes = attributes[attributes[,2],2]
stringAttributes = attributes[attributes[,3],3]
booleanAttributes = attributes[attributes[,1],1]

numericalSet = mydata[,colnames(mydata) %in% names(numericalAttributes)]
numericalSet = numericalSet[,!colnames(numericalSet) %in% rownames(factors)]
dateSet = mydata[,colnames(mydata) %in% names(dateAttributes)]
stringSet = mydata[,colnames(mydata) %in% names(stringAttributes)]
booleanSet = mydata[,colnames(mydata) %in% names(booleanAttributes)]

# add extracted date columns to date set
dateSet = cbind(dateSet, mydata[,!colnames(mydata) %in% rownames(attributes)])
dateSet = dateSet[, -ncol(dateSet)] # remove target

# reinsert target variable
target = mydata[,ncol(mydata)]
numericalSet = cbind(numericalSet, target = target)
factorSet = cbind(factorSet, target = target)
dateSet = cbind(dateSet, target = target)
stringSet = cbind(stringSet, target = target)
booleanSet = cbind(booleanSet, target = target)

# apparently xgboost requires at least one numerical column
# insert dummy column
dummy = rep(1, nrow(factorSet))
factorSet = cbind(dummy, factorSet)
dateSet = cbind(dummy, dateSet)
stringSet = cbind(dummy, stringSet)
booleanSet = cbind(dummy, booleanSet)

###MLR Setup
classif_task_numerical = makeClassifTask(id = "mtc", data = numericalSet, target = "target", positive="1")
classif_task_factor = makeClassifTask(id = "mtc", data = factorSet, target = "target", positive="1")
classif_task_date = makeClassifTask(id = "mtc", data = dateSet, target = "target", positive="1")
classif_task_string = makeClassifTask(id = "mtc", data = stringSet, target = "target", positive="1")
classif_task_boolean = makeClassifTask(id = "mtc", data = booleanSet, target = "target", positive="1")

set.seed(1234)
n = getTaskSize(classif_task) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]


res_numerical =  buildXG(classif_task_numerical,train.set,test.set,xgtune_high3$x)
res_factor =  buildXG(classif_task_factor,train.set,test.set,xgtune_high3$x)
res_date =  buildXG(classif_task_date,train.set,test.set,xgtune_high3$x)
res_string =  buildXG(classif_task_string,train.set,test.set,xgtune_high3$x)
res_boolean =  buildXG(classif_task_boolean,train.set,test.set,xgtune_high3$x)

res_numerical$auc #0.7762901
res_factor$auc #0.7385283
res_date$auc #0.6278982
res_string$auc #0.6240345
res_boolean$auc #0.6379374

png("fig/subset_comparison.png", height = 800, width = 1200)
getPlotAUC(list(numerical = res_numerical$predictions, factor = res_factor$predictions, date = res_date$predictions,
                string = res_string$predictions, boolean = res_boolean$predictions))
dev.off()