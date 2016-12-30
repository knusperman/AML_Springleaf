if (!"xgboost" %in% installed.packages()) remove..packages("xgboost")
if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"e1071" %in% installed.packages()) install.packages("e1071")
if (!"devtools" %in% installed.packages()) install.packages("devtools")
if (!"ROCR" %in% installed.packages()) install.packages("ROCR")
library(mlr)
library(devtools)
install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org") #necessary to run with mlr
library(xgboost)
library(e1071)
library(ROCR)


#merge data sets
data_numeric = as.data.frame(readRDS("data/numerics_sample_imputed.rds")) #10k sample no NAs
data_factors = as.data.frame(readRDS("data/factorAttributes.rds")) #full train records  no NAs b/c treated as level
data_target = data.frame(read.csv("data/target.csv")) #full train records

set.seed(1234) #get same sample of factor attributes, target
s = sample(1:nrow(data_factors),10000)
data_factors <- data_factors[s,]
data_target <- data_target[s,]

mydata <- cbind(data_numeric,data_factors,data_target)
colnames(mydata)[ncol(mydata)]="target"

mydata$target <- as.factor(mydata$target)

classif.task = makeClassifTask(id = "mtc", data = mydata, target = "target", positive="1")

n = getTaskSize(classif.task) #size of data
train.set = sample(n, size = n*0.8)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.SVM = makeLearner("classif.svm", predict.type = "prob", fix.factors.prediction = TRUE)

#get default hyper parameters
classif.lrn.RF$par.set
#manipulate hyper parameters
classif.lrn.RF= setHyperPars(classif.lrn.RF, ntree = 200)
#get changed hyper parameters
getHyperPars(classif.lrn.RF)
#prob or response for classification
#Occasionally, factor features may cause problems when fewer levels are present in the test data set than in the training data. 
#By setting fix.factors.prediction = TRUE these are avoided by adding a factor level for missing data in the test data set.

mod.RF  = train(classif.lrn.RF, classif.task, subset = train.set)
mod.XG  = train(classif.lrn.XG, classif.task, subset = train.set)
mod.SVM = train(classif.lrn.SVM, classif.task, subset = train.set)

pred.RF  = predict(mod.RF, task = classif.task, subset = test.set)
pred.XG  = predict(mod.XG, task = classif.task, subset = test.set)
pred.SVM = predict(mod.SVM, task = classif.task, subset = test.set)

#full response : pred.RF$data

getPredictionProbabilities(pred.XG)

mlr::performance(pred.RF, auc)
mlr::performance(pred.XG, auc)
mlr::performance(pred.SVM, auc)
predDF.RF = generateThreshVsPerfData(pred.RF, measures = list(fpr, tpr, mmce))
plotROCCurves(predDF.RF)
mlr::performance(pred.RF, auc)
plotThreshVsPerf(predDF.RF)

##compare learners
comparisondf = generateThreshVsPerfData(list(rf = pred.RF, xgb = pred.XG,svm=pred.SVM), measures = list(fpr, tpr))
plotROCCurves(comparisondf)
qplot(x = fpr, y = tpr, color = learner, data = comparisondf$data, geom = "path")
