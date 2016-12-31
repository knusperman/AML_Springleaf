if (!"xgboost" %in% installed.packages()) remove..packages("xgboost")
if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"e1071" %in% installed.packages()) install.packages("e1071")
if (!"devtools" %in% installed.packages()) install.packages("devtools")
if (!"ROCR" %in% installed.packages()) install.packages("ROCR")
if(!"parallelMap" %in% installed.packages()) install.packages("parallelMap")
library(mlr)
library(parallelMap)
library(devtools)
install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org") #necessary to run with mlr
library(xgboost)
library(e1071)
library(ROCR)


classif.task = makeClassifTask(id = "mtc", data = mydata, target = "target", positive="1")

n = getTaskSize(classif.task) #size of data
train.set = sample(n, size = n*0.8)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
#Occasionally, factor features may cause problems when fewer levels are present in the test data set than in the training data. 
#By setting fix.factors.prediction = TRUE these are avoided by adding a factor level for missing data in the test data set.

#get default hyper parameters
classif.lrn.RF$par.set
#manipulate hyper parameters
classif.lrn.RF= setHyperPars(classif.lrn.RF, ntree = 200)
#get changed hyper parameters
getHyperPars(classif.lrn.RF)
#prob or response for classification

mod.RF  = train(classif.lrn.RF, classif.task, subset = train.set)
pred.RF  = predict(mod.RF, task = classif.task, subset = test.set)
mlr::performance(pred.RF, auc)





#############################################################################
#ROC Analysis
#############################################################################

predDF.RF = generateThreshVsPerfData(pred.RF, measures = list(fpr, tpr, mmce))
plotROCCurves(predDF.RF)
mlr::performance(pred.RF, auc)
plotThreshVsPerf(predDF.RF)

##compare learners
comparisondf = generateThreshVsPerfData(list(rf = pred.RF, xgb = pred.XG,svm=pred.SVM), measures = list(fpr, tpr))
plotROCCurves(comparisondf)
qplot(x = fpr, y = tpr, color = learner, data = comparisondf$data, geom = "path")
