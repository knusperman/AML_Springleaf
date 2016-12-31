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


classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.SVM = makeLearner("classif.svm", predict.type = "prob", fix.factors.prediction = TRUE)

#get default hyper parameters
classif.lrn.RF$par.set
classif.lrn.XG$par.set
#manipulate hyper parameters
classif.lrn.RF= setHyperPars(classif.lrn.RF, ntree = 200)
classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals = list(nrounds=15,eval_metric="auc"))
#get changed hyper parameters
getHyperPars(classif.lrn.RF)
getHyperPars(classif.lrn.XG)
#prob or response for classification
#Occasionally, factor features may cause problems when fewer levels are present in the test data set than in the training data. 
#By setting fix.factors.prediction = TRUE these are avoided by adding a factor level for missing data in the test data set.

###https://www.kaggle.com/casalicchio/prudential-life-insurance-assessment/use-the-mlr-package-scores-0-649/run/139876
library(parallelMap)
parallelStartSocket(2)
#parallelExport("trainLearner.regr.xgboost", "predictLearner.regr.xgboost" , "makeRLearner.regr.xgboost")
# # 1) Define the set of parameters you want to tune (here 'eta')
ps = makeParamSet(
  makeNumericParam("eta", lower = 0.1, upper = 0.3),
  makeNumericParam("max_depth", lower = 2, upper = 14, trafo=function(x) round(x,0)),
  makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
  makeNumericParam("subsample", lower = 1, upper = 2, trafo = function(x) x/2)
)
# # 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 3L)
# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 10)
# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK
res = tuneParams(classif.lrn.XG, task = classif.task, resampling = rdesc, par.set = ps, control = ctrl)
res
# # 5) set the optimal hyperparameter
classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals = res$x)

#cv = crossval(lrn, trainTask, iter = 3, measures = SQWK, show.info = TRUE)
parallelStop()

###



mod.RF  = train(classif.lrn.RF, classif.task, subset = train.set)
pred.RF  = predict(mod.RF, task = classif.task, subset = test.set)
mlr::performance(pred.RF, auc)

mod.XG  = train(classif.lrn.XG, classif.task, subset = train.set)
pred.XG  = predict(mod.XG, task = classif.task, subset = test.set)
mlr::performance(pred.XG, auc)


pred.SVM = predict(mod.SVM, task = classif.task, subset = test.set)
mod.SVM = train(classif.lrn.SVM, classif.task, subset = train.set)
mlr::performance(pred.SVM, auc)

#full response : pred.RF$data




predDF.RF = generateThreshVsPerfData(pred.RF, measures = list(fpr, tpr, mmce))
plotROCCurves(predDF.RF)
mlr::performance(pred.RF, auc)
plotThreshVsPerf(predDF.RF)

##compare learners
comparisondf = generateThreshVsPerfData(list(rf = pred.RF, xgb = pred.XG,svm=pred.SVM), measures = list(fpr, tpr))
plotROCCurves(comparisondf)
qplot(x = fpr, y = tpr, color = learner, data = comparisondf$data, geom = "path")
