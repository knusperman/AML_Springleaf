#needs prediction_exec code to run
classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.XG$par.set
classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals = list(nrounds=25,eval_metric="auc"))
getHyperPars(classif.lrn.XG)

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
#res
# # 5) set the optimal hyperparameter
classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals = res$x)

#cv = crossval(lrn, trainTask, iter = 3, measures = SQWK, show.info = TRUE)
parallelStop()

###

mod.XG  = train(classif.lrn.XG, classif.task, subset = train.set)
pred.XG  = predict(mod.XG, task = classif.task, subset = test.set)
mlr::performance(pred.XG, auc)
