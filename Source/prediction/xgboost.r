#needs prediction_exec code to run
classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals=list(eval_metric="auc"))
cores = parallel::detectCores()
cluster = makeCluster(cores, type="SOCK")
############################################################################################################
###################################### PARAMETER OPTIMIZATION###############################################
############################################################################################################
doParamOptimizationXG = function(learner, task){
  ps = makeParamSet(
  makeNumericParam("eta", lower = 0.1, upper = 0.3),
  makeDiscreteParam("max_depth", values=c(6,7,8,9)),
  makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
  makeNumericParam("subsample", lower = 1, upper = 2, trafo = function(x) x/2)
  )

rdesc = makeResampleDesc("CV", iters = 3L) # # 2) Use 3-fold Cross-Validation to measure improvements
ctrl =  makeTuneControlRandom(maxit = 20) # # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter

registerDoSNOW(cluster)
stune = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl)# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK
stopCluster(cluster)

stune
}

# # 5) set the optimal hyperparameter
# parameteroptimization =doParamOptimizationXG(classif.lrn.XG, classsif.task)
# classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals = parameteroptimization$x)

############################################################################################################
################################# Training #################################################################
############################################################################################################
# if no parameter optimization is performed and a certain config should run:
classif.lrn.XG$par.set
classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals = list(nrounds=65,eval_metric="auc",eta = 0.128,max_depth=6,colsample_bytree=0.653,subsample=0.816))
getHyperPars(classif.lrn.XG)

mod.XG  = train(classif.lrn.XG, classif.task, subset = train.set)
############################################################################################################
################################# Prediction################################################################
############################################################################################################
pred.XG  = predict(mod.XG, task = classif.task, subset = test.set)
mlr::performance(pred.XG, auc)
