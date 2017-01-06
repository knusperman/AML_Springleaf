classif.lrn.RPART = makeLearner("classif.rpart",predict.type="prob")
classif.lrn.RPART = setHyperPars(classif.lrn.RPART, par.vals=list(eval_metric="auc"))
############################################################################################################
###################################### PARAMETER OPTIMIZATION###############################################
############################################################################################################
doParamOptimizationRPART <- function(learner,task){
  
  cluster = makeCluster(parallel::detectCores(), type="SOCK")
  set_cv <- makeResampleDesc("CV",iters = 3L)
  
  gs <- makeParamSet(
    makeIntegerParam("minsplit",lower = 10, upper = 50),
    makeIntegerParam("minbucket", lower = 5, upper = 50),
    makeNumericParam("cp", lower = 0.0001, upper = 0.001)
  )
  
  gscontrol <- makeTuneControlRandom(maxit=20)
  #hypertune the parameters
  registerDoSNOW(cluster)
  stune <- tuneParams(learner = learner, resampling = set_cv, task = task, par.set = gs, control = gscontrol, measures = acc)
  stopCluster(cluster)
  stune
  #for minsplit in 10:50, minbucket5:50, cp 0.001:0.001 [Tune] Result: minsplit=23; minbucket=8; cp=0.000991 : acc.test.mean=0.772
}

# # 5) set the optimal hyperparameter
#parameteroptimization = doParamOptimizationRPART(classif.lrn.RPART, classif.task)
#classif.lrn.RPART = setHyperPars(classif.lrn.RPART, par.vals=parameteroptimization$x)

############################################################################################################
################################# Training #################################################################
############################################################################################################
# if no parameter optimization is performed and a certain config should run:
classif.lrn.RPART$par.set
classif.lrn.RPART = setHyperPars(classif.lrn.RPART, par.vals = list(eval_metric="auc",minsplit=48,minbucket=8,cp=0.000763))
getHyperPars(classif.lrn.XG)

mod.RPART = train(classif.lrn.RPART, classif.task,subset=train.set)
############################################################################################################
################################# Prediction################################################################
############################################################################################################
pred.RPART = predict(mod.RPART, task=classif.task, subset=test.set)
mlr::performance(pred.RPART,auc)

rpart.plot(t.rpart2$learner.model, fallen.leaves=FALSE)
