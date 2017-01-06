
classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.RF = setHyperPars(classif.lrn.RF, par.vals=list(eval_metric="auc"))
############################
cores = parallel::detectCores()
cluster = makeCluster(cores, type="SOCK")
############################
getHyperPars(classif.lrn.RF) #get changed hyper parameters
############################################################################################################
###################################### PARAMETER OPTIMIZATION###############################################
############################################################################################################
doparameteroptimizationRF <- function(learner, task){
  
rf_param <- makeParamSet(
  makeDiscreteParam("ntree",values = c(300,400,500,600)),
  makeDiscreteParam("mtry", values = c(30,40,50,60))
 )
rancontrol <- makeTuneControlRandom(maxit = 5L)
set_cv <- makeResampleDesc("CV",iters = 2L)

registerDoSNOW(cluster)
stune <- tuneParams(learner = learner, resampling = set_cv, task = task, par.set = rf_param, control = rancontrol)
stopCluster(cluster)
stune
}

# # 5) set the optimal hyperparameter
# parameteroptimization = doParamOptimizationRF(classif.lrn.RF, classif.task)
# classif.lrn.RF = setHyperPars(classif.lrn.RF, par.vals = parameteroptimization$x)

############################################################################################################
################################# Training #################################################################
############################################################################################################
# if no parameter optimization is performed and a certain config should run:
classif.lrn.RF$par.set
classif.lrn.RF= setHyperPars(classif.lrn.RF,eval_metric="auc", ntree = 150,mtry=37,nodesize=10) #forest size = ntree * cores
getHyperPars(classif.lrn.RF)

registerDoSNOW(cluster)
mods = foreach(i=1:cores,.inorder=FALSE,.packages="mlr") %dopar% {
  train(classif.lrn.RF, classif.task, subset = train.set)
}
stopCluster(cluster)
mod.RF = mods[[1]]
for(i in 2:cores){
  mod.RF$learner.model = combine(mod.RF$learner.model,mods[[i]]$learner.model)
}
#remove(mods)
############################################################################################################
################################# Prediction################################################################
############################################################################################################
pred.RF  = predict(mod.RF, task = classif.task, subset = test.set)
mlr::performance(pred.RF, auc)

#see other files for xgb or svm