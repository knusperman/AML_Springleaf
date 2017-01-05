
classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)

classif.lrn.RF$par.set
classif.lrn.RF= setHyperPars(classif.lrn.RF, ntree = 150) #forest size = ntree * cores

############################
cores = 2
cluster = makeCluster(cores, type="SOCK")
############################
getHyperPars(classif.lrn.RF) #get changed hyper parameters
############################################################################################################
###################################### PARAMETER OPTIMIZATION###############################################
############################################################################################################
doparameteroptimizationRF <- function(learner, task){
  
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
 )
rancontrol <- makeTuneControlRandom(maxit = 5L)
set_cv <- makeResampleDesc("CV",iters = 3L)

registerDoSNOW(cluster)
result <- tuneParams(learner = learner, resampling = set_cv, task = task, par.set = rf_param, control = rancontrol)
stopCluster(cluster)
result
}
############################################################################################################
############################################################################################################
############################################################################################################

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
############################################################################################################
############################################################################################################
pred.RF  = predict(mod.RF, task = classif.task, subset = test.set)
mlr::performance(pred.RF, auc)

#see other files for xgb or svm