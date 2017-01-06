buildXG <- function(data,task,train,test,pars=list()){
  
  classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
  if(length(pars)){
    classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals=pars)
  }else{
    classif.lrn.XG = setHyperPars(classif.lrn.XG, par.vals=list(eval_metric="auc"))
  }
  
  mod.XG  = train(classif.lrn.XG, task, subset = train)
  pred.XG  = predict(mod.XG, task = task, subset = test)
  auc <- mlr::performance(pred.XG, auc)
  list(model=mod.XG,predictions=pred.XG,auc=auc)
}
buildRF <- function(data,task,train,test,pars=list()){

  cores = parallel::detectCores()
  treesPerTask = 500/cores   #500 trees is standard
  cluster = makeCluster(cores, type="SOCK")
  
  classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  if(is.null(pars)){
    classif.lrn.RF = setHyperPars(classif.lrn.RF, par.vals=pars)
  }else{
    classif.lrn.RF = setHyperPars(classif.lrn.RF, par.vals=list(ntree= treesPerTask))
  }
  #parallel execution
  registerDoSNOW(cluster)
  mods = foreach(i=1:cores,.inorder=FALSE,.packages="mlr") %dopar% {
    train(classif.lrn.RF, task, subset = train)
  }
  stopCluster(cluster)
  mod.RF = mods[[1]]
  for(i in 2:cores){
    mod.RF$learner.model = combine(mod.RF$learner.model,mods[[i]]$learner.model)
  }
  
  pred.RF  = predict(mod.RF, task = task, subset = test)
  auc <- mlr::performance(pred.RF, auc)
  
  list(model=mod.RF,predictions=pred.RF,auc=auc)
}
buildRPART <- function(data,task,train,test,pars=list()){
  classif.lrn.RPART = makeLearner("classif.rpart", predict.type = "prob", fix.factors.prediction = TRUE)
  if(is.null(pars)){
    classif.lrn.RPART = setHyperPars(classif.lrn.RPART, par.vals=pars)
  }else{
    classif.lrn.RPART = setHyperPars(classif.lrn.RPART)
  }
  mod.RPART  = train(classif.lrn.RPART, task, subset = train)
  pred.RPART  = predict(mod.RPART, task = task, subset = test)
  auc <- mlr::performance(pred.RPART, auc)
  list(model=mod.RPART,predictions=pred.RPART,auc=auc)
}

