############################################################################################################
###################################### BUILD FUNCTIONS #####################################################
############################################################################################################
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
  if(length(pars)){
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
  #now combine the trees. take the model spec from the first tree. 
  #the number of trees will not be adjusted in the model description, but in its learner.model attribute!
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
  if(length(pars)){
    classif.lrn.RPART = setHyperPars(classif.lrn.RPART, par.vals=pars)
  }else{
    classif.lrn.RPART = setHyperPars(classif.lrn.RPART)
  }
  mod.RPART  = train(classif.lrn.RPART, task, subset = train)
  pred.RPART  = predict(mod.RPART, task = task, subset = test)
  auc <- mlr::performance(pred.RPART, auc)
  
  list(model=mod.RPART,predictions=pred.RPART,auc=auc)
}
############################################################################################################
###################################### PARAMETER OPTIMIZATION###############################################
############################################################################################################
doParamOptimizationRPART <- function(task,minsplitvector,minbucketvector,cpvector){
  classif.lrn.RPART = makeLearner("classif.rpart", predict.type = "prob", fix.factors.prediction = TRUE)
  parallelStartSocket(2)
  
  ps <- makeParamSet(
    makeDiscreteParam("minsplit",values = minsplitvector),
    makeDiscreteParam("minbucket", values = minbucketvector),
    makeDiscreteParam("cp", values= cpvector )
  )
  rdesc <- makeResampleDesc("CV",iters = 3L)
  ctrl <- makeTuneControlGrid()
  #hypertune the parameters
  res <- tuneParams(learner = classif.lrn.RPART, resampling = rdesc, task = task, par.set = ps, control = ctrl)
  parallelStop()
  res
  #for minsplit in 10:50, minbucket5:50, cp 0.001:0.001 [Tune] Result: minsplit=23; minbucket=8; cp=0.000991 : acc.test.mean=0.772
}
doParamOptimizationRF <- function(task,ntreevector,mtryvector){
  parallelStartSocket(2)
  classif.lrn.RF = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  ps <- makeParamSet(
    makeDiscreteParam("ntree",values = ntreevector),
    makeDiscreteParam("mtry", values = mtryvector)
  )
  rdesc = makeResampleDesc("CV", iters = 2L)
  ctrl =  makeTuneControlGrid()
  res = tuneParams(classif.lrn.RF, task = task, resampling = rdesc, par.set = ps, control = ctrl)
  parallelStop()
  res
}
doParamOptimizationXG = function(task,roundsvector,etavector,maxdepthvector,colsamplevector,subsamplevector){
  classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
  parallelStartSocket(2)
  ps = makeParamSet(
    makeDiscreteParam("nrounds", values=roundsvector),
    makeDiscreteParam("eta", values=etavector),
    makeDiscreteParam("max_depth", values=maxdepthvector),
    makeDiscreteParam("colsample_bytree", values=colsamplevector),
    makeDiscreteParam("subsample", values=subsamplevector)
  )
  rdesc = makeResampleDesc("CV", iters = 3L) 
  ctrl =  makeTuneControlGrid() 
  res = tuneParams(classif.lrn.XG, task = task, resampling = rdesc, par.set = ps, control = ctrl)
  parallelStop()
  res
}
