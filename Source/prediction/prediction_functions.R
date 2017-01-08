
buildDataSet <- function(numericparts){
  data_numeric = buildNumericData(numericparts) #imputed sample for training just in part 3
  
  data_factors = as.data.frame(readRDS("data/final/factorAttributes_FINAL.rds"))[rownames(data_numeric),] #full train records. no NAs b/c treated as level
  data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(data_numeric),]
  data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(data_numeric),] #f
  data_boolean = as.data.frame(readRDS("data/final/booleanAttributes_FINAL.rds"))[rownames(data_numeric),]
  data_target  = as.data.frame(readRDS("data/target.rds"))[rownames(data_numeric),] #f #full train records
  
  mydata <- cbind(data_numeric,data_factors,data_strings,data_dates,data_boolean, data_target)
  colnames(mydata)[ncol(mydata)]="target"
  mydata$target <- as.factor(mydata$target)
  #cleaning the environment
  remove(data_numeric, data_factors,data_strings,data_dates,data_boolean,data_target)
  mydata
}

############################################################################################################
###################################### BUILD FUNCTIONS #####################################################
############################################################################################################
buildXG <- function(task,train,test,pars=list()){
  
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
buildRF <- function(task,train,test,pars=list()){

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
buildRPART <- function(task,train,test,pars=list()){
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
buildSVM <- function(task,train,test,pars=list()){
  classif.lrn.SVM = makeLearner("classif.svm", predict.type = "prob", fix.factors.prediction = TRUE)
  mod.SVM = train(classif.lrn.SVM, task, subset = train)
  pred.SVM = predict(mod.SVM, task = task, subset = test)
  auc <- mlr::performance(pred.SVM, auc) #0.7218
  list(model=mod.SVM,predictions=pred.SVM,auc=auc)
}
buildKNN <- function(task,train,test,pars=list()){
  classif.lrn.kknn = makeLearner("classif.kknn", predict.type = "prob", fix.factors.prediction = TRUE)
  classif.lrn.kknn$par.set
  mod.kknn = train(classif.lrn.kknn, task, subset = train)
  pred.kknn  = predict(mod.kknn, task = task, subset = test)
  auc = mlr::performance(pred.kknn,auc) #0.6187
  list(model=mod.kknn, predictions = pred.kknn, auc=auc)
}
buildNNET <- function(task,train,test,pars=list()){

  classif.lrn.nnet = makeLearner("classif.avNNet", predict.type = "prob", fix.factors.prediction = TRUE)
  classif.lrn.nnet = setHyperPars(classif.lrn.nnet, MaxNWts = 35011) #max weights in sample
  classif.lrn.nnet = setHyperPars(classif.lrn.nnet, size = 10) # cannot use size = 100 apparently (error message cannot allocate vector of size <some> kbs)
  mod.nnet = train(classif.lrn.nnet, task)
  pred.nnet = predict(mod.nnet, task=task,subset=test)
  auc = mlr::performance(pred.nnet,auc) #0.5
  list(model=mod.nnet, predictions = pred.nnet, auc=auc)
  
}
############################################################################################################
###################################### PARAMETER Tuning###############################################
############################################################################################################
doParamTuningRPART <- function(task,minsplitvector,minbucketvector,cpvector){
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
doParamTuningRF <- function(task,ntreevector,mtryvector){
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
doParamTuningXG = function(task,roundsvector,etavector,maxdepthvector,colsamplevector,subsamplevector){
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
doParamRandomTuningRPART <- function(task){
  classif.lrn.RPART = makeLearner("classif.rpart", predict.type = "prob", fix.factors.prediction = TRUE)
  parallelStartSocket(2)
  
  ps <- makeParamSet(
    makeIntegerParam("minsplit",lower = 10, upper =50),
    makeIntegerParam("minbucket", lower = 5, upper =50),
    makeNumericParam("cp", lower =0.0001 , upper = 0.001 )
  )
  rdesc <- makeResampleDesc("CV",iters = 3L)
  ctrl <- makeTuneControlRandom(maxit = 50)
  #hypertune the parameters
  res <- tuneParams(learner = classif.lrn.RPART, resampling = rdesc, task = task, par.set = ps, control = ctrl)
  parallelStop()
  res
  # [Tune] Result: minsplit=23; minbucket=8; cp=0.000991 : acc.test.mean=0.772
}
doParamRandomTuningXG = function(task){
  classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
  parallelStartSocket(2)
  ps = makeParamSet(
    makeDiscreteParam("nrounds", values=15),
    makeNumericParam("eta", lower = 0.1, upper = 0.3),
    makeIntegerParam("max_depth", lower = 4, upper = 12),
    makeIntegerParam("colsample_bytree", lower = 0.3, upper = 0.9),
    makeIntegerParam("subsample", lower = 0.3, upper = 0.9)
  )
  rdesc = makeResampleDesc("CV", iters = 3L) 
  ctrl =  makeTuneControlRandom(maxit = 50) 
  res = tuneParams(classif.lrn.XG, task = task, resampling = rdesc, par.set = ps, control = ctrl)
  parallelStop()
  res
  # [Tune ] Result: eta=0.172; max_depth=4; colsample_bytree=0.51; subsample=0.838 
}