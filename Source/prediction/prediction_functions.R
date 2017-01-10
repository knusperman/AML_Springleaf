
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
doParamTuningRF <- function(task,ntreevector,mtryvector){ #not used due to computational complexity--> direct fit on train sample and evaluation on test
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
doParamRandomTuningXG = function(task, nrounds, etalow, etahigh, max_depth){
  classif.lrn.XG = makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE)
  parallelStartSocket(2)
  ps = makeParamSet(
    makeDiscreteParam("nrounds", values=nrounds),
    makeNumericParam("eta", lower = etalow, upper = etahigh),
    makeDiscreteParam("max_depth",values = max_depth),
    makeIntegerParam("colsample_bytree", lower = 0.5, upper = 1),
    makeIntegerParam("subsample", lower = 0.5, upper = 1)
  )
  rdesc = makeResampleDesc("CV", iters = 3L) 
  ctrl =  makeTuneControlRandom(maxit = 20) 
  res = tuneParams(classif.lrn.XG, task = task, resampling = rdesc, par.set = ps, control = ctrl)
  parallelStop()
  res
  # [Tune ] Result: eta=0.172; max_depth=4; colsample_bytree=0.51; subsample=0.838 
}



# requires a list of all pred attributes
getPlotAUC = function(pred) {
  comparisondf = generateThreshVsPerfData(pred, measures = list(fpr, tpr))
  p = ggplot(data= comparisondf$data, aes(x=fpr, y=tpr, color=learner))+geom_abline(slope = 1, intercept = 0, size = 2) +geom_path(size = 2)+xlab("False positive rate") + ylab("True positive rate") + theme_bw()+ 
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
    theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), legend.title = element_text(size = 40, face = "bold"))
  
  return(p)
}

buildNumericData <- function(vec){
  data = data.frame()
  for(i in vec){
    p1 =  as.data.frame(readRDS(paste("data/numeric imputations/impsplit",i,"_done1.rds",sep = "")))
    p2 =  as.data.frame(readRDS(paste("data/numeric imputations/impsplit",i,"_done2.rds",sep = "")))
    data = rbind(data,rbind(p1,p2))
  }
  data
}

customXGBoostTune = function(task,train,test,
                             nrounds, etas, max_depths, colsamples, subsamples) {
  grid = expand.grid(nrounds, etas, max_depths, colsamples, subsamples)
  results = list(grid = grid)
  for (i in 1:nrow(grid)) {
    params = list(nrounds = grid[i,1], 
                  eta = grid[i,2],
                  max_depth = grid[i,3],
                  colsample_bytree = grid[i,4],
                  subsample = grid[i,5])
    res = buildXG(task, train, test, params)
    results[[(i+1)]] = res
  }
  return(results)
}

# helper function to plot a heatmap to visualize the tuning results
# requires data that has at least an x, a y and an auc column
# x and y can be either "nrounds", "eta", "max_depth", "colsample" or "subsample"
# takes the highest value for every combination of x and y
plotHeatMap = function(data, x, y, xlab, ylab) {
  combos = expand.grid(unique(x), unique(y))
  plotData = as.data.frame(cbind(x = combos[,1], y = combos[,2], auc = numeric(nrow(combos))))
  for (i in nrow(combos)) {
    plotData[i,3] = max(data$auc[which(x == combos[i,1] & y == combos[i,2])])
  }
  p = ggplot(data, aes(x = ordered(x), y = ordered(y), fill = auc)) + geom_tile(color = "white") +
    xlab(xlab) + ylab(ylab) + 
    theme_bw() +
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
    theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), legend.title = element_text(size = 40, face = "bold")) +
    scale_fill_gradient(limits = c(min(data$auc), max(data$auc)), low = "red", high = "green", guide = "colorbar")
  return(p)
}

#check if rows correspond to sample, so that merge with factors etc. is correct:
#s <- readRDS("data/sample.rds") #1 = 1:50000, 2 = 500001:100000, 3 = 100001:145231 in train set
#s[c(1:5,50001:50005,100001:100005)] == rownames(data_numeric)[c(1:5,50001:50005,100001:100005)]

#collist = list("cols_numeric"=colnames(data_numeric),"cols_factors"=colnames(data_factors),"cols_strings" = colnames(data_strings), "cols_dates" = colnames(data_dates))
#saveRDS(collist, "data/collist.rds")

#NAstatistics <- readRDS("data/NAstatistics.rds") #since data has no more information about amout of imputation in a column
#collist <- readRDS("data/collist.rds") #some data for selecting the right columns for predicting
