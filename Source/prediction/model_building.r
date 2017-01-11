source("models/prediction_initialize.R")

buildXG(classif_task,train.set,test.set)->xgdefault
saveRDS(xgdefault, "models/imputed/xgboost_default.rds")

buildRPART(classif_task,train.set,test.set)->rpartdefault
saveRDS(rpartdefault, "models/imputed/rpart_default.rds")

buildRF(classif_task,train.set,test.set)->rfdefault
saveRDS(rfdefault, "models/imputed/rf_default.rds")

## tuned models (see tuning plan)

rpart_tuned <- readRDS("models/imputed/rpart_tuned.rds")
rf_tuned <- readRDS("models/imputed/rf_500_40_10.rds") # see rf_tuningplan for details
xg_tuned <- readRDS("models/imputed/xgboost_tuned.rds") # see xg_tuningplan for details
# or
# buildRF(classif_task,train.set,test.set, readRDS("models/imputed/rf_tunedparams.rds"))
# buildXG(classif_task,train.set,test.set, readRDS("models/imputed/xg_tunedparams.rds"))
# buildRPART(classif_task,train.set,test.set, readRDS("models/imputed/rpart_tunedparams.rds"))


#rpart_tuned_full <- buildRPART(classif_task_full,train_set_full,test_set_full,pars =readRDS("models/imputed/rpart_tunedparams.rds"))
rpart_tuned_full <- readRDS("models/imputed/FULL_rpart_tuned.rds")
# xg_tuned_full <- buildXG(classif_task_full, train_set_full,test_set_full,pars=readRDS("models/imputed/xg_tunedparams.rds"))
xg_tuned_full <- readRDS( "models/imputed/FULL_xg_tuned.rds")

#rfparams = readRDS("models/imputed/rf_tunedparams.rds")
#rfparams$ntree = rfparams$ntree/2 #as it takes roughly 10GB for each core.. better do just two tasks instead of #core tasks.
#rf_tuned_full <- buildRF(classif_task_full, train_set_full,test_set_full,pars=rfparams,2)
rf_tuned_full <- readRDS( "models/imputed/FULL_rf_tuned.rds")


#### scaling
mydata_scaled <- mydata
mydata_scaled[, which(sapply(mydata, class) =="numeric")] <- scale(mydata[, which(sapply(mydata, class) =="numeric")])
classif_task_scaled = makeClassifTask(id = "mtc", data = mydata_scaled, target = "target", positive="1")

#xg_scaled = buildXG(classif_task_scaled,train.set,test.set, readRDS("models/imputed/xg_tunedparams.rds"))
#rfparams = readRDS("models/imputed/rf_tunedparams.rds")
#rfparams$ntree = rfparams$ntree/4 #use all cores (4)
#rf_scaled =  buildRF(classif_task_scaled,train.set,test.set, rfparams)
#rpart_scaled =  buildRPART(classif_task_scaled,train.set,test.set, readRDS("models/imputed/rpart_tunedparams.rds"))


## now build model on 145k samples and include test data

mydata_fullsample <- buildCombinedDataSet() 
trainrows <- mydata_fullsample$trainrownames
testrows <- mydata_fullsample$testrownames
trainindices <- 1:145231 
testindices <- 145232:290463 #different to rownames, which are strings, but can also be used to subset. But not for mlR task
mydata_fullsample <- mydata_fullsample$data
###MLR Setup
for(i in which(sapply(mydata_fullsample, class) %in%c("character","logical"))){
  mydata_fullsample[,i] = as.factor(mydata_fullsample[,i])
}
classif_task_full = makeClassifTask(id = "mtcfull", data = mydata_fullsample, target = "target", positive="1")
set.seed(1234)
xg_tuned_full <- buildXG(classif_task_full, trainindices, testindices[1:10],pars = list(nrounds=3))



