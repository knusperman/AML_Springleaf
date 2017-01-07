#############################################################################
#ROC Analysis
#############################################################################
#get pred.XX objects to compare first


####compare the default
readRDS("models/imputed/xgboost_default.rds")->xgdefault
readRDS("models/imputed/rpart_default.rds")->rpartdefault
readRDS("models/imputed/rf_default.rds")->rfdefault
png(filename = "fig/ROC_default.png",width = 1000,height = 800)
getPlotAUC(list(XGBoost = xgdefault$predictions,RPART=rpartdefault$predictions, RF=rfdefault$predictions))
dev.off()

####
readRDS("models/imputed/rpart_default.rds")->rpartdefault
readRDS("models/imputed/rpart-tuning.rds")->rparttuning
tunedrpart <- buildRPART(mydata,classif.task,train.set,test.set,pars = rparttuning$x)
getPlotAUC(list(default = rpartdefault$predictions, tuned =  tunedrpart$predictions))
## example code for three facts to see threshold
#predDF.RF = generateThreshVsPerfData(pred[[1]], measures = list(fpr, tpr, mmce))
#plotROCCurves(predDF.RF)
#mlr::performance(pred.RF, auc)
#plotThreshVsPerf(predDF.RF)

##compare learners

