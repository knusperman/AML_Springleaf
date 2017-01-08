#############################################################################
#ROC Analysis
#############################################################################

xg_default <- readRDS("models/imputed/xgboost_default.rds")
rpart_default <- readRDS("models/imputed/rpart_default.rds")
rf_default <- readRDS("models/imputed/rf_default.rds")
rpart_tuned <- readRDS("models/imputed/rpart_tuned.rds")
rf_tuned <- readRDS("models/imputed/rf_500_40_10.rds")
xg_tuned <- readRDS("models/imputed/xgboost_tuned.rds")

####compare the defaults

png(filename = "fig/ROC_default.png",width = 1000,height = 800)
getPlotAUC(list(XGBoost = xg_default$predictions,RPART=rpart_default$predictions, RF=rf_default$predictions))
dev.off()
#### compare rpart default vs tuned
png(filename = "fig/ROC_RPART-comparison.png",width = 1000,height = 800)
getPlotAUC(list(default = rpart_default$predictions, tuned =  rpart_tuned$predictions))
dev.off()
#### compare rf default vs tuned
png(filename = "fig/ROC_RF-comparison.png",width = 1000,height = 800)
getPlotAUC(list(default = rf_default$predictions, tuned =  rf_tuned$predictions))
dev.off()
#### compare xg default vs tuned
png(filename = "fig/ROC_XG-comparison.png",width = 1000,height = 800)
getPlotAUC(list(default = xg_default$predictions, tuned =  xg_tuned$predictions))
dev.off()

### compare the tuned
png(filename = "fig/ROC_tuned.png",width = 1000,height = 800)
getPlotAUC(list(XGBoost = xg_tuned$predictions, RPART =  rpart_tuned$predictions, RF = rf_tuned$predictions))
dev.off()


## example code for three facts to see threshold
#predDF.RF = generateThreshVsPerfData(pred[[1]], measures = list(fpr, tpr, mmce))
#plotROCCurves(predDF.RF)
#mlr::performance(pred.RF, auc)
#plotThreshVsPerf(predDF.RF)

##compare learners

