buildXG(mydata,classif.task,train.set,test.set)->xgdefault
saveRDS(xgdefault, "models/imputed/xgboost_default.rds")
remove(xgdefault)
buildRPART(mydata,classif.task,train.set,test.set)->rpartdefault
saveRDS(rpartdefault, "models/imputed/rpart_default.rds")
remove(rpartdefault)
buildRF(mydata,classif.task,train.set,test.set)->rfdefault
saveRDS(rpartdefault, "models/imputed/rf_default.rds")
remove(rfdefault)

rpart_tune = doParamOptimizationRPART(classif.task,minsplitvector = c(10,15,20,25,30,35),minbucketvector = c(10,15,20,25,30,35),cpvector = c(0.0001,0.0005,0.0010,0.0015))
saveRDS(rpart_tune, "models/imputed/rpart_tune.rds")
remove(rpart_tune)
xg_tune = doParamOptimizationXG(classif.task,roundsvector = 15,etavector = c(0.1,0.125,0.15,0.175),maxdepthvector = c(6,7,8,9),subsamplevector = c(0.7,0.8,0.9),colsamplevector = c(0.7,0.8,0,9)) 
saveRDS(xg_tune, "models/imputed/xg_tune.rds")
remove(xg_tune)

#getPlotAUC(list(rpartdefault = rpartdefault$predictions, rparttuned = rparttuned$predictions))

