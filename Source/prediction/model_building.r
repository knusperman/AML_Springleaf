##default models

buildXG(classif.task,train.set,test.set)->xgdefault
saveRDS(xgdefault, "models/imputed/xgboost_default.rds")

buildRPART(classif.task,train.set,test.set)->rpartdefault
saveRDS(rpartdefault, "models/imputed/rpart_default.rds")

buildRF(classif.task,train.set,test.set)->rfdefault
saveRDS(rfdefault, "models/imputed/rf_default.rds")

## tuned models (see tuning plan)



#getPlotAUC(list(rpartdefault = rpartdefault$predictions, rparttuned = rparttuned$predictions))

