##default models

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
# or buildRF(classif_task,train.set,test.set, readRDS("models/imputed/rf_tunedparams.rds))
# or buildXG(classif_task,train.set,test.set, readRDS("models/imputed/xg_tunedparams.rds))
# or buildRPART(classif_task,train.set,test.set, readRDS("models/imputed/rpart_tunedparams.rds"))


## now build model on 145k samples


mydata_fullsample <- buildDataSet(c(1,2,3)) # 3 indicates the third data sample part, which is used for training (45k records)

###MLR Setup
classif_task_full = makeClassifTask(id = "mtcfull", data = mydata_fullsample, target = "target", positive="1")
set.seed(1234)
n = getTaskSize(classif_task_full) #size of data
train.set.full = sample(n, size = n*0.9)
test.set.full = 1:n
test.set.full <- test.set.full[-which(test.set.full %in% train.set.full)]

rpart_tuned_full <- buildRPART(classif_task_full,train.set.full,test.set.full,pars =readRDS("models/imputed/rpart_tunedparams.rds"))

