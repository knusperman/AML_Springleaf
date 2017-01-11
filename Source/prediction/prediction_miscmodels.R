
source("models/prediction_functions.R")
source("models/prediction_initialize.R") #45k sample with 90:10 train:test split

############ models on 45k sample 

xg_default = buildXG(classif_task,train.set,test.set)
rpart_default = buildRPART(classif_task,train.set,test.set)
rf_default = buildRF(classif_task,train.set,test.set,pars = list(ntree=500/parallel::detectCores()))

## tuned models (see tuning plan)
rpart_tuned = buildRPART(classif_task,train.set,test.set,pars = list(minsplit = 15, minbucket = 30, cp = 0.0015))
rf_tuned = buildRPART(classif_task,train.set,test.set,pars = list(ntree = 500/parallel::detectCores(), mtry = 40, nodesize = 10))
xg_tuned = buildXG(classif_task,train.set,test.set,pars=list(nrounds=1000, eta=0.02, max_depth=14, colsample_bytree=0.6, subsample=0.8))

############ scaled 45k sample
mydata_scaled <- mydata
mydata_scaled[, which(sapply(mydata, class) =="numeric")] <- scale(mydata[, which(sapply(mydata, class) =="numeric")])
classif_task_scaled = makeClassifTask(id = "mtc", data = mydata_scaled, target = "target", positive="1")

xg_scaled = buildXG(classif_task_scaled,train.set,test.set, readRDS("parametersettings/xg_tunedparams.rds"))

rfparams = readRDS("parametersettings/rf_tunedparams.rds")
rfparams$ntree = rfparams$ntree/4 #use all cores (4)
rf_scaled =  buildRF(classif_task_scaled,train.set,test.set, rfparams)

rpart_scaled =  buildRPART(classif_task_scaled,train.set,test.set, readRDS("parametersettings/rpart_tunedparams.rds"))

############ tuned models on full TRAIN set (145k) with 90:10 test split
fulldata = buildDataSet(c(1,2,3),withExtraNumerics = TRUE,convertToFactor = TRUE)

classif_task_full = makeClassifTask(id = "mtc", data = fulldata, target = "target", positive="1")
set.seed(1234)
n = getTaskSize(classif_task_full) #size of data
train_set_full = sample(n, size = n*0.9)
test_set_full = 1:n
test_set_full <- test_set_full[-which(test_set_full %in% train_set_full)]

rpart_tuned_full <- buildRPART(classif_task_full,train_set_full,test_set_full,pars =readRDS("parametersettings/rpart_tunedparams.rds"))

xg_tuned_full <- buildXG(classif_task_full, train_set_full,test_set_full,pars=readRDS("parametersettings/xg_tunedparams.rds"))

rfparams = readRDS("parametersettings/rf_tunedparams.rds")
rfparams$ntree = rfparams$ntree/2 #as it takes roughly 10GB for each core.. better do just two tasks instead of #core tasks.
rf_tuned_full <- buildRF(classif_task_full, train_set_full,test_set_full,pars=rfparams,2)

## now build model on 145k samples and include test data



