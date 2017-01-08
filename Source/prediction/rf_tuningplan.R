######################################################
# 1st check mtry with ntree = 300 
# rf300_30 #done by MC by param_tune
# rf300_40 #done by MC by param_tune
#ntree = floor(ntree/parallel::detectCores())

rf300_40 = buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=40))#done by MH 0.7520
saveRDS(rf300_40, "models/imputed/rf300_40.rds")

rf300_50 = buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=50))#done by MC 0.7520
saveRDS(rf300_50, "models/imputed/rf300_50.rds")

rf300_60 = buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=60))#done by MH 0.7496
saveRDS(rf300_60, "models/imputed/rf300_60.rds")

rf_300_70 = buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=70)) # done by MH 0.7518
saveRDS(rf300_70, "models/imputed/rf300_70.rds")
################################################################################
################################################################################
##### set mtry = 40. ############################################################
################################################################################

##### next tuning param: nodesize
########## fixed: mtry = 40, ntree = 300

#nodesize = 1 default, so we do not need to fit that

rf_300_40_5= buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=40, nodesize=5)) # done by AWS AUC 0.74599
saveRDS(rf_300_40_5, "models/imputed/rf300_70.rds")

rf_300_40_10 = buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=40,nodesize=10)) # done by AWS AUC 0.7525
saveRDS(rf_300_40_10, "models/imputed/rf_300_40_10.rds")

rf_300_40_20 = buildRF(classif.task,train.set,test.set,list(ntree=floor(300/parallel::detectCores()),mtry=40,nodesize=20)) # done AUC 0.7492
saveRDS(rf_300_40_20, "models/imputed/rf_300_40_20.rds")

################################################################################
################################################################################
##### set mtry = 40, nodesize = 20. ############################################
################################################################################

##### next tuning param: ntree
########## fixed: mtry = 40, ntree = 20


rf_400_40_10= buildRF(classif.task,train.set,test.set,list(ntree=floor(400/parallel::detectCores()),mtry=40, nodesize=10)) # done by MH AUC 0.7509
saveRDS(rf_400_40_10, "models/imputed/rf_400_40_10.rds")

rf_500_40_10 = buildRF(classif.task,train.set,test.set,list(ntree=floor(500/parallel::detectCores()),mtry=40,nodesize=10)) # done by MHO AUC 0.7520
saveRDS(rf_500_40_10, "models/imputed/rf_500_40_10.rds")

rf_600_40_10 = buildRF(classif.task,train.set,test.set,list(ntree=floor(600/parallel::detectCores()),mtry=40,nodesize=10)) # in progress MHO AUC 
saveRDS(rf_300_40_20, "models/imputed/rf_600_40_10.rds")

# what happens at default nodesize of 1
rf_700_30_1 = buildRF(classif.task,train.set,test.set,list(ntree=floor(700/parallel::detectCores()),mtry=30,nodesize=1)) # done by MHO AUC 0.7510
saveRDS(rf_300_40_20, "models/imputed/rf_600_40_10.rds")

rf_600_40_1 = buildRF(classif.task,train.set,test.set,list(ntree=floor(600/parallel::detectCores()),mtry=40,nodesize=1)) # done by MHO AUC 0.7499
saveRDS(rf_600_40_1, "models/imputed/rf_600_40_1.rds")
