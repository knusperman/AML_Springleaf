######################################################
# 1st check mtry with ntree = 300 
#rf300_30 #done by MC by param_tune
# rf300_40 #done by MC by param_tune
ntree = floor(ntree/parallel::detectCores())

time = Sys.time()
rf300_40 = buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=40))#done by MH 0.7520
diff = Sys.time() - time
saveRDS(rf300_40, "data/prediction/rf300_40.rds")
saveRDS(diff, "data/prediction/rf300_40_time.rds")

time = Sys.time()
rf300_50 = buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=50))#done by MC 0.7520
diff = Sys.time() - time
saveRDS(rf300_50, "data/prediction/rf300_50.rds")
saveRDS(diff, "data/prediction/rf300_50_time.rds")

time = Sys.time()
rf300_60 = buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=60))#done by MH 0.7496
diff = Sys.time() - time
saveRDS(rf300_60, "data/prediction/rf300_60.rds")
saveRDS(diff, "data/prediction/rf300_60_time.rds")

time = Sys.time()
rf_300_70 = buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=70)) # done by MH 0.7518
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")
################################################################################
################################################################################
##### set mtry = 40. ############################################################
################################################################################

##### next tuning param: nodesize
########## fixed: mtry = 40, ntree = 300

#nodesize = 1 default, so we do not need to fit that

rf_300_40_5= buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=40, nodesize=5)) # done by AWS AUC 0.74599
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")

rf_300_40_10 = buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=40,nodesize=10)) # done by AWS AUC 0.7525
diff = Sys.time() - time
saveRDS(rf_300_40_10, "data/prediction/rf_300_40_10.aws")

rf_300_40_20 = buildRF(classif.task,train.set,test.set,list(ntree=75,mtry=40,nodesize=20)) # done AUC 0.7492
diff = Sys.time() - time
saveRDS(rf_300_40_20, "models/imputed/rf_300_40_20.rds")



################################################################################
################################################################################
##### set mtry = 40, nodesize = 20. ############################################
################################################################################

##### next tuning param: ntree
########## fixed: mtry = 40, ntree = 20


rf_400_40_10= buildRF(classif.task,train.set,test.set,list(ntree=100,mtry=40, nodesize=10)) # done by MH 
saveRDS(rf_400_40_10, "data/prediction/rf_400_40_10.rds")

rf_500_40_10 = buildRF(classif.task,train.set,test.set,list(ntree=125,mtry=40,nodesize=10)) # done by MHO
saveRDS(rf_500_40_10, "data/prediction/rf_500_40_10.rds")

rf_600_40_10 = buildRF(classif.task,train.set,test.set,list(ntree=150,mtry=40,nodesize=10)) # done AWS
saveRDS(rf_300_40_20, "models/imputed/rf_600_40_10.rds")


#### # todo
rf_600_40_1 = buildRF(classif.task,train.set,test.set,list(ntree=150,mtry=40,nodesize=1)) # done by MHO
saveRDS(rf_600_40_1, "data/prediction/rf_600_40_1.rds")
