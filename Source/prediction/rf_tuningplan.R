######################################################
# 1st check mtry with ntree = 300 
#rf300_30 #done by MC by param_tune
# rf300_40 #done by MC by param_tune
ntree = floor(ntree/parallel::detectCores())

time = Sys.time()
rf300_40 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=40))#done by MH 0.7520
diff = Sys.time() - time
saveRDS(rf300_40, "data/prediction/rf300_40.rds")
saveRDS(diff, "data/prediction/rf300_40_time.rds")

time = Sys.time()
rf300_50 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=50))#done by MC 0.7520
diff = Sys.time() - time
saveRDS(rf300_50, "data/prediction/rf300_50.rds")
saveRDS(diff, "data/prediction/rf300_50_time.rds")

time = Sys.time()
rf300_60 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=60))#done by MH 0.7496
diff = Sys.time() - time
saveRDS(rf300_60, "data/prediction/rf300_60.rds")
saveRDS(diff, "data/prediction/rf300_60_time.rds")

time = Sys.time()
rf_300_70 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=70)) # done by MH 0.7518
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")
################################################################################
################################################################################
##### set mtry = 40. ############################################################
################################################################################

##### next tuning param: nodesize
########## fixed: mtry = 40, ntree = 30

#nodesize = 1 default, so we do not need to fit that

rf_300_40_5= buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=70, nodesize=5)) # done by MH 0.7518
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")

rf_300_40_10 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=70,nodesize=10)) # done by MH 0.7518
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")

rf_300_40_20 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=70,nodesize=20)) # done by MH 0.7518
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")



