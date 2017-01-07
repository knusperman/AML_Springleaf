######################################################
# 1st check mtry with ntree = 300 
#rf300_30 #done by MC by param_tune
# rf300_40 #done by MC by param_tune
ntree = floor(ntree/parallel::detectCores())

time = Sys.time()
rf300_40 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=40))#todo by MC 
diff = Sys.time() - time
saveRDS(rf300_40, "data/prediction/rf300_40.rds")
saveRDS(diff, "data/prediction/rf300_40_time.rds")


time = Sys.time()
rf300_50 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=50))#todo by MC 
diff = Sys.time() - time
saveRDS(rf300_50, "data/prediction/rf300_50.rds")
saveRDS(diff, "data/prediction/rf300_50_time.rds")



time = Sys.time()
rf300_60 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=60))#todo by MC ::: best solution among the four
diff = Sys.time() - time
saveRDS(rf300_60, "data/prediction/rf300_60.rds")
saveRDS(diff, "data/prediction/rf300_60_time.rds")



time = Sys.time()
rf_300_70 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=70))
diff = Sys.time() - time
saveRDS(rf300_70, "data/prediction/rf300_70.rds")
saveRDS(diff, "data/prediction/rf300_70_time.rds")