#1st check mtry with ntree = 300
#rf300_30 #done by MC by param_tune
# rf300_40 #done by MC by param_tune
rf300_50 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=50))#todo by MC 
rf300_60 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=60))#todo by MC ::: best solution among the four
#rf_300_70 = buildRF(mydata,classif.task,train.set,test.set,list(ntree=75,mtry=70)) MH 12:40