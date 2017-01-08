#####################
# grid searches
#DONE on MCs mbp
xgtune1 <- doParamTuningXG(classif.task, roundsvector = c(10,20,50,100), etavector = c(0.15), maxdepthvector = c(5,10,15),colsamplevector = c(0.5), subsamplevector = c(0.8))
#results:
# nrounds = 50, eta = 0.15, max_depth = 5, colsample = .5, subsample = .8

#next: fix nrounds to 50 and tune eta and max_depth further
#DONE on MHs mbp
xgtune2 <- doParamTuningXG(classif.task, roundsvector = c(50), etavector = c(0.1,0.15,0.2), maxdepthvector = c(4,5,6),colsamplevector = c(0.5), subsamplevector = c(0.8))
#results:
# nrounds = 50, eta = 0.2, max_depth = 4, colsample = .5, subsample = .8

#next: vary eta further and max_depth further. 
#in progress on MHs mbp
xgtune3 <- doParamTuningXG(classif.task, roundsvector = c(50), etavector = c(0.15,0.25), maxdepthvector = c(3,4,5),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune3,"models/imputed/xgtune3.rds")

xgtune4 <- doParamTuningXG(classif.task, roundsvector = c(50), etavector = c(0.15), maxdepthvector = c(4),colsamplevector = c(0.5,0.6,0.7), subsamplevector = c(0.6,0.7,0.8))
saveRDS(xgtune4,"models/imputed/xgtune4.rds")



######################
#### own idea: try out high number of rounds and see what happends to the eta
xgtune_high1 <- doParamTuningXG(classif.task, roundsvector =c(100), etavector=c(0.1,0.15,0.2), maxdepthvector = c(6),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high1,"models/imputed/xgtune_high1.rds")

xgtune_high2 <- doParamTuningXG(classif.task, roundsvector =c(100), etavector=c(0.05,0.075,0.1), maxdepthvector = c(5,6,7),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high2,"models/imputed/xgtune_high2.rds")

xgtune_high3 <- doParamTuningXG(classif.task, roundsvector =c(200), etavector=c(0.05,0.075), maxdepthvector = c(5,6,7),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high3,"models/imputed/xgtune_high3.rds")

xghigh1 <- buildXG(classif.task,train.set,test.set,xgtune_high1$x)
xghigh2 <- buildXG(classif.task,train.set,test.set,xgtune_high2$x)
xghigh3 <- buildXG(classif.task,train.set,test.set,xgtune_high3$x)

xg3 <- buildXG(classif.task,train.set,test.set,xgtune3$x)
xg4<- buildXG(classif.task,train.set,test.set,xgtune4$x)
