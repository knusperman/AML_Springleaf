#####################
# grid searches
#DONE on MCs mbp
xgtune1 <- doParamTuningXG(classif.task, roundsvector = c(10,20,50,100), etavector = c(0.15), maxdepthvector = c(5,10,15),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune1,"models/imputed/xgtune1.rds")
#results:
# nrounds = 50, eta = 0.15, max_depth = 5, colsample = .5, subsample = .8

#next: fix nrounds to 50 and tune eta and max_depth further
xgtune2 <- doParamTuningXG(classif.task, roundsvector = c(50), etavector = c(0.1,0.15,0.2), maxdepthvector = c(4,5,6),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune2,"models/imputed/xgtune2.rds")
#results:
# nrounds = 50, eta = 0.2, max_depth = 4, colsample = .5, subsample = .8
# rounds changed! now eta goes up to 0.2 (?), maxdepth down to miniumum value of 4.

#next: vary eta further and max_depth further . 
xgtune3 <- doParamTuningXG(classif.task, roundsvector = c(50), etavector = c(0.15,0.25), maxdepthvector = c(3,4,5),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune3,"models/imputed/xgtune3.rds")
# results:
# nrounds = 50, eta = 0.15, max_depth = 4, colsample =.5, subsample = .8
# maxdepth stays at 4, eta goes down to 0.15.

#next: change colsample and subsample
xgtune4 <- doParamTuningXG(classif.task, roundsvector = c(50), etavector = c(0.15), maxdepthvector = c(4),colsamplevector = c(0.5,0.6,0.7), subsamplevector = c(0.6,0.7,0.8))
saveRDS(xgtune4,"models/imputed/xgtune4.rds")
#results:
# nrounds = 50, 0.15, 4, colsample_bytree = .5, subsample = .8
# colsample /subsample stays at fixed value

#fits on sample data are found at the end of the tuning plan.

######################
# another idea: try out high number of rounds and see what happends to eta
xgtune_high1 <- doParamTuningXG(classif.task, roundsvector =c(100), etavector=c(0.1,0.15,0.2), maxdepthvector = c(6),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high1,"models/imputed/xgtune_high1.rds")
#results
# nrounds = 100, eta = 0.1, max_depth = 6, colsample = .5, subsample = .8
# eta at minimum

# next: try more granular values for eta and check maxdepth
xgtune_high2 <- doParamTuningXG(classif.task, roundsvector =c(100), etavector=c(0.05,0.075,0.1), maxdepthvector = c(5,6,7),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high2,"models/imputed/xgtune_high2.rds")
#results
# nrounds = 100, eta = 0.075, max_depth = 7, colsample_bytree = .5, subsample = .8
# eta goes down a bit, but not to minium. maxdepth goes up to 7.
# try out varying maxdepth on sample!

#next: try out how 200 trees perform
xgtune_high3 <- doParamTuningXG(classif.task, roundsvector =c(200), etavector=c(0.05,0.075), maxdepthvector = c(5,6,7),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high3,"models/imputed/xgtune_high3.rds")
#results
# nrounds = 200, eta = 0.05, max_depth = 5, colsample: .5, subsample =.8
# eta at minimum supplied, maxdepth as well. 
# try out varying eta and maxdepth on sample!

#############################################################################################################################
#############################################################################################################################

xgtune1 <- readRDS("models/imputed/xgtune1.rds")#on mcs mbp
xgtune2 <- readRDS("models/imputed/xgtune2.rds")
xgtune3 <- readRDS("models/imputed/xgtune3.rds")
xgtune4 <- readRDS("models/imputed/xgtune4.rds")
xgtune2$x$eval_metric ="auc"
xgtune3$x$eval_metric ="auc"
xgtune4$x$eval_metric ="auc"

xg2 <- buildXG(classif.task,train.set,test.set,xgtune3$x) #auc = 0.7613
xg3 <- buildXG(classif.task,train.set,test.set,xgtune3$x) #auc = 0.7614
xg4 <- buildXG(classif.task,train.set,test.set,xgtune4$x) #auc = 0.7645

xgtune_high1 <- readRDS("models/imputed/xgtune_high1.rds")
xgtune_high2 <- readRDS("models/imputed/xgtune_high2.rds")
xgtune_high3 <- readRDS("models/imputed/xgtune_high3.rds")
xgtune_high1$x$eval_metric ="auc"
xgtune_high2$x$eval_metric ="auc"
xgtune_high3$x$eval_metric ="auc"
xghigh1 <- buildXG(classif.task,train.set,test.set,xgtune_high1$x)#auc = 0.7665
xghigh2 <- buildXG(classif.task,train.set,test.set,xgtune_high2$x)#auc = 0.7630
xghigh3 <- buildXG(classif.task,train.set,test.set,xgtune_high3$x)#auc = 0.7670



xgtune4rounds<- xgtune_high3$x
xgtune4rounds$nrounds = 180
xg4tune4roundsfit =  buildXG(classif.task,train.set,test.set,xgtune4rounds) #auc = 0.7701
xgtune4rounds$nrounds = 160
xg4tune4roundsfit =  buildXG(classif.task,train.set,test.set,xgtune4rounds) #auc = 0.7

