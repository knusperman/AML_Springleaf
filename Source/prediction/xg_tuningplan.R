source("source/prediction/prediction_initialize.R")
source("source/prediction/prediction_functions.R")
#####################
#prior random search
randomXG1 <- doParamRandomTuningXG(classif_task,nrounds = 1 ,etalow=0.1,etahigh=0.3,max_depth = c(6,7,8,9))
#results:
#$eta = 0.1536837 max_depth = 6 colsample_bytree = 0.6700221 $subsample = 0.9047795
randomXG2 <- doParamRandomTuningXG(classif_task,nrounds = 20 ,etalow=0.1,etahigh=0.3,max_depth = c(6,7,8,9))
#results:
#$eta = 0.1690215 max_depth = 8 colsample_bytree = 0.748096 $subsample = [1] 0.9325617
randomXG3 <- doParamRandomTuningXG(classif_task,nrounds= 20 ,etalow=0.05,etahigh=0.15,max_depth = c(6,7,8,9))
#results:
#$eta = 0.1374282 max_depth = 6 colsample_bytree = 0.857983 $subsample = 0.728183

# grid searches
#first: widespread
xgtune1 <- doParamTuningXG(classif_task, roundsvector = c(10,20,50,100), etavector = c(0.15), maxdepthvector = c(5,10,15),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune1,"models/imputed/xgtune1.rds")
#results:
# nrounds = 50, eta = 0.15, max_depth = 5, colsample = .5, subsample = .8

#next: fix nrounds to 50 and tune eta and max_depth further
xgtune2 <- doParamTuningXG(classif_task, roundsvector = c(50), etavector = c(0.1,0.15,0.2), maxdepthvector = c(4,5,6),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune2,"models/imputed/xgtune2.rds")
#results:
# nrounds = 50, eta = 0.2, max_depth = 4, colsample = .5, subsample = .8
# rounds changed! now eta goes up to 0.2 (?), maxdepth down to miniumum value of 4.

#next: vary eta further and max_depth further . 
xgtune3 <- doParamTuningXG(classif_task, roundsvector = c(50), etavector = c(0.15,0.25), maxdepthvector = c(3,4,5),colsamplevector = c(0.5), subsamplevector = c(0.8))
saveRDS(xgtune3,"models/imputed/xgtune3.rds")
# results:
# nrounds = 50, eta = 0.15, max_depth = 4, colsample =.5, subsample = .8
# maxdepth stays at 4, eta goes down to 0.15.

#next: change colsample and subsample
xgtune4 <- doParamTuningXG(classif_task, roundsvector = c(50), etavector = c(0.15), maxdepthvector = c(4),colsamplevector = c(0.5,0.6,0.7), subsamplevector = c(0.6,0.7,0.8))
saveRDS(xgtune4,"models/imputed/xgtune4.rds")
#results:
# nrounds = 50, 0.15, 4, colsample_bytree = .5, subsample = .8
# colsample /subsample stays at fixed value

#fits on sample data are found at the end of the tuning plan.

######################
# another idea: try out high number of rounds and see what happends to eta
xgtune_high1 <- doParamTuningXG(classif_task, roundsvector =c(100), etavector=c(0.1,0.15,0.2), maxdepthvector = c(6),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high1,"models/imputed/xgtune_high1.rds")
#results
# nrounds = 100, eta = 0.1, max_depth = 6, colsample = .5, subsample = .8
# eta at minimum

# next: try more granular values for eta and check maxdepth
xgtune_high2 <- doParamTuningXG(classif_task, roundsvector =c(100), etavector=c(0.05,0.075,0.1), maxdepthvector = c(5,6,7),colsamplevector = c(0.5),subsamplevector = c(0.8))
saveRDS(xgtune_high2,"models/imputed/xgtune_high2.rds")
#results
# nrounds = 100, eta = 0.075, max_depth = 7, colsample_bytree = .5, subsample = .8
# eta goes down a bit, but not to minium. maxdepth goes up to 7.
# try out varying maxdepth on sample!

#next: try out how 200 trees perform
xgtune_high3 <- doParamTuningXG(classif_task, roundsvector =c(200), etavector=c(0.05,0.075), maxdepthvector = c(5,6,7),colsamplevector = c(0.5),subsamplevector = c(0.8))
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
xgtune4 <- readRDS("models/imputed/xgtune4.rds") # == same as xgtune3 as colsample, subsample did not change
xgtune2$x$eval_metric ="auc"
xgtune3$x$eval_metric ="auc"
xgtune4$x$eval_metric ="auc"
set.seed(1234)
xg2 <- buildXG(classif_task,train.set,test.set,xgtune2$x) #auc = 0.7605456
set.seed(1234)
xg3 <- buildXG(classif_task,train.set,test.set,xgtune3$x) #auc = 0.7629712
set.seed(1234)
xg4 <- buildXG(classif_task,train.set,test.set,xgtune4$x) #auc = 0.7629712 == same as xgtune3 as colsample, subsample did not change

xgtune_high1 <- readRDS("models/imputed/xgtune_high1.rds")
xgtune_high2 <- readRDS("models/imputed/xgtune_high2.rds")
xgtune_high3 <- readRDS("models/imputed/xgtune_high3.rds")
xgtune_high1$x$eval_metric ="auc"
xgtune_high2$x$eval_metric ="auc"
xgtune_high3$x$eval_metric ="auc"
set.seed(1234)
xghigh1 <- buildXG(classif_task,train.set,test.set,xgtune_high1$x)#auc = 0.7601221
set.seed(1234)
xghigh2 <- buildXG(classif_task,train.set,test.set,xgtune_high2$x)#auc = 0.7656591
set.seed(1234)
xghigh3 <- buildXG(classif_task,train.set,test.set,xgtune_high3$x)#auc = 0.7715333

#now check how AUC changes with increased max depth
xgtune_high2depth <- xgtune_high2
xgtune_high2depth$x$max_depth = 8 
set.seed(1234)
xghigh2depth <- buildXG(classif_task,train.set,test.set,xgtune_high2depth$x)#auc = 0.7600198 -- worse than initial setting with depth 7.

xgtune4rounds<- xgtune_high3$x #take high3 with auc = 0.7670
xgtune4rounds$nrounds = 180
set.seed(1234)
xg4tune4roundsfit =  buildXG(classif_task,train.set,test.set,xgtune4rounds) #auc = 0.7701 -- better than 0.7670
xgtune4rounds2 <- xgtune_high3$x
xgtune4rounds2$nrounds = 160
set.seed(1234)
xg4tune4roundsfit =  buildXG(classif_task,train.set,test.set,xgtune4rounds2) #auc = 0.7692 -- worse than 180 version. take 180. 
### now check eta
xgtune4rounds_eta = xgtune4rounds
xgtune4rounds_eta$eta = 0.04 #instead of 0.05
set.seed(1234)
xg4tune4rounds_etafit =  buildXG(classif_task,train.set,test.set,xgtune4rounds_eta) #auc = 0.7691 -- worse than 0.7701.
### now check max_depth 
xgtune4rounds_eta_depth = xgtune4rounds
xgtune4rounds_eta_depth$max_depth = 4
set.seed(1234)
xg4tune4rounds_etadepthfit = buildXG(classif_task,train.set,test.set,xgtune4rounds_eta_depth) #auc = 0.7681 -- worse than 0.7701
##########################################
##########################################
### FINAL XGBOOST TUNING: xgtune_high3 with 180 rounds instead of 200: 
#nrounds =180, eta = 0.05,  max_depth = 5, colsample: .5, subsample =.8
xgboostfinalparameters <- list(nrounds=180,eta=0.05,max_depth=5,colsample_bytree=0.5,subsample=0.8, eval_metric="auc")
saveRDS(xgboostfinalparameters, "models/imputed/xg_tunedparams.rds")

classif_task = classif.task
xgboostfinal = buildXG(classif_task, train.set,test.set, xgboostfinalparameters)
set.seed(1234)
xgboostfinal = buildXG(classif_task, train.set,test.set, xgboostfinalparameters)

saveRDS(xgboostfinal, "models/imputed/xgboost_tuned.rds")

# customized grid search
# this section is concerned with a more thorough tuning approach
nrounds = c(100)
eta = c(0.01, 0.015)
max_depth = c(5, 6, 7, 8, 9, 10)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults1 = customXGBoostTune(classif_task, train.set,test.set, 
                                nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults1, "data/tuneResults1.rds")

nrounds = c(150) #MC
eta = c(0.01, 0.015)
max_depth = c(5, 6, 7, 8, 9, 10)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults2 = customXGBoostTune(classif_task, train.set,test.set, 
                                nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults2, "data/tuneResults2.rds")

nrounds = c(200) #MH
eta = c(0.01, 0.015)
max_depth = c(5, 6, 7, 8, 9, 10)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults3 = customXGBoostTune(classif_task, train.set,test.set, 
                                 nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults3, "data/tuneResults3.rds")

nrounds = c(500) #MC
eta = c(0.01, 0.015)
max_depth = c(5, 6, 7, 8, 9, 10)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults4 = customXGBoostTune(classif_task, train.set,test.set, 
                                 nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults4, "data/tuneResults4.rds")

nrounds = c(1000) #MC
eta = c(0.01, 0.015)
max_depth = c(5, 6, 7, 8, 9, 10)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults5 = customXGBoostTune(classif_task, train.set,test.set, 
                                 nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults5, "data/tuneResults5.rds")

nrounds = c(100, 150, 500, 1000) #MC
eta = c(0.01, 0.015)
max_depth = c(11, 12)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults6 = customXGBoostTune(classif_task, train.set,test.set, 
                                 nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults6, "data/tuneResults6.rds")

nrounds = c(100, 150, 500, 1000) #MC
eta = c(0.01, 0.015)
max_depth = c(13, 14)
colsample = c(0.5, 0.6, 0.7)
subsample = 0.8
tuneResults7 = customXGBoostTune(classif_task, train.set,test.set, 
                                 nrounds, eta, max_depth, colsample, subsample)
saveRDS(tuneResults7, "data/tuneResults7.rds")

tuneResults1 = readRDS("data/tuneResults1.rds")
tuneResults2 = readRDS("data/tuneResults2.rds")
tuneResults3 = readRDS("data/tuneResults3.rds")
tuneResults4 = readRDS("data/tuneResults4.rds")
tuneResults5 = readRDS("data/tuneResults5.rds")
tuneResults6 = readRDS("data/tuneResults6.rds")
tuneResults7 = readRDS("data/tuneResults7.rds")
colnames(tuneResults1$grid) = c("nrounds", "eta", "max_depth", "colsample", "subsample")
auc1 = as.data.frame(cbind(tuneResults1$grid, numeric(nrow(tuneResults1$grid))))
for (i in 1:(length(tuneResults1)-1)) auc1[i,6] = tuneResults1[[(i+1)]]$auc
auc2 = as.data.frame(cbind(tuneResults2$grid, numeric(nrow(tuneResults2$grid))))
for (i in 1:(length(tuneResults2)-1)) auc2[i,6] = tuneResults2[[(i+1)]]$auc
auc3 = as.data.frame(cbind(tuneResults3$grid, numeric(nrow(tuneResults3$grid))))
for (i in 1:(length(tuneResults3)-1)) auc3[i,6] = tuneResults3[[(i+1)]]$auc
auc4 = as.data.frame(cbind(tuneResults4$grid, numeric(nrow(tuneResults4$grid))))
for (i in 1:(length(tuneResults4)-1)) auc4[i,6] = tuneResults4[[(i+1)]]$auc
auc5 = as.data.frame(cbind(tuneResults5$grid, numeric(nrow(tuneResults5$grid))))
for (i in 1:(length(tuneResults5)-1)) auc5[i,6] = tuneResults5[[(i+1)]]$auc
auc6 = as.data.frame(cbind(tuneResults6$grid, numeric(nrow(tuneResults6$grid))))
for (i in 1:(length(tuneResults6)-1)) auc6[i,6] = tuneResults6[[(i+1)]]$auc
auc7 = as.data.frame(cbind(tuneResults7$grid, numeric(nrow(tuneResults7$grid))))
for (i in 1:(length(tuneResults7)-1)) auc7[i,6] = tuneResults7[[(i+1)]]$auc

aucs = rbind(auc1, auc2, auc3, auc4, auc5, auc6, auc7)
colnames(aucs) = c("nrounds", "eta", "max_depth", "colsample", "subsample", "auc")
write.csv(aucs, "data/xgboost_tuning.csv")

png("fig/xgb_tuneResults_nrounds_maxDepth.png", width = 800, height = 800)
plotHeatMap(aucs, aucs$nrounds, aucs$max_depth, "nrounds", "max_depth")
dev.off()

png("fig/xgb_tuneResults_nrounds_eta.png", width = 800, height = 800)
plotHeatMap(aucs, aucs$nrounds, aucs$eta, "nrounds", "eta")
dev.off()

png("fig/xgb_tuneResults_nrounds_colsample.png", width = 800, height = 800)
plotHeatMap(aucs, aucs$nrounds, aucs$colsample, "nrounds", "colsample")
dev.off()

auc5 = read.csv("data/xgboost_tuning.csv")
auc5 = auc5[auc5$nrounds == 500,]
auc5 = auc5[,-1]
