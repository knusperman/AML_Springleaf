rf10k = buildRF(classif_task,train.set,test.set,pars = list(ntree=125)) # =500
saveRDS(rf10k, "rf10k.rds")

rpart10k = buildRPART(classif_task,train.set,test.set, pars = list(minbucket =30,minsplit=15,cp=0.0015 ))
saveRDS(rpart10k, "rpart10k.rds")

xg10k = buildXG(classif_task,train.set,test.set,pars = list(nrounds=100,max_depth=7))
saveRDS(xg10k, "xg10k.rds")


rf45k <- readRDS("models/imputed/rf_default.rds")
rpart45k <- readRDS("models/imputed/rpart_tuned.rds")
xg45k <- buildXG(classif_task, train.set,test.set, pars=list(nrounds=100,max_depth=7))

#taken from notes, models not available in environment
svm10k = 1288
svm45k = 34831


plotData = as.data.frame(cbind(sample = rep(c("10k", "45k"), 4), 
                               runtime = c(rf10k$model$time, rf45k$model$time, rpart10k$model$time,
                                           rpart45k$model$time, xg10k$model$time, xg45k$model$time, 
                                           svm10k, svm45k),
                               learner = c("Random forest","Random forest", "RPART", "RPART", "XGBoost", "XGBoost", "SVM","SVM")))
plotData$runtime <-as.numeric(as.character(plotData$runtime))

png("fig/timecomparison_10_40log.png", height=800, width=1200)
ggplot(data = plotData, aes(x=sample, y=runtime, group = learner)) +
  geom_path(aes(colour = learner), size = 2) +
  xlab("Sample size") + ylab("Runtime in seconds") + theme_bw() + 
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) + scale_y_log10()+
  theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), legend.title = element_text(size = 40, face = "bold"))
dev.off()

png("fig/timecomparison_10_40.png", height=800, width=1200)
ggplot(data = plotData, aes(x=sample, y=runtime, group = learner)) +
  geom_path(aes(colour = learner), size = 2) +
  xlab("Sample size") + ylab("Runtime in seconds") + theme_bw() + 
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) +
  theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), legend.title = element_text(size = 40, face = "bold"))
dev.off()
