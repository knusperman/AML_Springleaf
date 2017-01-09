# in this file, the performance achieved by PCA and the normal set are compared 
# both in terms of AUC and computation time
# try prediction based on our transformed data

library(xgboost)
library(mlr)

xgtune_high3 <- readRDS("models/imputed/xgtune_high3.rds") # results of xg_tuningplan.R
source("source/prediction/prediction_functions.R")

train_transformed = readRDS("data/PCA_transformed_set.rds")

data_factors = as.data.frame(readRDS("data/final/factorAttributes_FINAL.rds"))[rownames(train_transformed),] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(train_transformed),]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(train_transformed),] #f
data_boolean = as.data.frame(readRDS("data/final/booleanAttributes_FINAL.rds"))[rownames(train_transformed),]
data_target  = as.data.frame(readRDS("data/target.rds"))[rownames(train_transformed),] #f #full train records
mydata <- cbind(train_transformed,data_factors,data_strings,data_dates,data_boolean, data_target)
colnames(mydata)[ncol(mydata)]="target"

classif_task_pca = makeClassifTask(id = "mtc", data = mydata, target = "target", positive="1")
mydata <- buildDataSet(c(1,2,3))
classif_task_cleansed = makeClassifTask(id = "mtc", data = mydata, target = "target", positive="1")



# now test for different sample sizes
# 20000
set.seed(1234)
n = 20000 # take almost all samples (for an even number)
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

time = Sys.time()
res_pca_20k =  buildXG(classif_task_pca,train.set,test.set,xgtune_high3$x)
diff_pca_20k = Sys.time() - time
time = Sys.time()
res_cleansed_20k =  buildXG(classif_task_cleansed,train.set,test.set,xgtune_high3$x)
diff_cleansed_20k = Sys.time() - time



# 40000
set.seed(1234)
n = 40000 # take almost all samples (for an even number)
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

time = Sys.time()
res_pca_40k =  buildXG(classif_task_pca,train.set,test.set,xgtune_high3$x)
diff_pca_40k = Sys.time() - time
time = Sys.time()
res_cleansed_40k =  buildXG(classif_task_cleansed,train.set,test.set,xgtune_high3$x)
diff_cleansed_40k = Sys.time() - time


# 60000
set.seed(1234)
n = 60000 # take almost all samples (for an even number)
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

time = Sys.time()
res_pca_60k =  buildXG(classif_task_pca,train.set,test.set,xgtune_high3$x)
diff_pca_60k = Sys.time() - time
time = Sys.time()
res_cleansed_60k =  buildXG(classif_task_cleansed,train.set,test.set,xgtune_high3$x)
diff_cleansed_60k = Sys.time() - time


# 80000
set.seed(1234)
n = 80000 # take almost all samples (for an even number)
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]

time = Sys.time()
res_pca_80k =  buildXG(classif_task_pca,train.set,test.set,xgtune_high3$x)
diff_pca_80k = Sys.time() - time
time = Sys.time()
res_cleansed_80k =  buildXG(classif_task_cleansed,train.set,test.set,xgtune_high3$x)
diff_cleansed_80k = Sys.time() - time

runtimes = c(diff_pca_20k, diff_pca_40k, diff_pca_60k, diff_pca_80k, 
             diff_cleansed_20k, diff_cleansed_40k, diff_cleansed_60k, diff_cleansed_80k)
saveRDS(runtimes, "data/runtimesComparison.rds")
aucs = c(res_pca_20k$auc, res_pca_40k$auc, res_pca_60k$auc, res_pca_80k$auc, 
         res_cleansed_20k$auc, res_cleansed_40k$auc, res_cleansed_60k$auc, res_cleansed_80k$auc)
saveRDS(runtimes, "data/runtimesComparison_aucs.rds")

# plot runtimes
plotData = as.data.frame(cbind(runtime = runtimes, 
                         size = rep(c("20k", "40k", "60k", "80k"), 2), 
                         set = c(rep("PCA", 4), rep("Whole data set", 4))))
plotData[,1] = as.numeric(as.character(plotData[,1]))
  
png("fig/pcavswhole_performance.png", height = 800, width = 1200)
ggplot(data = plotData, aes(x=size, y=runtime, group=set)) +
  geom_line(aes(colour = set), size = 2) + 
  xlab("Sample size") + ylab("Runtime in minutes") + theme_bw()+ 
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
  theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), legend.title = element_text(size = 40, face = "bold"))
dev.off()

# plot aucs
plotData = as.data.frame(cbind(auc = aucs, 
                               size = rep(c("20k", "40k", "60k", "80k"), 2), 
                               set = c(rep("PCA", 4), rep("Whole data set", 4))))
plotData[,1] = as.numeric(as.character(plotData[,1]))
png("fig/pcavswhole_auc.png", height = 800, width = 1200)
ggplot(data = plotData, aes(x=size, y=auc, group=set)) +
  geom_line(aes(colour = set), size = 2) + 
  xlab("Sample size") + ylab("AUC") + theme_bw()+ 
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
  theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), legend.title = element_text(size = 40, face = "bold"))
dev.off()
