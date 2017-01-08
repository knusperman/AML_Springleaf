xgtune_high3 <- readRDS("models/imputed/xgtune_high3.rds") # results of xg_tuningplan.R

source("source/prediction/prediction_initialize_baseline.R")
source("source/prediction/prediction_initialize.R")
source("source/prediction/prediction_initialize_PCA.R")
source("source/prediction/prediction_functions.R")
res_cleansed =  buildXG(classif_task,train.set,test.set,xgtune_high3$x)
res_pca =  buildXG(classif_task_pca,train.set,test.set,xgtune_high3$x)
res_baseline =  buildXG(classif_task_baseline,train.set,test.set,xgtune_high3$x)

res_cleansed$auc # 0.7653273
res_pca$auc # 0.7622879
res_baseline$auc # 0.7572693

png("fig/dataset_comparison.png", height = 800, width = 1200)
getPlotAUC(list(cleansed = res_cleansed$predictions, pca = res_pca$predictions, baseline = res_baseline$predictions))
dev.off()