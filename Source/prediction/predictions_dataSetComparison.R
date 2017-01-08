xgtune_high3 <- readRDS("models/imputed/xgtune_high3.rds") # results of xg_tuningplan.R

source("source/prediction/prediction_initialize_baseline.R")
source("source/prediction/prediction_initialize.R")
source("source/prediction/prediction_initialize_PCA.R")
source("source/prediction/prediction_functions.R")
res_cleansed =  buildXG(classif_task,train.set,test.set,xgtune_high3$x)
res_pca =  buildXG(classif_task_pca,train.set,test.set,xgtune_high3$x)
res_baseline =  buildXG(classif_task_baseline,train.set,test.set,xgtune_high3$x)
