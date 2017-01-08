
rpart_tune = doParamOptimizationRPART(classif.task,minsplitvector = c(10,15,20,25,30,35),minbucketvector = c(10,15,20,25,30,35),cpvector = c(0.0001,0.0005,0.0010,0.0015))
saveRDS(rpart_tune, "models/imputed/rpart_tuned.rds")
# result:
# minsplit = 15, minbucket = 30, cp = 0.0015

