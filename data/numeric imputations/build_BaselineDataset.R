numericalData_imputed = readRDS("data/baseline_numericalAndFactor.rds")

# build entire baseline dataset
trainData = readRDS("data/check1.rds")
trainData = trainData[,-1]
target = trainData[,ncol(trainData)]
trainData = trainData[,-ncol(trainData)]
dataset = as.data.frame(cbind(numericalData_imputed, 
                              trainData[,!colnames(trainData) %in% colnames(numericalData_imputed)], target = target))
saveRDS(dataset, "data/final/baselineDataset.rds")