numericalData_imputed = readRDS("data/numeric imputations/baseline_numericalAndFactor.rds")

# build entire baseline dataset
trainData = readRDS("data/check1.rds")
trainData = trainData[,-1]
target = trainData[,ncol(trainData)]
trainData = trainData[,-ncol(trainData)]
dataset = as.data.frame(cbind(numericalData_imputed, 
                              trainData[,!colnames(trainData) %in% colnames(numericalData_imputed)], target = target))

# eliminate leftover NAs in factor levels
for (i in 1:ncol(dataset)) {
  if (sum(is.na(dataset[,i])) > 0) {
    levels(dataset[,i]) = c(levels(dataset[,i]), "na")
    dataset[is.na(dataset[,i]),i] = "na"
  }
}
saveRDS(dataset, "data/final/baselineDataset.rds")
