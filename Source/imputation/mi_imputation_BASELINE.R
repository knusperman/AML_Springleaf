# the baseline dataset serves as a comparison of our sophisticated cleaning methods vs. 
# no cleaning at all
# We assume that our cleaned dataset significantly outperforms the uncleaned dataset

source("source/imputation/mi_imputation_helperFunctions.R")
source("source/imputation/mi_imputation.R")
trainData = readRDS("data/check1.rds")
set.seed(1234)
factors = readRDS("data/factorPotentials.rds")
numericals = readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")
target = trainData[,ncol(trainData)]
trainData = trainData[,-ncol(trainData)]
trainData = trainData[,-1]
numericalData = as.data.frame(trainData[,(colnames(trainData) %in% colnames(numericals))])
for (i in 1:ncol(numericalData)) {
  numericalData[,i] = as.numeric(numericalData[,i])
}
remove(trainData)
remove(numericals)


naCorMat <- getMissingnesPatternCorMat(numericalData)
spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(df)),which(colnames(spearman) %in% colnames(df))] # to be sure to select only top correlations that are in current set


miNACorMat <- buildMiceMatrix(pearson,5,naCorMat,0.7)
row.names(miNACorMat) = colnames(numericalData)
colnames(miNACorMat) = colnames(numericalData)
miMatrix <- miNACorMat
#miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
numericalData_imputed <- numericalData
seq = 1:ncol(numericalData)
###### adjust i up to ncol(df) on all computing devices.
errors = numeric(0)
for(i in seq){
  imp = tryCatch({
    imp <- createimputation(numericalData, numericalData_imputed, i)
   }, error = function(e) e)
  if (inherits(imp, "error")) {
    errors = c(errors, i)
    next
  }
  if(class(imp)=="data.frame"){
    numericalData_imputed[rownames(imp), i] = imp[,1]
  }
}

# still some NAs left
# cheap workaround: NAs = median of the attribute
for (i in 1:ncol(numericalData_imputed)) {
  numericalData_imputed[is.na(numericalData_imputed[,i]),i] = median(na.omit(numericalData_imputed[,i]))
}

saveRDS(numericalData_imputed, "data/baseline_numericalAndFactor.rds")