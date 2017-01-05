set.seed(1234)
numericalData = as.data.frame(readRDS("data/numericalData_withoutCor1.rds"))

for (i in 1:ncol(numericalData)) {
  numericalData[,i] = as.numeric(numericalData[,i])
}

source("source/imputation/mi_imputation.R")
naCorMat <- getMissingnesPatternCorMat(numericalData)

# take pearson here because it takes less time
pearson = cor(numericalData, use = "pairwise.complete.obs")
pearson[is.na(pearson)] = 0
saveRDS(pearson, "data/tempPearson.rds")

source("source/CorrelationHelper.R")
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

length(errors) # how many attributes could not be imputed?
sum(is.na(numericalData))
sum(is.na(numericalData_imputed))

# still some NAs left
# cheap workaround: NAs = median of the attribute
for (i in 1:ncol(numericalData_imputed)) {
  numericalData_imputed[is.na(numericalData_imputed[,i]),i] = median(na.omit(numericalData_imputed[,i]))
}

saveRDS(numericalData_imputed, "data/imputed_numericalAndFactors.rds")
booleanData = readRDS("data/final/booleanAttributes_FINAL.rds")
dateData = readRDS("data/final/dateData_FINAL.rds")
stringData = readRDS("data/final/stringData_FINAL.rds")
target = readRDS("data/target.rds")

dataset = as.data.frame(cbind(numericalData_imputed, booleanData, dateData, stringData, target = target))
saveRDS("data/final/undifferentiatedDataset.rds")
