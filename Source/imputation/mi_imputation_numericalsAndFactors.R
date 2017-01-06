set.seed(1234)
numericalData = as.data.frame(readRDS("data/numericalData_withoutCor1.rds"))
source("source/imputation/mi_imputation_helperFunctions.R")

for (i in 1:ncol(numericalData)) {
  numericalData[,i] = as.numeric(numericalData[,i])
}

numericalDataChunk1 = numericalData[1:50000,]
source("source/imputation/mi_imputation.R")
naCorMat <- getMissingnesPatternCorMat(numericalData)


# take pearson here because it takes less time
pearson = cor(numericalDataChunk1, use = "pairwise.complete.obs")
pearson[is.na(pearson)] = 0
saveRDS(pearson, "data/tempPearson.rds")

miNACorMat <- buildMiceMatrix(pearson,5,naCorMat,0.7)
row.names(miNACorMat) = colnames(numericalDataChunk1)
colnames(miNACorMat) = colnames(numericalDataChunk1)
miMatrix <- miNACorMat
#miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
numericalData_imputed <- numericalDataChunk1
seq = 1:ncol(numericalDataChunk1)
###### adjust i up to ncol(df) on all computing devices.

if (!"snow" %in% installed.packages()) install.packages("snow")
library(snow)
nCores = parallel::detectCores()
cl = makeCluster(nCores, type = "SOCK")
snow::clusterCall(cl, function() library(mi))

source("source/imputation/mi_imputation_helperFunctions.R")

# export currently loaded environment
ex = ls(.GlobalEnv)
snow::clusterExport(cl, ex)
res = snow::clusterApply(cl = cl, x = seq, fun = function(x) {
  res = tryCatch(imputeWrapper(numericalData_imputed, x, miMatrix), error = function(e) e)
  if (!inherits(res, "error")) return(res)
})
snow::stopCluster(cl)

for (i in 1:length(res)) {
  temp = res[[i]]
  col = which(colnames(numericalData_imputed) %in% colnames(res[[i]]))
  numericalData_imputed[is.na(numericalData_imputed[,col]),col] = temp
}

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
