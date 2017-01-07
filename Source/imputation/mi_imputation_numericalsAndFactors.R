set.seed(1234)
numericalData = as.data.frame(readRDS("data/numericalData_withoutCor1.rds"))
source("source/imputation/mi_imputation_helperFunctions.R")

for (i in 1:ncol(numericalData)) {
  numericalData[,i] = as.numeric(numericalData[,i])
}

nas = apply(numericalData, 2, function(x) sum(is.na(x))/length(x))

numericalData = numericalData[,which(nas < 0.5)]

numericalDataChunk1 = numericalData[1:50000,]
source("source/imputation/mi_imputation.R")
naCorMat <- getMissingnesPatternCorMat(numericalData)


spearman <- readRDS("data/spearman.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(numericalData)),which(colnames(spearman) %in% colnames(numericalData))] # to be sure to select only top correlations that are in current set

miNACorMat <- buildMiceMatrix(spearman,5,naCorMat,0.7)
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
  res = tryCatch(imputeWrapper(numericalDataChunk1, x, miMatrix), error = function(e) e)
  if (!inherits(res, "error")) return(res)
})
snow::stopCluster(cl)

sum(is.na(numericalDataChunk1)) #20136296

for (i in 1:length(res)) {
  print(i)
  if (!is.null(nrow(res[[i]]))) {
    numericalDataChunk1[is.na(numericalDataChunk1[,i]),i] = temp
  }
}
sum(is.na(numericalDataChunk1))


# still some NAs left
# cheap workaround: NAs = median of the attribute
for (i in 1:ncol(numericalDataChunk1)) {
  numericalDataChunk1[is.na(numericalDataChunk1[,i]),i] = median(na.omit(numericalDataChunk1[,i]))
}

saveRDS(numericalDataChunk1, "data/numeric imputations/numericalDataChunk1.rds")
remove(numericalDataChunk1)

numericalDataChunk2 = numericalData[50000:50001,]
source("source/imputation/mi_imputation.R")
naCorMat <- getMissingnesPatternCorMat(numericalDataChunk2)


# take pearson here because it takes less time
pearson = cor(numericalDataChunk2, use = "pairwise.complete.obs")
pearson[is.na(pearson)] = 0
saveRDS(pearson, "data/tempPearson.rds")

miNACorMat <- buildMiceMatrix(pearson,5,naCorMat,0.7)
row.names(miNACorMat) = colnames(numericalDataChunk2)
colnames(miNACorMat) = colnames(numericalDataChunk2)
miMatrix <- miNACorMat
#miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
numericalData_imputed <- numericalDataChunk2
seq = 1:ncol(numericalDataChunk2)
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
  res = tryCatch(imputeWrapper(numericalDataChunk2, x, miMatrix), error = function(e) e)
  if (!inherits(res, "error")) return(res)
})
snow::stopCluster(cl)

sum(is.na(numericalDataChunk2))

for (i in 1:length(res)) {
  temp = res[[i]]
  print(i)
  if (!is.null(nrow(res[[i]]))) {
    numericalDataChunk2[is.na(numericalDataChunk2[,i]),i] = temp
  }
}
sum(is.na(numericalDataChunk2))

# still some NAs left
# cheap workaround: NAs = median of the attribute
for (i in 1:ncol(numericalDataChunk2)) {
  numericalDataChunk2[is.na(numericalDataChunk2[,i]),i] = median(na.omit(numericalDataChunk2[,i]))
}


saveRDS(numericalDataChunk2, "data/numeric imputations/numericalDataChunk2.rds")
remove(numericalDataChunk2)

saveRDS(numericalData_imputed, "data/imputed_numericalAndFactors.rds")
booleanData = readRDS("data/final/booleanAttributes_FINAL.rds")
dateData = readRDS("data/final/dateData_FINAL.rds")
stringData = readRDS("data/final/stringData_FINAL.rds")
target = readRDS("data/target.rds")

dataset = as.data.frame(cbind(numericalData_imputed, booleanData, dateData, stringData, target = target))
saveRDS("data/final/undifferentiatedDataset.rds")
