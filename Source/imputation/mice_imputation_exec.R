source("Source/imputation/mi_imputation.R")
df <- readRDS("data/numeric imputations/impsplit1.rds")
naCorMat <- getMissingnesPatternCorMat(df)

spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(df)),which(colnames(spearman) %in% colnames(df))] # to be sure to select only top correlations that are in current set
miNACorMat <- buildMiceMatrix(spearman,5,naCorMat,0.7)
row.names(miNACorMat) = colnames(df)
colnames(miNACorMat) = colnames(df)

miMatrix <- miNACorMat
#miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
df_imputed <- df
seq = 1:ncol(df)
remove(miNACorMat)
remove(spearman)
remove(naCorMat)
require(mice)
r = mice(df_imputed, predictorMatrix = miMatrix)
