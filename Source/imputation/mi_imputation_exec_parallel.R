
############################################################################################################
#not on aws
numericalDataSample <- data.frame(readRDS("data/numericalAttributes_cleansed_withoutFactors.rds"),stringsAsFactors =FALSE)
killedNAs <- readRDS("data/killednumericsNA.rds")
set.seed(1234)
s <- readRDS("data/sample.rds")
#set.seed(1234)
#s <- sample(1:nrow(numericalDataSample), nrow(numericalDataSample))
df <- numericalDataSample
remove(numericalDataSample)
df$VAR_1444<-NULL #quasi identical to 1445 and no useful information
df$VAR_1445<-NULL
df$VAR_0445<-NULL #kick at imputation merge
df$VAR_0449<-NULL
df <- df[, -which(colnames(df) %in% killedNAs)]
for(i in 1:ncol(df)) df[,i] = as.numeric(df[,i])
df1 <- df[s[1:50000],]
df2 <- df[s[50001:100000],]
df3 <- df[s[100001:length(s)],]

saveRDS(df1,"data/numeric imputations/impsplit1.rds")
saveRDS(df2,"data/numeric imputations/impsplit2.rds")
saveRDS(df3,"data/numeric imputations/impsplit3.rds")
#############################################################################
#############################################################################
source("Source/imputation/mi_imputation.r")
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

if (!"snow" %in% installed.packages()) install.packages("snow")
library(snow)
nCores = parallel::detectCores()
cl = makeCluster(nCores, type = "SOCK")
snow::clusterCall(cl, function() library(mi))

# export currently loaded environment
ex = ls(.GlobalEnv)
snow::clusterExport(cl, ex)
res = snow::clusterApply(cl = cl, x = seq, fun = function(x) {
  res = tryCatch(imputeWrapper(df_imputed, x, miMatrix), error = function(e) e)
  if (!inherits(res, "error")) return(res)
})
snow::stopCluster(cl)

for (i in 1:length(res)) {
  temp = res[[i]]
  col = which(colnames(df_imputed) %in% colnames(res[[i]]))
  df_imputed[is.na(df_imputed[,col]),col] = temp
}

imputeWrapper = function(dataframe, colnum, miMatrix) {
  impCols = which(miMatrix[,colnum]==1)
  colname = colnames(dataframe)[colnum]
  impDF = dataframe[,c(colnum, impCols)]
  print(colnames(impDF))
  print(paste("imputing:", colname, "----",colnum, sep=" "))
  if(sum(is.na(df_imputed[,colnum])) > 0) {
    result = complete(mi(impDF,n.chains=1, n.iter=15,parallel=FALSE),1)
    result = data.frame(row.names = row.names(impDF[,1]), res = result[unlist(result[paste("missing_",colname,sep="")]),1])
    colnames(result) = colname
    gc() # collect some garbage
    return(result)
  }
  else {
    print("no NAs.")
  }
}