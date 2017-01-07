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
source("source/imputation/mi_imputation_helperFunctions.R")
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
seq = 1:
  ###### adjust i up to ncol(df) on all computing devices. 
  for(i in seq){ 
    imp <- createimputation(df, df_imputed, i)
    if(class(imp)=="data.frame"){
      df_imputed[rownames(imp), i] = imp[,1]
    }
  }