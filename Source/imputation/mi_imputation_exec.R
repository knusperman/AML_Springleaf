
############################################################################################################
#not on aws
numericalDataSample <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")
killedNAs <- readRDS("data/killednumericsNA.rds")
set.seed(1234)
s <- sample(1:nrow(numericalDataSample), nrow(numericalDataSample))
df <- numericalDataSample
df <- as.data.frame(df)[s[130001:length(s)],]
df$VAR_1444<-NULL #quasi identical to 1445 and no useful information
df$VAR_1445<-NULL
df$VAR_0445<-NULL #kick at imputation merge
df$VAR_0449<-NULL
df <- df[, -which(colnames(df) %in% killedNAs)]

saveRDS(df, "data/newnum.rds")

#END not on aws
############################################################################################################
#on AWS, elsewehere
df <- readRDS("data/newnum.rds")
#END on AWS
############################################################################################################

#cast factors to numerics
for(i in 1:ncol(df)) df[,i]= as.numeric(df[,i])
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
exclude =c(208,209,210,716)
###### adjust i up to ncol(df) on all computing devices. 
for(i in seq[-exclude]){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}

afterImputationNAs <- apply(df_imputed[,1000:ncol(df_imputed)], 2, function(x) sum(is.na(x)))
afterImputationNAs <- afterImputationNAs[afterImputationNAs>0]
