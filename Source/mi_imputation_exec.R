
############################################################################################################
#not on aws
numericalDataSample <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")

set.seed(1234)
s <- sample(1:nrow(numericalDataSample), 10000)
df <- as.data.frame(numericalDataSample)[s,] 
saveRDS(df, "data/newnum.rds")
#END not on aws
############################################################################################################
#on AWS, elsewehere
df <- readRDS("data/newnum.rds")
#END on AWS
############################################################################################################

#cast factors to numerics
df <- as.data.frame(df)
df$VAR_1444<-NULL
df$VAR_1445<-NULL
df$VAR_0445<-NULL
df$VAR_0449<-NULL

for(i in 1:ncol(df)) df[,i]= as.numeric(df[,i])

spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(df)),which(colnames(spearman) %in% colnames(df))] # to be sure to select only top correlations that are in current set
miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
df_imputed <- df

###### adjust i up to ncol(df) on all computing devices. 
for(i in 800){ 
  imp <- createimputation(i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
  
}

NAs <- apply(df, 2, function(x) sum(is.na(x)))
NAratio <- sum(NAs)/sum(ncol(df)*nrow(df))

afterImputationNAs <- apply(df_imputed[,1000:ncol(df_imputed)], 2, function(x) sum(is.na(x)))
afterImputationNAs <- afterImputationNAs[afterImputationNAs>0]
