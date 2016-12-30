p1 <- readRDS("data/imp new/df_imputed1-499.rds")
p2 <- readRDS("data/imp new/df_imputed500-799.rds") #tbd
p3 <- readRDS("data/imp new/df_imputed800-1000.rds")
p4 <- readRDS("data/imp new/df_imputed1001-1199.rds")
p5 <- readRDS("data/imp new/df_imputed1200-end.rds")

merged_imputations <- cbind(p1,p2,p3,p4,p5)
source("Source/mi_imputation.R")
spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(df)),which(colnames(spearman) %in% colnames(df))] # to be sure to select only top correlations that are in current set
miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations

df <- merged_imputations
df_imputed <- df

#check for skipped cols during computation due to strange errors to ensure that there is no NA anywhere.
for(i in 1:ncol(df)){ 
  imp <- createimputation(i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
  
}
merged_imputations <- df_imputed
merged_imputations$VAR_0445 <- NULL #col 163
merged_imputations$VAR_0449 <- NULL #col 166
sum(sapply(merged_imputations, function(x) sum(is.na(x))))
saveRDS(merged_imputations, "data/numerics_sample_imputed.rds")
