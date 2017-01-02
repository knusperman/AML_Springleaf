install.packages("readr")
library(readr)
read_csv("/Users/mheuchert/Downloads/test.csv")->testset
readRDS("data/newnum.rds")->train_num
testset <- testset[,which(colnames(testset) %in% colnames(train_num) )]

df <- testset
df <- as.data.frame(df)
for(i in 1:ncol(df)){
  df[,i]= as.numeric(df[,i])
}
df_imputed <- df

for(i in 1:100){ #for cols 1-10
  imp <- createimputation(i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
  
}
