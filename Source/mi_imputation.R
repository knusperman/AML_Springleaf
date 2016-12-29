buildMiceMatrix = function(correlation, usedAttributes = 100) {
  diag(correlation) = 0
  result = matrix(ncol = ncol(correlation), nrow = nrow(correlation))
  for (i in 1:ncol(correlation)) {
    cors = order(abs(correlation[,i]), decreasing = TRUE)[1:usedAttributes]
    temp = seq(1, ncol(correlation), by = 1)
    temp = temp %in% cors
    temp[temp == TRUE] = 1
    temp[temp == FALSE] = 0
    result[,i] = temp
  }
  diag(result) = 0
  return(result)
}
spearman <- readRDS("data/spearman_without1.rds")
miceMatrix <- buildMiceMatrix(spearman, 5)

getMostCorCols <- function(colnum){
  which(miceMatrix[,colnum]==1)
}

numericalDataSample <- readRDS("data/numericalData_sampleLowestNA_withoutCor1.rds")

df <- as.data.frame(numericalDataSample)
df_imputed <- df
library(mi)

createimputation <- function(colnum){
  colname = colnames(df)[colnum]
  print(paste("imputing:", colname, sep=" "))
  if(sum(is.na(df[,colnum]))>0){
    print("NAs detected. Starting imputation")
    result <- complete(mi(df[, c(colnum, getMostCorCols(colnum))],n.chains=1, n.iter=15),1)
    result <- data.frame(row.names = rownames(result[unlist(result[paste("missing_",colname,sep="")]),]), result[unlist(result[paste("missing_",colname,sep="")]),1])
    colnames(result) = colname
    result
  }else{
    print("no NAs.")
  }
  
}

for(i in 114:116){
  imp <- createimputation(i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
  
}

