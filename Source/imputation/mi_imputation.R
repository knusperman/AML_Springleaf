if (!"mi" %in% installed.packages()) install.packages("mi")
library(mi)

#miCorMatrix = function(correlation, usedAttributes = 100) {
  #based on buildMiceMatrix to get top x correlations of the attributes in a square matrix
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
buildMiceMatrix = function(correlation, usedAttributes = 5, naCor, naCorThreshold) {
  diag(correlation) = 0
  diag(naCor)= 0
  result = matrix(ncol = ncol(correlation), nrow = nrow(correlation))
  for (i in 1:ncol(correlation)) {
    ord = order(abs(correlation[,i]), decreasing = TRUE)
    ord = ord[!(ord %in% (which(naCor[,i] > naCorThreshold)))]
    ord = ord[1:usedAttributes]
    temp = seq(1, ncol(correlation), by = 1)
    temp = temp %in% ord
    temp[temp == TRUE] = 1
    temp[temp == FALSE] = 0
    result[,i] = temp
  }
  return(result)
}

getMostCorCols <- function(colnum){
  #translates miCorMatrix Output so that the indices of the intesting correlations are returened
  which(miMatrix[,colnum]==1)
}
getMissingnesPatternCorMat <- function(data ){
  cor(is.na(data),use="everything",method="pearson")
}

createimputation <- function(colnum, runParallelinside=TRUE){
  #takes the number of the column that is checked for imputation
  #if necessary a data frame is returned holding the imputed values. 
  colname = colnames(df)[colnum]
  print(paste("imputing:", colname, "----",colnum, sep=" "))
  if(sum(is.na(df_imputed[,colnum]))>0){
    print("NAs detected. Starting imputation")
    impCols = getMostCorCols(colnum)
    result <- complete(mi(df[, c(colnum, impCols)],n.chains=1, n.iter=15,parallel=runParallelinside),1)
    result <- data.frame(row.names = row.names(df[is.na(df[,colnum]),]), result[unlist(result[paste("missing_",colname,sep="")]),1])
    colnames(result) = colname
    result
  }else{
    print("no NAs.")
  }
  
}

