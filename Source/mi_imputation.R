if (!"mi" %in% installed.packages()) install.packages("mi")
library(mi)

miCorMatrix = function(correlation, usedAttributes = 100) {
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

getMostCorCols <- function(colnum){
  #translates miCorMatrix Output so that the indices of the intesting correlations are returened
  which(miMatrix[,colnum]==1)
}

createimputation <- function(colnum, runParallelinside=TRUE){
  #takes the number of the column that is checked for imputation
  #if necessary a data frame is returned holding the imputed values. 
  colname = colnames(df)[colnum]
  print(paste("imputing:", colname, "----",colnum, sep=" "))
  if(sum(is.na(df_imputed[,colnum]))>0){
    print("NAs detected. Starting imputation")
    impCols = getMostCorCols(colnum)
    if(0 < sum(length(impCols)+1 == apply(df[is.na(df[,colnum]),c(colnum,impCols)], 1, function(x) sum(is.na(x))))){
      #there is at least one row where all included attributes are NA. 
      #randomly choose imputation vars
      print("All NA row detected. Sample imputation attributes. ")
      while(0 < sum(length(impCols)+1 == apply(df[is.na(df[,colnum]),c(colnum,impCols)], 1, function(x) sum(is.na(x))))){
        impCols = sample((1:dim(df)[2])[-colnum],size = 5) #choose new attributes and repeat if there is a all NA row again. 
      }
    }
    result <- complete(mi(df[, c(colnum, impCols)],n.chains=1, n.iter=15,parallel=runParallelinside),1)
    result <- data.frame(row.names = row.names(df[is.na(df[,colnum]),]), result[unlist(result[paste("missing_",colname,sep="")]),1])
    colnames(result) = colname
    result
  }else{
    print("no NAs.")
  }
  
}

