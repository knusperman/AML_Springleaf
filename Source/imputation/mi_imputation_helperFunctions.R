# wrapper function for the complete function of the MI package for imputation
# builds the imputation matrix (i.e. those attributes used for imputation and the attribute to be imputed)
# also formats the result of complete
imputeWrapper = function(dataframe, colnum, miMatrix) {
  impCols = which(miMatrix[,colnum]==1)
  colname = colnames(dataframe)[colnum]
  impDF = dataframe[,c(colnum, impCols)]
  print(paste("imputing:", colname, "----",colnum, sep=" "))
  if(sum(is.na(df_imputed[,colnum])) > 0) {
    result = complete(mi(impDF,n.chains=1, n.iter=15,parallel=FALSE),1)
    result = data.frame(row.names = row.names(impDF[,1]), res = result[unlist(result[paste("missing_",colname,sep="")]),1])
    gc() # collect some garbage
    return(result)
  }
  else {
    print("no NAs.")
  }
}


# provides the attributes with the highest absolute correlation per attribute
# to limit the amount of attributes used for value imputation for the most
# expressive ones for the attribute of the missing value
buildMiceMatrix = function(correlation, usedAttributes = 5, naCor, naCorThreshold) {
  diag(correlation) = 0
  result = matrix(ncol = ncol(correlation), nrow = nrow(correlation))
  for (i in 1:ncol(correlation)) {
    ord = order(abs(correlation[,i]), decreasing = TRUE)
    ord = ord[!(ord %in% (which(naCor[,i] > naCorThreshold)))]
    ord = ord[1:usedAttributes]
    temp = seq(1, ncol(correlation), by = 1)
    temp = temp %in% ord
    temp[temp == TRUE] = 1
    temp[temp == FALSE] = 0
    result[i,] = temp
  }
  return(result)
}
