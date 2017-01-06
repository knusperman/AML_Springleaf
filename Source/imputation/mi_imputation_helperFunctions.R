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