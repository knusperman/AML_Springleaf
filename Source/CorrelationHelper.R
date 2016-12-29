eliminateDuplicateCorrelations = function(x) {
  previous = data.frame(cbind(numeric(0), numeric(0)))
  remove = numeric(0)
  for(i in 1:nrow(x)) {
    contained = FALSE
    for (j in 1:nrow(previous)) {
      if (nrow(previous) == 0) break
      if (x[i,1] == previous[j,2] & x[i,2] == previous[j,1]) {
        contained = TRUE
        break
      }
    }
    if (!contained) previous = rbind(previous, x[i,])
    else remove = c(remove, i)
  }
  return(x[-remove,])
}

# provides the attributes with the highest absolute correlation per attribute
# to limit the amount of attributes used for value imputation for the most
# expressive ones for the attribute of the missing value
buildMiceMatrix = function(correlation, usedAttributes = 100) {
  diag(correlation) = 0
  result = matrix(ncol = ncol(correlation), nrow = nrow(correlation))
  for (i in 1:ncol(correlation)) {
    cors = order(abs(correlation[,i]))[1:usedAttributes]
    temp = seq(1, ncol(correlation), by = 1)
    temp = temp %in% cors
    temp[temp == TRUE] = 1
    temp[temp == FALSE] = 0
    result[i,] = temp
  }
  return(result)
}