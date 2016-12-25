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