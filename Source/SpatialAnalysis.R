# find potential zip codes
potentialZip = apply(numericalData, 2, function(x) {
  min(na.omit(x)) >= 10000 & max(na.omit(x)) <= 99999
})

which(potentialZip == TRUE)