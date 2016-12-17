convertObviousNAs = function(x) {
  # fill blanks with n/a values
  x[x == ""] = NA
  # fill -1 with n/a values
  x[x == -1] = NA
  # fill [] with n/a values
  x[x == "[]"] = NA
  return(x)
}

getNAsPercentage = function(x) {
  print("starting...")
  result = numeric(ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    result[i] = sum(is.na(x[,i]))/nrow(x)
  }
  return(result)
}


findOneValueAndNAs = function(x) {
  print("starting...")
  result = logical(ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    if (length(unique(na.omit(x[,i]))) == 1) result[i] = TRUE
  }
  return(result)
}

inspectValues = function(x) {
  result = data.frame(matrix(nrow = 11, ncol = 3))
  row.names(result) = c("uniqueValues", "min", "max", "median", "uniqueMean", "uniqueMeanMinus1Extremum",
                                    "uniqueMeanMinus2Extrema", "uniqueMeanMinus3Extrema", "naValues",
                        "secondLowest", "secondHighest")
  for (i in 1:ncol(x)) {
    print(paste("handling column", i, "out of", ncol(x)))
    column = numeric(7)
    temp = x[,i]
    column[9] = sum(is.na(temp))
    temp = na.omit(temp)
    column[1] = length(unique(temp))
    column[2] = min(temp)
    column[3] = max(temp)
    column[4] = median(temp)
    uniqueTemp = unique(temp)
    column[5] = mean(uniqueTemp)
    sortedUniqueTemp = sort(unique(temp))
    column[10] = sortedUniqueTemp[2]
    column[11] = sortedUniqueTemp[(length(sortedUniqueTemp)-1)]
    temp = temp[!temp == min(temp)]
    temp = temp[!temp == max(temp)]
    column[6] = mean(unique(temp))
    temp = temp[!temp == min(temp)]
    temp = temp[!temp == max(temp)]
    column[7] = mean(unique(temp))
    temp = temp[!temp == min(temp)]
    temp = temp[!temp == max(temp)]
    column[8] = mean(unique(temp))
    result[,i] = column
  }
  return(result)
}

# determines values that are too high and therefore likely to be NAs by looking at huge steps in sorted unique values
findAbsurdlyHighValues = function(x, stepThreshold) {
  result = numeric(0)
  print("starting...")
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    uniqueValues = sort(na.omit(unique(x[,i])))
    if (length(uniqueValues) == 1) next
    for (j in 1:(length(uniqueValues) - 1)) {
      # take abs for negative values
      # add one to avoid problems with zeros
      if (abs(uniqueValues[j]+1) < abs(uniqueValues[j+1]/stepThreshold)) {
        result = c(result, i)
        break
      }
    }
  }
  return(result)
}

convertNAs = function(x, naEncodings) {
  print("starting...")
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  replacements = 0
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    for (j in 1:nrow(x)) {
      if (x[j,i] %in% naEncodings) {
        x[j,i] = NA
        replacements = replacements + 1
      }
    }
  }
  print(paste(replacements, "replacements done out of", ncol(x) * nrow(x), "total values"))
  return(x)
}