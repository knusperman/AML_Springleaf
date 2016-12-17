findStrings = function(x) {
  print("starting...")
  result = rep(FALSE, ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    for (j in 1:nrow(x)) {
      if (is.na(x[j,i])) next
      temp = x[j,i]
      if (length(grep("[a-zA-Z]", x[j,i], perl = TRUE)) > 0) {
        result[i] = TRUE
        break
      }
    }
  }
  return(result)
}

findBooleans = function(x) {
  print("starting...")
  result = rep(TRUE, ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    for (j in 1:nrow(x)) {
      if (is.na(x[j,i])) next
      if(length(grep("false|true", x[j,i], ignore.case = TRUE)) == 0) {
        result[i] = FALSE
        break
      }
    }
  }
  return(result)
}

convertToBooleans = function(x) {
  print("starting...")
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    for (j in 1:nrow(x)) {
      if (is.na(x[j,i])) next
      if(length(grep("false", x[j,i], ignore.case = TRUE)) == 0) {
        x[j,i] = FALSE
      }
      else x[j,i] = TRUE
    }
  }
  return(x)
}

findDates = function(x)  {
  print("starting...")
  result = rep(FALSE, ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    for (j in 1:nrow(x)) {
      if (is.na(x[j,i])) next
      if (length(grep("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", x[j,i], perl = TRUE)) > 0) {
        if (length(grep("[0-9]", x[j,i], perl = TRUE)) > 0) {
        result[i] = TRUE
        break
        }
        else break
      }
      else break
    }
  }
  return(result)
}