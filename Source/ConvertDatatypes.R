findStrings = function(x) {
  print("starting...")
  result = rep(FALSE, ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    if (length(grep("[a-zA-Z]", x[,i], perl = TRUE)) > 0) {
      result[i] = TRUE
    }
  }
  return(result)
}

findBooleans = function(x) {
  print("starting...")
  result = rep(FALSE, ncol(x))
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    if(length(grep("false|true", x[,i], ignore.case = TRUE)) == length(na.omit(x[,i]))) {
      result[i] = TRUE
    }
  }
  return(result)
}

convertBooleans = function(x) {
  print("starting...")
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    for (j in 1:nrow(x)) {
      if (is.na(x[j,i])) next
      if (length(grep("false", x[j,i], ignore.case = TRUE)) > 0) x[j,i] = FALSE
      else if (length(grep("true", x[j,i], ignore.case = TRUE)) > 0) x[j,i] = TRUE
      else {
        warning("Value other than true/false found, converting to NA")
        x[j,i] = NA
      }
    }
    x[,i] = as.logical(x[,i])
  }
  return(result)
}

convert01Booleans = function(x) {
  print("starting...")
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    x[which(x[,i] == 1),i] = TRUE
    x[which(x[,i] == 0),i] = FALSE
    x[,i] = as.logical(x[,i])
  }
  return(x)
}

convertToBooleans = function(x) {
  print("starting...")
  printTimes = seq(ncol(x)/10, ncol(x), length.out = 10)
  printTimes = floor(printTimes)
  for (i in 1:ncol(x)) {
    if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
    x[grepl("false", x[j,i], ignore.case = TRUE),j] = FALSE
    x[grepl("true", x[j,i], ignore.case = TRUE),j] = TRUE
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
    if (length(grep("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", x[,i], perl = TRUE)) ==
        length(na.omit(x[,i]))) {
      if (length(grep("[0-9]", x[,i], perl = TRUE)) ==
          length(na.omit(x[,i]))) {
      result[i] = TRUE
      }
    }
  }
  return(result)
}