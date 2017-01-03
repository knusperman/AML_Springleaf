
extractDateData = function(dateData, timeSpan) {
  if (!timeSpan %in% c("m", "y", "d", "h")) stop("timespan must be either m, y, d or h")
  result = dateData
  for (i in 1:ncol(dateData)) {
    
    # somehow needed as else invalid factor levels are produced
    if (timeSpan == "h") result[,i] = as.numeric(result[,i])
    for (j in 1:nrow(dateData)) {
      if (timeSpan == "m") result[j,i] = format(as.Date(dateData[j,i], format = "%d%B%y:%H:%M:%S"),'%B')
      if (timeSpan == "y") result[j,i] = format(as.Date(dateData[j,i], format = "%d%B%y:%H:%M:%S"),'%y')
      if (timeSpan == "d") result[j,i] = weekdays(as.Date(dateData[j,i], format = "%d%B%y:%H:%M:%S"))
      if (timeSpan == "h") result[j,i] = substr(format(as.Date(dateData[j,i], format = "%d%B%y:%H:%M:%S"),'%r'),1,2)
    }
  } 
  return(result)
}

# builds plots of given date data with the target as a red line
# if relative = TRUE it only plots the target per month as a % value
buildDatePlots = function(data, target, path, relative = FALSE, month = FALSE) {
  for (i in 1:ncol(data)) {
    numbers = as.data.frame(table(data[,i]))
    # sort according to calendar if months are given
    if (month) numbers = numbers[c(5,4,8,1,9,7,6,2,12,11,10,3),]
    responseRate = numeric(nrow(numbers))
    for (j in 1:nrow(numbers)) {
      targets = target[which(data[,i] == numbers[j,1])]
      responseRate[j] = sum(as.numeric(targets))
    }
    plotData = data.frame(cbind(id = as.character(numbers[,1]), occurrences = numbers[,2], 
                                responseRate = responseRate), stringsAsFactors = FALSE)
    plotData[,1] = factor(plotData[,1], levels = unique(plotData[,1]))
    plotData[,2] = as.numeric(plotData[,2])
    plotData[,3] = as.numeric(plotData[,3])
    if (!relative) makeDatePlot(plotData, path = paste(path, i, ".png", sep = ""))
    else makeRelativeDatePlot(plotData, path = paste(path, i, ".png", sep = ""))
  }
}

# requires as input a dataframe plotData with columns named id, occurrences and responseRate
makeDatePlot = function(plotData, path) {
  png(path, height = 800, width = 800)
  p = ggplot(data = plotData, aes(x = id, y = occurrences)) + 
    geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of occurrences") + theme_bw() +
    geom_line(data = plotData, aes(x = id, y = responseRate, group = 1), colour = "red", size = 4) + 
    theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + scale_x_discrete("", levels(plotData$id))
  print(p)
  dev.off()
}

makeRelativeDatePlot = function(plotData, path) {
  png(path, height = 800, width = 800)
  p = ggplot(data = plotData, aes(x = id, y = (responseRate/occurrences))) + 
    geom_bar(stat = "identity") + xlab("Attributes") + ylab("Target response rate") + theme_bw() +
    theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + scale_x_discrete("", levels(plotData$id))
  print(p)
  dev.off()
}