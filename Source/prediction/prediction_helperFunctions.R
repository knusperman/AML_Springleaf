# requires a list of all pred attributes
getPlotAUC = function(pred) {
  totalRows = length(pred) * nrow(pred[[1]]$data)
  plotData = as.data.frame(cbind(x = numeric(totalRows), y = numeric(totalRows), Approach = numeric(totalRows)))
  index = 1
  for (i in 1:length(pred)) {
    temp = pred[[i]]
    temp = as.data.frame(temp$data[order(temp$data$prob.0),])
    temp$truth = as.numeric(temp$truth)
    plotData[index:(index + totalRows - 1), 1] = seq(from = 0, to = 1, length.out = nrow(temp))
    plotData[index:(index + totalRows - 1), 2] = cumsum(temp$truth)/sum(temp$truth)
    plotData[index:(index + totalRows - 1), 3] = rep(names(pred)[i])
    index = index + totalRows
  }
  p = ggplot(data = plotData, aes(x = x, y = y, group = Approach)) + 
    geom_line(aes(linetype = Approach), size = 2) + geom_abline(slope = 1, intercept = 0, size = 2) +
    xlab("False positive rate") + ylab("True positive rate") + theme_bw() +
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
    theme(legend.text = element_text(size = 40), legend.title = element_text(size = 40, face = "bold"))
  return(p)
}

