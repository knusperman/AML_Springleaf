
getPlotAUC = function(pred) {
  data = as.data.frame(pred$data[order(pred$data$prob.0),])
  data$truth = as.numeric(as.character(data$truth))
  plotData = as.data.frame(cbind(x = seq(from = 0, to = 1, length.out = nrow(data)), 
                                 y = (cumsum(data$truth)/sum(data$truth))))
  p = ggplot(data = plotData, aes(x = x, y = y)) + 
    geom_line(size = 2) + geom_abline(slope = 1, intercept = 0, size = 2) +
    xlab("False positive rate") + ylab("True positive rate") + theme_bw() +
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) 
  return(p)
}

addLinesToAUCPlot = function(plot, pred) {
  data = as.data.frame(pred$data[order(pred$data$prob.0),])
  data$truth = as.numeric(as.character(data$truth))
  plotData = as.data.frame(cbind(x = seq(from = 0, to = 1, length.out = nrow(data)), 
                                 y = (cumsum(data$truth)/sum(data$truth))))
  p = p + geom_line(aes(x, y, size = 2), plotData)
  return(p)
}
