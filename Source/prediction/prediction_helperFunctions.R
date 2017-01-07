# requires a list of all pred attributes
getPlotAUC = function(pred) {
  comparisondf = generateThreshVsPerfData(pred, measures = list(fpr, tpr))
  p = ggplot(data= comparisondf$data, aes(x=fpr, y=tpr, color=learner))+geom_abline(slope = 1, intercept = 0, size = 2) +geom_path(size = 2)+xlab("False positive rate") + ylab("True positive rate") + theme_bw()+ 
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
    theme(legend.text = element_text(size = 40), legend.title = element_text(size = 40, face = "bold"))
  
  return(p)
}

b
buildNumericData <- function(vec){
  data = data.frame()
  for(i in vec){
    p1 =  as.data.frame(readRDS(paste("data/numeric imputations/impsplit",i,"_done1.rds",sep = "")))
    p2 =  as.data.frame(readRDS(paste("data/numeric imputations/impsplit",i,"_done2.rds",sep = "")))
    data = rbind(data,rbind(p1,p2))
  }
  data
}
