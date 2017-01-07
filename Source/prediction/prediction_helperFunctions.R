# requires a list of all pred attributes
getPlotAUC = function(pred) {
  comparisondf = generateThreshVsPerfData(pred, measures = list(fpr, tpr))
  p = ggplot(data= comparisondf$data, aes(x=fpr, y=tpr, color=learner))+geom_abline(slope = 1, intercept = 0, size = 2) +geom_path(size = 2)+xlab("False positive rate") + ylab("True positive rate") + theme_bw()+ 
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
    theme(legend.text = element_text(size = 40), legend.key.height = 1.5, legend.title = element_text(size = 40, face = "bold"))
  
  return(p)
}

buildNumericData <- function(vec){
  data = data.frame()
  for(i in vec){
    p1 =  as.data.frame(readRDS(paste("data/numeric imputations/impsplit",i,"_done1.rds",sep = "")))
    p2 =  as.data.frame(readRDS(paste("data/numeric imputations/impsplit",i,"_done2.rds",sep = "")))
    data = rbind(data,rbind(p1,p2))
  }
  data
}

#check if rows correspond to sample, so that merge with factors etc. is correct:
#s <- readRDS("data/sample.rds") #1 = 1:50000, 2 = 500001:100000, 3 = 100001:145231 in train set
#s[c(1:5,50001:50005,100001:100005)] == rownames(data_numeric)[c(1:5,50001:50005,100001:100005)]

#collist = list("cols_numeric"=colnames(data_numeric),"cols_factors"=colnames(data_factors),"cols_strings" = colnames(data_strings), "cols_dates" = colnames(data_dates))
#saveRDS(collist, "data/collist.rds")

#NAstatistics <- readRDS("data/NAstatistics.rds") #since data has no more information about amout of imputation in a column
#collist <- readRDS("data/collist.rds") #some data for selecting the right columns for predicting
