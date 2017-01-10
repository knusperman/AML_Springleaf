# get variable importance via manual random forest (mlr does not return importance, 
# even if importance is set to true)
res = randomForest(target ~., data = mydata, ntree = 500, mtry = 40, nodesize = 10, importance = TRUE)
plotData = as.data.frame(cbind(id = seq_along(res$importance[,4]), 
                               importance = sort(res$importance[,4], decreasing = TRUE)))


png("fig/varImportance.png", height = 800, width = 800)
ggplot(plotData, aes(x = id, y = importance)) + 
  geom_line(size = 2) +
  xlab("Attributes") + ylab("Mean Decrease in Gini") + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

# investige variables with high importance
head(plotData)
unique(mydata[,colnames(mydata) == "VAR_0274"]) # states
unique(mydata[,colnames(mydata) == "VAR_0237"]) # states
unique(mydata[,colnames(mydata) == "VAR_0342"]) # unidentified string factors
# month data
# month data
unique(mydata[,colnames(mydata) == "VAR_0810"]) # unidentified factors



