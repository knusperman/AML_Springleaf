stringData = readRDS("data/stringAttributes_cleansed.rds")
target = readRDS("data/target.rds")
uniqueValues = apply(stringData, 2, function(x) {
  length(unique(x))
})

plotData = as.data.frame(cbind(id = seq_along(uniqueValues), uniqueValues))

png("fig/stringUniqueValues", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = uniqueValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

unique(stringData[,3])

