stringData = readRDS("data/stringAttributes_cleansed.rds")
target = as.numeric(readRDS("data/target.rds"))
uniqueValues = apply(stringData, 2, function(x) {
  length(unique(x))
})

plotData = as.data.frame(cbind(id = seq_along(uniqueValues), uniqueValues))

png("fig/stringUniqueValues.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = uniqueValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# dig into cities
numbers = as.data.frame(table(stringData[,3]))
ord = order(numbers[,2], decreasing = TRUE)
plotData = numbers[ord,][1:10,]
plotData[,1] = factor(plotData[,1], levels = plotData[,1])
# check if any of the top cities has higher response rates
responseRate = numeric(10)
for (i in 1:nrow(plotData)) {
  targets = target[which(stringData[,3] == as.character(plotData[i,1]))]
  responseRate[i] = sum(targets)
}
plotData = cbind(plotData, responseRate)
png("fig/cityNumbers.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") + xlab("City") + ylab("Number of occurrences") + 
  geom_line(data = plotData, aes(x = Var1, y = responseRate, group = 1), colour = "red", size = 4) + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

# dig into job descriptions
stringCol = 17
numbers = as.data.frame(table(stringData[,stringCol]))
ord = order(numbers[,2], decreasing = TRUE)
plotData = numbers[ord,][1:10,]
plotData[,1] = factor(plotData[,1], levels = plotData[,1])
responseRate = numeric(10)
for (i in 1:nrow(plotData)) {
  targets = target[which(stringData[,stringCol] == as.character(plotData[i,1]))]
  responseRate[i] = sum(targets)
}
plotData = cbind(plotData, responseRate)
png("fig/jobDescriptionNumbers.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") + xlab("Job description") + ylab("Number of occurrences") + 
  geom_line(data = plotData, aes(x = Var1, y = responseRate, group = 1), colour = "red", size = 4) + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

# dig into job descriptions 2
stringCol = 20
numbers = as.data.frame(table(stringData[,stringCol]))
ord = order(numbers[,2], decreasing = TRUE)
plotData = numbers[ord,][1:10,]
responseRate = numeric(10)
for (i in 1:nrow(plotData)) {
  targets = target[which(stringData[,stringCol] == as.character(plotData[i,1]))]
  responseRate[i] = sum(targets)
}
plotData[,1] = sapply(plotData[,1], function(x) {
  sub = substr(x, 1, 10)
  sub = paste(sub, "...", sep = "")
  return(sub)
})
plotData = cbind(plotData, responseRate)
plotData[,1] = factor(plotData[,1], levels = plotData[,1])
png("fig/jobDescriptionNumbers2.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") + xlab("Job description") + ylab("Number of occurrences") + 
  geom_line(data = plotData, aes(x = Var1, y = responseRate, group = 1), colour = "red", size = 4) + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

# check for the other strings by comparing the maximum of the response rate vs. the minimum 
# (for the 10 highest occurrences)
stringCol = seq(1, 21, by = 1)
result = numeric(0)
naPercentage = numeric(0)
j = 1
for (i in stringCol) {
  print(i)
  numbers = as.data.frame(table(stringData[,i]))
  ord = order(numbers[,2], decreasing = TRUE)
  plotData = numbers[ord,]
  responseRate = numeric(nrow(numbers))
  for (k in 1:nrow(plotData)) {
    targets = target[which(stringData[,i] == as.character(plotData[k,1]))]
    responseRate[k] = sum(targets) / length(targets)
  }
  responseRate[is.nan(responseRate)] = NA
  result[j] = var(na.omit(responseRate))
  naPercentage[j] = sum(is.na(stringData[,i]))/nrow(stringData)
  j = j + 1
}
result = cbind(VarianceResponse = result, UniqueValues = uniqueValues, naPercentage = naPercentage)


