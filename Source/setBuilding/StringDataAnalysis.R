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

# check for all strings by comparing the maximum of the response rate vs. the minimum 
# (for the 10 highest occurrences)
# reason for this: check if there are any significant differences regarding the highest occurrences and their impact on the target
# the higher the variance, the more impact on the target value
# this is just done as an overview, strings with a low NA percentage and low number of unique values are used anyway
stringCol = seq(1, 17, by = 1)
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

# how to process the data:
# remove cities (3), job descriptions (14, 16) (-> overfitting)
# remove 4 because of 99.99% NAs
# remove 14, 16, 4 due to overfitting
# handle the rest as factors
# bundle states manually (the lower tail) because randomForest can only handle up to 53 factor levels
relevantData = stringData[,-c(3, 14, 16, 4)]
# handle states
plotData = as.data.frame(sort(table(relevantData[,3]), decreasing = TRUE))
plotData = cbind(rownames(plotData), plotData)
plotData[,1] = factor(plotData[,1], levels = unique(plotData[,1]))
colnames(plotData) = c("id", "occurrences")
png("fig/states1.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = occurrences)) + 
  geom_bar(stat = "identity") + xlab("States") + ylab("Number of occurrences") + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) +
  theme(axis.text.x=element_blank())
dev.off()

plotData = as.data.frame(sort(table(relevantData[,4]), decreasing = TRUE))
plotData = cbind(rownames(plotData), plotData)
plotData[,1] = factor(plotData[,1], levels = unique(plotData[,1]))
colnames(plotData) = c("id", "occurrences")
png("fig/states2.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = occurrences)) + 
  geom_bar(stat = "identity") + xlab("States") + ylab("Number of occurrences") + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) +
  theme(axis.text.x=element_blank())
dev.off()

sum()
# very long tail -> bundle all states that have less than 1000 occurrences
# since we work with samples for training the predictors initially, this is required to ensure some sort of stable behavior
stateData1 = as.character(relevantData[,3])
stateData2 = as.character(relevantData[,4])
filter1 = as.data.frame(which(table(stateData1) < 1000))
saveRDS(filter1, "data/stateFilter1.rds")
filter2 = as.data.frame(which(table(stateData2) < 1000))
saveRDS(filter2, "data/stateFilter2.rds")
rownames(filter1) %in% rownames(filter2) # the first filter almost is a subset of the second filter
# set the value of everything that matches these filters to "other"
stateData1[stateData1 %in% rownames(filter1)] = "other"
stateData2[stateData2 %in% rownames(filter2)] = "other"

# merge back with relevantData
relevantData[,3] = stateData1
relevantData[,4] = stateData2

# for all: convert NAs into another factor level
for (i in 1:ncol(relevantData)) {
  relevantData[,i] = as.character(relevantData[,i])
  relevantData[is.na(relevantData[,i]),i] = "na"
}

saveRDS(relevantData, "data/final/stringData_FINAL.rds")
