
#############################################
# NA HANDLING
#############################################
# FILE CONTAINS PLOT GENERATION AND REASONING FOR THE IDENTIFICATION OF NA ENCODINGS
# REQUIRES CERTAIN OBJECTS BEING INITIALIZED FROM DATACLEANING.R
source("source/DataCleaning/ConvertNAs_Functions.R")

# inspect data to find NA encodings
initialNAs = getNAsPercentage(trainData)

# how many rows have only NA values
sum(initialNAs == 1) # 0
# remove these
# trainData = trainData[,-which(initialNAs == 1)]
# sampleTraining = sampleTraining[,-which(initialNAs == 1)]

# now go to more sophisticated NA conversion
numericalData = trainData[,numericalColumns]
overview = inspectValues(numericalData)

# plot mean of unique values vs. mean of unique values with outliers removed
ord = order(overview[5,])
  
plotData = as.data.frame(cbind(id = rep(seq_along(overview[5,ord]), 2), 
                               val = c(overview[5,ord], overview[6,ord]),
                               group = c(rep("Mean unique values", length.out = length(overview[5,ord])),
                                           rep("Mean w/o outliers", length.out = length(overview[5,ord])))))
plotData[,1] = as.numeric(plotData[,1])
plotData[,2] = as.numeric(plotData[,2])
plotData[,3] = as.factor(unlist(plotData[,3]))

png("fig/mean_unique_value.png", height = 800, width = 1200)
ggplot(plotData, aes(x = id, y = val)) + geom_line(aes(colour = group), size = 2) +
  xlab("Attribute") + ylab("Value") + 
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
  theme(legend.text = element_text(size = 40), legend.key.size = unit(2,"cm"), 
        legend.title = element_blank())
dev.off()
  
  
# check which mean of unique values differs drastically from the mean of unique means minus outliers
# these outliers are most likely to be different encodings of NA values
naColumns = which(overview[5,] > (overview[6,]*10))

# handle attributes with negative mean differently
# negativeMean = overview[,which(overview[5,] < 0)]
length(which(overview[,which(overview[5,] < 0)][5,] < (overview[,which(overview[5,] < 0)][6,]*10)))/length(overview[,which(overview[5,] < 0)]) # 0.9621212
# almost all negative means columns are likely to be due to negative NA encodings
# the columns not appearing there are due to NaN values when extrema are removed

# drill down to find NA encodings
naColumns = c(naColumns, which(overview[5,] < 0))
overview[2,naColumns]
# these seem to be mainly -99999 values -> likely encoding of NA values
naEncodings = -99999
overview[3,naColumns]
# other encodings
naEncodings = c(naEncodings, 1e+09, 99, 9999, 100) 
# these values are likely to be NA encodings, the other found values are 'too random'

# do the same analysis when 2 outliers are removed at each end 
# random inspections have shown that sometimes NA values are encoded in different ways as two very high values
naColumns = which(overview[5,] > (overview[7,]*10))
overview[10,naColumns] # likely to be no NA encodings
overview[11,naColumns] # 9996, 9998, 98 likely to be NA encodings, but inspect attributes manually
unique(numericalData[,naColumns[2]]) #9996 likely to be NA encoding
unique(numericalData[,naColumns[3]]) #98 likely to be NA encoding
unique(numericalData[,naColumns[length(naColumns)-2]]) #9998 likely to be NA encoding
naEncodings = c(naEncodings, 9996, 9998, 98)

# manual inspection further revealed that sometimes there exist absurdly high values
naColumns = findAbsurdlyHighValues(numericalData, stepThreshold = 100)
# get the maximum 5 values for these candidate columns
potentialNAValues = numeric(0)
for (i in 1:length(naColumns)) {
  uniqueValues = sort(unique(numericalData[,naColumns[i]]))
  potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)])
  if (length(uniqueValues) > 1) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-1])
  if (length(uniqueValues) > 2) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-2])
  if (length(uniqueValues) > 3) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-3])
  if (length(uniqueValues) > 4) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-4])
  if (length(uniqueValues) > 5) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-5])
}
potentialNAValues = sort(unique(potentialNAValues))

# get the maximum 6 values for these candidate columns as comparison
potentialNAValues = numeric(0)
for (i in 1:length(naColumns)) {
  uniqueValues = sort(unique(numericalData[,naColumns[i]]))
  potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)])
  if (length(uniqueValues) > 1) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-1])
  if (length(uniqueValues) > 2) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-2])
  if (length(uniqueValues) > 3) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-3])
  if (length(uniqueValues) > 4) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-4])
  if (length(uniqueValues) > 5) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-5])
  if (length(uniqueValues) > 6) potentialNAValues = c(potentialNAValues, uniqueValues[length(uniqueValues)-6])
}
# pattern-like (99999...) outliers do not change compared to the top 5 version above
# therefore, pattern-like outliers are assumed to be NA encodings
# e.g. a column containing all of the below outliers also contained "normal-looking" data with 3-4 digits

# likely candidates for NAs are therefore 999999999, 999999998, 999999997, 999999996, 999999995, 999999994
naEncodings = c(naEncodings, 999999999, 999999998, 999999997, 999999996, 999999995, 999999994)

# convert encodings into actual NAs and update numerical data accordingly
sum(is.na(numericalData)) # 400332
sum(is.na(numericalData))/length(numericalData) # 0.02136243
sum(is.na(sampleTraining)) # 571408
sum(is.na(sampleTraining))/(ncol(sampleTraining)* nrow(sampleTraining)) # 0.02969896
sampleTraining = convertNAsFaster(sampleTraining, naEncodings)
numericalData = convertNAsFaster(numericalData, naEncodings)
sum(is.na(numericalData)) # 4182816
sum(is.na(numericalData))/length(numericalData) # 0.2232026
sum(is.na(sampleTraining)) # 4309069
sum(is.na(sampleTraining))/(ncol(sampleTraining)* nrow(sampleTraining)) # 0.2239641

# backup
write.csv2(sampleTraining, "data/sampleWithNAs.csv")

# also update the entire file
sum(is.na(trainData)) # 8341185
sum(is.na(trainData))/(nrow(trainData)*ncol(trainData)) # 
trainData = convertNAsFaster(trainData, naEncodings)

# from numerical data, these should be removed (due to problems with correlations etc.)
oneValueAndNAColumns = findOneValueAndNAs(numericalData) #49
numericalData = numericalData[,!oneValueAndNAColumns]
sum(is.na(numericalData))/(nrow(numericalData)*ncol(numericalData))
# total NAs now: 0.2279587

# analyze NA distributions over columns
naPerColumn = apply(sampleTraining, 2, function(x) sum(is.na(x))/length(x))
naPerColumn = sort(naPerColumn)
naPerColumn = as.data.frame(naPerColumn)
naPerColumn = as.data.frame(cbind(Attributes = seq(1, nrow(naPerColumn), by = 1), naPerColumn))
png("fig/NA_column.png", height = 800, width = 800)
ggplot(data = naPerColumn, aes(x = Attributes, y = naPerColumn)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Percentage of NAs") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()
# analyze NA distributions over rows
naPerRow = apply(sampleTraining, 1, function(x) sum(is.na(x))/length(x))
naPerRow = sort(naPerRow)
naPerRow = as.data.frame(naPerRow)
naPerRow = as.data.frame(cbind(Attributes = seq(1, nrow(naPerRow), by = 1), naPerRow))

png("fig/NA_row.png", height = 800, width = 800)
ggplot(data = naPerRow, aes(x = Attributes, y = naPerRow)) + 
  geom_bar(stat = "identity") + xlab("Observations") + ylab("Percentage of NAs") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# now that we got (most) NAs converted, we might follow another sampling approach:
# get samples with the lowest NA percentage
# for this, we need to find out, if the number of NAs impacts the target
trainData = readRDS("data/check2.rds")
target = as.numeric(readRDS("data/target.rds"))
naRows = apply(trainData, 1, function(x) {
  sum(is.na(x))})
orderIndices = order(naRows)
lm(target ~ naRows) # not very high coefficient
sum(target)/length(target) # 0.2325468 target values of 1
# compare the 10000 lowest NA entries with the 10000 highest NA entries and the avg
sum(target[orderIndices[1:10000]])/10000 # 0.1774
sum(target[orderIndices[(length(orderIndices)-9999): length(orderIndices)-9999]]) /
  10000 # 0.2842

lowNA = trainData[orderIndices[1:10000],]
sum(is.na(lowNA)) # 2497846
higherNA = trainData[orderIndices[10001:20000],]
sum(is.na(higherNA)) # 2876050

# plot for visualization
breaks = seq(1, length(orderIndices), length.out = 11)
targetOrdered = target[orderIndices]
naTargetData = numeric(0)
for (i in 1:10) {
  temp = targetOrdered[breaks[i]:(breaks[i+1])]
  temp = sum(temp)/length(temp)
  naTargetData = c(naTargetData, temp)
}
naTargetData = as.data.frame(cbind(id = seq_along(naTargetData), naTargetData))

png("fig/NAvsTarget.png", height = 800, width = 800)
ggplot(data = naTargetData, aes(x = id, y = naTargetData)) + 
  geom_bar(stat = "identity") + xlab("Decile") + ylab("Percentage of responses") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()
#############################################
# END NA HANDLING
#############################################
