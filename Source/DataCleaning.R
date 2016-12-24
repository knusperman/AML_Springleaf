if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"reshape2" %in% installed.packages()) install.packages("reshape2")
if (!"corrplot" %in% installed.packages()) install.packages("corrplot")
library(ggplot2)
library(reshape2)
library(corrplot)


trainData = read.csv("data/train.csv", stringsAsFactors = FALSE, strip.white = TRUE)
ncol(trainData) # initially 1934 columns

source("source/ConvertNAs.R")
trainData = convertObviousNAs(trainData)
# remove ID column and target column
target = trainData[, ncol(trainData)]
trainData = trainData[,-c(1, ncol(trainData))]

# perform some initial manual analysis of data
oneFactor = as.vector(which(sapply(apply(trainData, 2, unique), length) == 1)) #8 occurrences
# 6 NAs, 1 0, 1 1
twoFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 2)) #66 occurrences
threeFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 3)) #36 occurrences
fourFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 4)) #65 occurrences
fiveFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 5)) #58
sixFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 6)) #29
sevenFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 7)) #38
eightFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 8)) #73
nineFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 9)) #60
tenFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) == 10)) #75
moreThanTenFactors = as.vector(which(sapply(apply(trainData, 2, unique), length) > 10)) #rest

# remove columns with only one factor (they are basically useless)
trainData = trainData[,-oneFactor]

# find duplicate attributes
# this function does NOT check, whether one vector contains more information than the other
# or whether the vectors are complementary
findDuplicateAttributes = function(data) {
  duplicates = data.frame(x = numeric(0), y = numeric(0))
  for (i in 1:ncol(data)-1) {
    print(paste("Handling column", i, "out of", ncol(data)))
    for (j in (i+1):ncol(data)) {
      differ = FALSE
      # if two vector types differ, continue
      if (class(data[,i]) != class(data[,j])) next
      for (k in 1:nrow(data)) {
        if (is.na(data[k, i]) & is.na(data[k,j])) next
        else {
          differ = TRUE
          break
        }
        # if two values differ, these vectors are not duplicates of each other
        if (data[k, i] != data[k,j]) {
          differ = TRUE
          break
        }
      }
      # if no values differed, these vectors are duplicates of each other
      if (!differ) duplicates = rbind(duplicates, cbind(i,j))
    } 
  }
  return(duplicates)
}

duplicateAttributes = findDuplicateAttributes(trainData[1:10000,]) #none

# check for duplicate rows
nrow(trainData) - nrow(unique(trainData)) #0

#### for initial analysis, sample 10000 rows only to speed up the performance
# do this after data cleaning, since for data cleaning the entire dataset should be used
set.seed(123)
sampleTraining = apply(trainData, 2, function(x) {
  sample(x, 10000, replace = FALSE) 
})

write.csv2(sampleTraining, "sample.csv")
sampleTraining = read.csv2("sample.csv", stringsAsFactors = FALSE, strip.white = TRUE)

# differentiate datatypes
source("source/ConvertDatatypes.R")
booleanColumns = findBooleans(sampleTraining) #13
booleanData = sampleTraining[,booleanColumns]

dateColumns = findDates(sampleTraining) #16
dateData = sampleTraining[,dateColumns]

stringColumns = findStrings(sampleTraining)
stringData = sampleTraining[,stringColumns & !(booleanColumns | dateColumns)] #21

numericalData = sampleTraining[,!(stringColumns | booleanColumns | dateColumns)] #1874
numericalData = apply(numericalData, 2, as.numeric)


#############################################
# NA HANDLING
#############################################
# inspect data to find NA encodings
initialNAs = getNAsPercentage(sampleTraining)
# total initial NAs: 574300 (including NAs filtered above with the trivial encoding -1, [], <blank>)
# percentage: 574300/19250000 = 0.02983377

# how many rows have only NA values
sum(initialNAs == 1) # 0
# remove these
# trainData = trainData[,-which(initialNAs == 1)]
# sampleTraining = sampleTraining[,-which(initialNAs == 1)]

# now go to more sophisticated NA conversion
# handle numericals differently that only contain one value and NAs
oneValueAndNAColumns = findOneValueAndNAs(numericalData)
oneValueAndNAData = numericalData[,oneValueAndNAColumns] #48
numericalData = numericalData[,!oneValueAndNAColumns]

overview = inspectValues(numericalData)

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
# these values are likely to be NA encodings, the other found values or 'too random'

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
}
# pattern-like (99999...) outliers do not change compared to the top 5 version above
# therefore, pattern-like outliers are assumed to be NA encodings
# e.g. a column containing all of the below outliers also contained "normal-looking" data with 3-4 digits

# likely candidates for NAs are therefore 999999999, 999999998, 999999997, 999999996, 999999995, 999999994
naEncodings = c(naEncodings, 999999999, 999999998, 999999997, 999999996, 999999995, 999999994)

# convert encodings into actual NAs and update numerical data accordingly
sampleTraining = convertNAsFaster(sampleTraining, naEncodings)
# 4308204 NAs now out of 19240000 total entries -> 0.2239191
numericalData = convertNAsFaster(numericalData, naEncodings)

# remove these from numerical data
numericalData = numericalData[,!oneValueAndNAColumns]
sum(is.na(numericalData))/(nrow(numericalData)*ncol(numericalData))
# total NAs now: 22.34%

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
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Percentage of NAs") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

numericalNAs = getNAsPercentage(numericalData)
# handle numericals differently that only contain one value and NAs
oneValueAndNAColumns = findOneValueAndNAs(numericalData)
oneValueAndNAData = numericalData[,oneValueAndNAColumns] #48
numericalData = numericalData[,!oneValueAndNAColumns]


#############################################
# END NA HANDLING
#############################################

#############################################
# HANDLING OF DIFFERENT DATATYPES
#############################################
# find out what a likely threshold for factors <-> numerical data might be
uniqueNumericalValues = apply(numericalData, 2, function(x) {
  length(unique(x))
})
uniqueNumericalValues = sort(uniqueNumericalValues)
uniqueNumericalValues = as.data.frame(cbind(id = seq_along(uniqueNumericalValues), uniqueNumericalValues))
png("fig/uniqueValues.png", height = 800, width = 800)
ggplot(data = uniqueNumericalValues, aes(x = id, y = uniqueNumericalValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# not much to see there, drill down to only attributes with 100 unique values or less
uniqueNumericalValues100 = uniqueNumericalValues[uniqueNumericalValues[,2] < 101,]
png("fig/uniqueValues_100.png", height = 800, width = 800)
ggplot(data = uniqueNumericalValues100, aes(x = id, y = uniqueNumericalValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# not much to see here either, look at variances of attribute values
# reasoning: factor attribute values are likely to be close together
# only consider less than 101 unique values, it is unlikely that there are more than 100 factors per attribute
uniqueValuesVariances = apply(numericalData[,which(uniqueNumericalValues[,2] < 101)], 2, function(x) {
  var(na.omit(unique(x)))
})
uniqueValuesVariances = sort(uniqueValuesVariances)
uniqueValuesVariances = as.data.frame(cbind(id = seq_along(uniqueValuesVariances), uniqueValuesVariances))
png("fig/uniqueValuesVariances_500.png", height = 800, width = 800)
ggplot(data = uniqueValuesVariances[1:500,], aes(x = id, y = uniqueValuesVariances)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# elbow somewhere between att 250 and 350 (sorted)
png("fig/uniqueValuesVariances_250_350.png", height = 800, width = 800)
ggplot(data = uniqueValuesVariances[250:350,], aes(x = id, y = uniqueValuesVariances)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# find biggest gap
gaps = abs(uniqueValuesVariances[250:349,2] - uniqueValuesVariances[251:350,2])
which(gaps == max(gaps)) # 94
# the biggest gap is between attribute 343 and 344

# analyse the number of unique values for these
factorPotentials = rownames(uniqueValuesVariances[1:343,])
factorPotentials_uniqueNumericalValues = uniqueNumericalValues[rownames(uniqueNumericalValues) %in% factorPotentials,]
factorPotentials_uniqueNumericalValues$id = seq_along(factorPotentials_uniqueNumericalValues$uniqueNumericalValues)
png("fig/uniqueValuesVariances_factorPotentials.png", height = 800, width = 800)
ggplot(data = factorPotentials_uniqueNumericalValues, aes(x = id, y = uniqueNumericalValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()
# find biggest gap
gaps = abs(factorPotentials_uniqueNumericalValues[1:342,2] - factorPotentials_uniqueNumericalValues[2:343,2])
which(gaps == max(gaps)) # 333
# the biggest gap is between attribute 333 and 334
# we therefore conclude that attributes with less than or equal to 36 unique values are categorical if there variance is also low

# some attributes only seem to have two distinct values
unique(as.vector(numericalData[,which(apply(numericalData, 2, function(x) {length(unique(x))}) == 2)])) #0, 1

# encode these as boolean
booleanTemp = numericalData[,which(apply(numericalData, 2, function(x) {length(unique(x))}) == 2)] #11
booleanTemp = convert01Booleans(booleanTemp)

# append to boolean data
booleanData = cbind(booleanData, booleanTemp) #24

# remove from numerical data
numericalData = numericalData[,-which(apply(numericalData, 2, function(x) {length(unique(x))}) == 2)]

# plot these different attribute types
waterfallData = data.frame(desc = c("Total attributes", 
                                    "ID & target", "Only one value", "Boolean", "Dates",
                                    "Strings", "Numerical", "Only 1 value and NA", "Probably categorical",
                                    "Actual numerical"), 
                           amount = c(1934, -2, -8, -24, -16, -21, -1863, -53, -333, -1488))
waterfallData$id = seq_along(waterfallData$amount)
waterfallData$cumsum = cumsum(waterfallData$amount)
# do some ugly manual coding due to reset after "Numerical"
waterfallData$cumsum[8:9] = waterfallData$cumsum[8:9] + 1874
waterfallData$start = c(0, head(waterfallData$cumsum, -1))
waterfallData$start[8:9] = c(1863, 1863-53)
waterfallData$cumsum[10] = 0
waterfallData$desc = factor(waterfallData$desc, levels = waterfallData$desc)

png("fig/typesOfAttributes_waterfall.png", height = 800, width = 800)
ggplot(waterfallData, aes(desc)) + 
  scale_y_continuous(limits = c(0, 2000)) +
  geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, ymin = cumsum, ymax = start), fill = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.45)) +
  geom_text(aes(y = rep(2000, 10), label = abs(amount)), size = 10, colour = "black") +
  xlab("") + ylab("Amount")
dev.off()

#############################################
# END ANALYSIS OF DIFFERENT ATTRIBUTE TYPES
#############################################

#############################################
# CORRELATION ANALYSIS
#############################################

# create correlation heatmap
correlations = cor(numericalData, use="pairwise.complete.obs")
# produces NA values due to the elimination of values used by "pairwise.complete.obs" (but there is no viable alternative)

# sort matrix according to highest correlations
# first transform matrix into vector with indices (-> dataframe)
# this does not seem to anything too useful...
#corrVector = as.vector(correlations)
#corrVector = as.data.frame(cbind(row = rep(1:sqrt(length(corrVector)), sqrt(length(corrVector))), 
#                                 column = rep(1:sqrt(length(corrVector)), each = sqrt(length(corrVector))),
#                                 values = corrVector))
#remove diagonal values
#print("starting...")
#printTimes = seq(nrow(corrVector)/10, nrow(corrVector), length.out = 10)
#printTimes = floor(printTimes)
#for (i in 1:nrow(corrVector)) {
#  if (i %in% printTimes) print(paste(which(printTimes == i)*10, "% done"))
#  if (corrVector[i,1] == corrVector[i,2]) corrVector[i,3] = -999
#}
#corrVector = corrVector[!corrVector[,3] == -999,]
#corrVector = corrVector[order(corrVector$values, decreasing = TRUE),]

# remove every second (duplicates)
#corrVector = corrVector[seq(1, nrow(corrVector) - 1, by = 2),]
#sum(unique(corrVector[,1])[1:100] %in% unique(corrVector[,2])[1:100])


png("fig/corrplot.png")
corrplot(correlations, method = "color", tl.pos = "n", ylab = "", xlab = "")
dev.off()
# not much to see, plot as barchart

temp = correlations
diag(temp) = NA
correlationVector = as.vector(temp)
correlationVector = sort(correlationVector)
plotData = as.data.frame(cbind(id = seq(1, 2000, by = 1), 
                               values = c(correlationVector[1:1000], 
                                          correlationVector[(length(correlationVector)-999):length(correlationVector)])))
plotDataAll = as.data.frame(cbind(id = seq_along(correlationVector), 
                               values = correlationVector))

png("fig/correlations_barplot.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

png("fig/correlations_barplot_all.png", height = 800, width = 800)
ggplot(data = plotDataAll, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# try out spearman correlations
correlationsSpearman = cor(numericalData, use="pairwise.complete.obs", method = "spearman")

png("fig/corrplot_spearman.png")
corrplot(correlationsSpearman, method = "color", tl.pos = "n")
dev.off()
# not much to see, plot as barchart

temp = correlationsSpearman
diag(temp) = NA
correlationVector = as.vector(temp)
correlationVector = sort(correlationVector)
plotData = as.data.frame(cbind(id = seq(1, 2000, by = 1), 
                               values = c(correlationVector[1:1000], 
                                          correlationVector[(length(correlationVector)-999):length(correlationVector)])))
plotDataAll = as.data.frame(cbind(id = seq_along(correlationVector), 
                                  values = correlationVector))

png("fig/correlations_barplot_spearman.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

png("fig/correlations_barplot_all_spearman.png", height = 800, width = 800)
ggplot(data = plotDataAll, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

#############################################
# END CORRELATION ANALYSIS
#############################################