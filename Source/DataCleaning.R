trainData = read.csv("train.csv", stringsAsFactors = FALSE, strip.white = TRUE)

trainData = convertObviousNAs(trainData)
# remove ID column and target column
target = trainData[, ncol(trainData)]
trainData = trainData[,-c(1, ncol(trainData))]

# perform some initial manual analysis of data
oneFactor = as.vector(which(sapply(apply(trainData, 2, unique), length) == 1)) #5 occurrences
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

# handle date variables differently
dateVariables = as.vector(which(apply(trainData, 2, function(x) {
  sum(grepl("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", x)) > 0
})))
dateSet = trainData[, dateVariables]
# 15, 18, 19 actually not date but Strings
dateSet = dateSet[, -c(15, 18, 19)]

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

write.csv2(trainData[1:1000,], "subset.csv")

# check for duplicate rows
nrow(trainData) - nrow(unique(trainData)) #0

# find out number of NAs per row
nasPerRow = apply(trainData, 1, function(x) {
  sum(is.na(x))
})

#### for initial analysis, sample 10000 rows only to speed up the performance
# do this after data cleaning, since for data cleaning the entire dataset should be used
set.seed(123)
sampleTraining = apply(trainData, 2, function(x) {
  sample(x, 10000, replace = FALSE) 
})

write.csv2(sampleTraining, "sample.csv")


# differentiate datatypes
source("source/ConvertDatatypes.R")
booleanColumns = findBooleans(sampleTraining) #13
booleanData = sampleTraining[,booleanColumns]

dateColumns = findDates(sampleTraining) #16
dateData = sampleTraining[,dateColumns]

stringColumns = findStrings(sampleTraining)
stringData = sampleTraining[,stringColumns & !(booleanColumns | dateColumns)] #21

numericalData = sampleTraining[,!(stringColumns | booleanColumns | dateColumns)] #1875
numericalData = apply(numericalData, 2, as.numeric)

# handle numericals differently that only contain one value and NAs
oneValueAndNAColumns = findOneValueAndNAs(numericalData)
oneValueAndNAData = numericalData[,oneValueAndNAColumns] #47

# remove these from numerical data
numericalData = numericalData[,!oneValueAndNAColumns]


#############################################
# NA HANDLING
#############################################
# inspect data to find NA encodings
source("source/ConvertNAs.R")
initialNAs = getNAsPercentage(sampleTraining)
pdf("fig/NA distribution.pdf")
barplot(sort(initialNAs), main = "NA % of all attributes", ylab = "NAs in %")
dev.off()

pdf("fig/NA distribution_onlyNA.pdf")
barplot(sort(initialNAs[!initialNAs == 0]), 
        main = paste("NA % of", length(initialNAs[!initialNAs == 0]), "attributes with NA values"),  
        ylab = "NAs in %")
dev.off()

# how many rows have only NA values
sum(initialNAs == 1) #3
# remove these
trainData = trainData[,-which(initialNAs == 1)]
sampleTraining = sampleTraining[,-which(initialNAs == 1)]


# now go to more sophisticated NA conversion
overview = inspectValues(numericalData)

# check which mean of unique values differs drastically from the mean of unique means minus outliers
# these outliers are most likely to be different encodings of NA values
naColumns = which(overview[5,] > (overview[6,]*10))

# handle attributes with negative mean differently
# negativeMean = overview[,which(overview[5,] < 0)]
length(which(overview[,which(overview[5,] < 0)][5,] < (overview[,which(overview[5,] < 0)][6,]*10)))/length(overview[,which(overview[5,] < 0)]) # 0.9621212
# almost all negative means columsn are likely to be due to negative NA encodings
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
numericalData = convertNAs(numericalData, naEncodings) 
# 3785860 replacements out of 18750000 total entries -> 20.19%
sum(is.na(numericalData))/(nrow(numericalData)*ncol(numericalData))
# total NAs now: 22.34%

# create correlation heatmap
correlations = cor(numericalData, use="pairwise.complete.obs")
# inspect elements except for diagonal
temp = correlations
diag(temp) = NA
temp = as.vector(temp)
temp = na.omit(temp)
temp = temp[seq(1, length(temp)-1, by = 2)]

# plot extreme values
barplot(c(sort(temp)[1:500], sort(temp)[(length(temp)-499):length(temp)]))

# plot all values
barplot(sort(temp))


if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"reshape2" %in% installed.packages()) install.packages("reshape2")
if (!"corrplot" %in% installed.packages()) install.packages("corrplot")
library(ggplot2)
library(reshape2)
library(corrplot)
pdf("corrplot.pdf")
corrplot(correlations[1:100, 1:100], method = "color", tl.pos = "n")
dev.off()