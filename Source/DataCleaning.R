if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"reshape2" %in% installed.packages()) install.packages("reshape2")
if (!"corrplot" %in% installed.packages()) install.packages("corrplot")
if (!"mice" %in% installed.packages()) install.packages("mice")
if (!"randomForest" %in% installed.packages()) install.packages("randomForest")
library(ggplot2)
library(reshape2)
library(corrplot)
library(mice)
library(randomForest)

trainData = read.csv("train.csv", stringsAsFactors = FALSE, strip.white = TRUE)
ncol(trainData) # initially 1934 columns
# still some white space remains for some reason, entirely remove all whitespace
trainData = apply(trainData, 2, function(x) {gsub(" ", "", x)})

source("source/ConvertNAs_Functions.R")
trainData = convertObviousNAs(trainData)

# remove ID column
trainData = trainData[,-1]

# remove target column
target = trainData[,ncol(trainData)]
trainData = trainData[,-ncol(trainData)]

# perform some initial manual analysis of data
oneFactor = as.vector(which(sapply(apply(trainData, 2, unique), length) == 1)) #8 occurrences
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

# differentiate data types
source("source/ConvertDatatypes.R")
booleanColumns = findBooleans(trainData) #13
dateColumns = findDates(trainData) #16
stringColumns = findStrings(trainData) #50
numericalColumns = !(stringColumns | booleanColumns | dateColumns) #1874
# convert datatypes
# convert to logical in trainData
for (i in 1:ncol(trainData)) {
  if (booleanColumns[i] == TRUE) trainData[,i] = as.logical(trainData[,i])
}

# convert to numeric in trainData
for (i in 1:ncol(trainData)) {
  if (numericalColumns[i] == TRUE) trainData[,i] = as.numeric(trainData[,i])
}

#### for initial analysis, sample 10000 rows only to speed up the performance
# do this after data cleaning, since for data cleaning the entire dataset should be used
set.seed(123)
sampleIndices = sample(1:nrow(trainData), 10000, replace = FALSE)
sampleTraining = trainData[sampleIndices,]
sampleTarget = trainData[,sampleIndices]
# write for backup
write.csv2(sampleTraining, "data/sample.csv")
# sampleTraining = read.csv2("sample.csv", stringsAsFactors = FALSE, strip.white = TRUE)
# sampleTraining = sampleTraining[,-1]

# differentiate datatypes in sample
booleanData = sampleTraining[,booleanColumns]
dateData = sampleTraining[,dateColumns]
stringData = sampleTraining[,stringColumns & !(booleanColumns | dateColumns)]
numericalData = sampleTraining[,numericalColumns]

#############################################
# NA ANALYIS (consult file ConvertNAs.R for further details on the analysis)
#############################################
naEncodings = c(-99999, 1e+09, 99, 9999, 100, 9996, 9998, 98, 
                999999999, 999999998, 999999997, 999999996, 999999995, 999999994)
trainData = convertNAsFaster(trainData, naEncodings)

# save
write.csv2(trainData, "trainData_initialCleansing.csv")
# trainData = read.csv2("trainData_initialCleansing.csv", , stringsAsFactors = FALSE, strip.white = TRUE)

# find attributes that only have NA and one value
oneValueAndNAColumns = findOneValueAndNAs(trainData) # 52


#############################################
# HANDLING OF DIFFERENT DATATYPES (consult file DatatypesAnalysis.R for further details)
#############################################
numericalData = trainData[,numericalColumns]

# some attributes only seem to have two distinct values
unique(as.vector(numericalData[,which(apply(numericalData, 2, function(x) {length(na.omit(unique(x)))}) == 2)])) #0, 1

# encode these as boolean
moreBooleanColumns = apply(trainData, 2, function(x) {length(unique(x))}) == 2 # 63

# convert to TRUE/FALSE notion
trainData[,moreBooleanColumns] = convert01Booleans(trainData[,moreBooleanColumns])

# append to boolean data
booleanColumns = booleanColumns | moreBooleanColumns

numericalColumns = numericalColumns & !booleanColumns
# remove from numerical data
numericalData = trainData[,numericalColumns]

write.csv2(trainData[,booleanColumns], "data/booleanAttributes_cleansed.csv")
write.csv2(trainData[,dateColumns], "data/dateAttributes_cleansed.csv")
write.csv2(trainData[,stringColumns], "data/stringAttributes_cleansed.csv")
write.csv2(trainData[,numericalColumns], "data/numericalAttributes_cleansed.csv")


# create backups
write.csv2(numericalData, "data/numericalData_sample10000.csv")
write.csv2(booleanData, "data/booleanData_sample10000.csv")
write.csv2(dateData, "data/dateData_sample10000.csv")
write.csv2(stringData, "data/stringData_sample10000.csv")
write.csv2(sampleTarget, "data/target_sample10000.csv")

#############################################
# END ANALYSIS OF DIFFERENT ATTRIBUTE TYPES
#############################################

# for the following, get the sample with the lowest amount of NAs
naRows = apply(trainData, 1, is.na)
naRows = apply(naRows, 2, sum)
orderIndices = order(naRows)
numericalData = read.csv2("numericalAttributes_cleansed.csv", 
                          stringsAsFactors = FALSE, strip.white = TRUE)

# sampleLowestNA = read.csv2("sampleLowestNA.csv", stringsAsFactors = FALSE, strip.white = TRUE)
numericalData_lowestNA = numericalData[orderIndices[1:10000],]

write.csv2(numericalData_lowestNA, "data/numericalData_sampleLowestNA.csv")
# flush environment to free up some RAM
rm(list = ls())
numericalData_lowestNA = read.csv2("data/numericalData_sampleLowestNA.csv", stringsAsFactors = FALSE,
                                    strip.white = TRUE, sep = ";")
numericalData_lowestNA = apply(numericalData_lowestNA, 2, as.numeric)                                  
#############################################
# CORRELATION ANALYSIS
#############################################

# create correlation heatmap
correlations = cor(numericalData_lowestNA, use="pairwise.complete.obs")
# produces NA values due to the elimination of values used by "pairwise.complete.obs" (but there is no viable alternative)
# eliminate NAs and replace by 0
correlations[is.na(correlations)] = 0

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


png("fig/corrplot.png", height = 800, width = 800)
corrplot(correlations, method = "color", tl.pos = "n", ylab = "", xlab = "", 
         order = "AOE", cl.cex = 3)
dev.off()
# not much to see, plot as barchart

temp = correlations
diag(temp) = NA
correlationVector = as.vector(temp)
correlationVector = sort(correlationVector)
plotData = as.data.frame(cbind(id = seq(1, 10000, by = 1), 
                               values = c(correlationVector[1:5000], 
                                          correlationVector[(length(correlationVector)-4999):length(correlationVector)])))
plotDataAll = as.data.frame(cbind(id = seq_along(correlationVector), 
                               values = correlationVector))

png("fig/correlations_barplot.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Correlation") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

png("fig/correlations_barplot_all.png", height = 800, width = 800)
ggplot(data = plotDataAll, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Correlation") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# try out spearman correlations
correlationsSpearman = cor(numericalData_lowestNA, 
                           use="pairwise.complete.obs", method = "spearman")
correlationsSpearman[is.na(correlationsSpearman)] = 0

png("fig/corrplot_spearman.png", height = 800, width = 800)
corrplot(correlationsSpearman, method = "color", tl.pos = "n", 
         order = "AOE", cl.cex = 3)
dev.off()

temp = correlationsSpearman
diag(temp) = NA
correlationVector = as.vector(temp)
correlationVector = sort(correlationVector)
plotData = as.data.frame(cbind(id = seq(1, 10000, by = 1), 
                               values = c(correlationVector[1:5000], 
                                          correlationVector[(length(correlationVector)-4999):length(correlationVector)])))
plotDataAll = as.data.frame(cbind(id = seq_along(correlationVector), 
                                  values = correlationVector))

png("fig/correlations_barplot_spearman.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Correlation") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

png("fig/correlations_barplot_all_spearman.png", height = 800, width = 800)
ggplot(data = plotDataAll, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Correlation") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# spearman looks similar to pearson, find entries with a correlation of 1

threshold = 1
indices = as.data.frame(which(correlationsSpearman >= threshold, arr.ind = TRUE))
rownames(indices) = NULL
# obviously this includes correlations of variables with itself, eliminate those
for (i in 1:nrow(indices)) {
  if (indices[i,1] == indices[i,2]) {
    indices[i,1] = NA
    indices[i,2] = NA
  }
}
indices = na.omit(indices)
# 76 perfect correlations (includes both sides, so actually 38) 
# same for negative correlations
thresholdNegative = -1
indicesNegative = as.data.frame(which(correlationsSpearman <= thresholdNegative, arr.ind = TRUE))
rownames(indicesNegative) = NULL
# obviously this includes correlations of variables with itself, eliminate those
for (i in 1:nrow(indicesNegative)) {
  if (indicesNegative[i,1] == indicesNegative[i,2]) {
    indicesNegative[i,1] = NA
    indicesNegative[i,2] = NA
  }
}
indicesNegative = na.omit(indicesNegative)
# 20 perfect negative correlations (includes both sides, so actually 10)
# eliminate doubles
source("source/CorrelationHelper.R")
indices = eliminateDuplicateCorrelations(indices)
indicesNegative = eliminateDuplicateCorrelations(indicesNegative)
# some attributes occur multiple times
removeIndices = c(indices[,1], indicesNegative[,1])
length(unique(removeIndices)) # 40
removeIndices = unique(removeIndices)
# -> we can remove 40 attributes in total
removed_highCorrelation = numericalData_lowestNA[,unique(removeIndices)] # for backup
numericalData_lowestNA = numericalData_lowestNA[,-unique(removeIndices)] # 1688 attributes left

write.csv2(numericalData_lowestNA, "numericalDataLowestNA_WithoutCor1.csv")

# run again for checking
cornew = cor(numericalData_lowestNA, use="pairwise.complete.obs", method = "spearman")
# no more correlations higher than 0.9989
# might even be considered to remove 95% correlations, e.g. to speed up further calculations

threshold = 0.95
indices = as.data.frame(which(cornew >= threshold, arr.ind = TRUE))
rownames(indices) = NULL
# obviously this includes correlations of variables with itself, eliminate those
for (i in 1:nrow(indices)) {
  if (indices[i,1] == indices[i,2]) {
    indices[i,1] = NA
    indices[i,2] = NA
  }
}
indices = na.omit(indices)
# 44 hits (so 22)
# same for negative correlations
thresholdNegative = -0.95
indicesNegative = as.data.frame(which(cornew <= thresholdNegative, arr.ind = TRUE))
rownames(indicesNegative) = NULL
# obviously this includes correlations of variables with itself, eliminate those
for (i in 1:nrow(indicesNegative)) {
  if (indicesNegative[i,1] == indicesNegative[i,2]) {
    indicesNegative[i,1] = NA
    indicesNegative[i,2] = NA
  }
}
indicesNegative = na.omit(indicesNegative)
# 84 hits (so 42)

indices = eliminateDuplicateCorrelations(indices)
indicesNegative = eliminateDuplicateCorrelations(indicesNegative)
# some attributes occur multiple times
removeIndices = c(indices[,1], indicesNegative[,1])
length(unique(removeIndices)) # 58
# -> we can remove 58 attributes in total
removed_highCorrelation95 = numericalData_lowestNA[,unique(removeIndices)] # for backup
numericalData_lowestNA = numericalData_lowestNA[,-unique(removeIndices)] # 1688 attributes left

write.csv2(numericalData_lowestNA, "numericalDataLowestNA_WithoutCor95.csv")



#############################################
# END CORRELATION ANALYSIS
#############################################
# impute values for PCA analysis
# pmm does not work (system is computationally singular)
# according to the MICE paper: study last eigenvector of covmat, variables with high values there
# often cause the singularity problem
# this does not work with the sample data, as some entries in the covmat
covmat = cov(numericalData, use = "pairwise.complete.obs")
# covmat contains NAs, do some cheating here since it only concerns 314 entries
covmat[is.na(covmat)] = 0
eigenvectors = eigen(covmat)$vectors
order(eigenvectors[,ncol(eigenvectors)], decreasing = TRUE)
# highest values for attributes 561  564 1548 1292 1743  560  764  956 1469  285
# ignore these attributes in mice to try out if pmm now works
imputedValues = mice(numericalData[,-c(561, 564, 1548, 1292, 1743, 560, 764, 956, 1469, 285)], 
                     method = "pmm") # does not work, exclude more attributes
#805  219 1475  783 1279 1041  844 1432  791 437
imputedValues = mice(numericalData[,-c(561, 564, 1548, 1292, 1743, 560, 764, 956, 1469, 285, 
                                       805, 219, 1475, 783, 1279, 1041, 844, 1432, 791, 437)], 
                     method = "pmm") # still does not work, ignore PMM


# try pmm with the lowest NA sample
# flush environment
rm(list = ls())
numericalData_lowestNA = read.csv2("data/numericalDataLowestNA_WithoutCor1.csv", 
                                   stringsAsFactors = FALSE, strip.white = TRUE)
numericalData_lowestNA = numericalData_lowestNA[,-1]

imputedValues = mice(numericalData_lowestNA, method = "pmm", m = 1, maxit = 1) # does not work 
# system is computationally singular
# try out if maybe removing 95% correlations also works
imputedValues = mice(numericalData_lowestNA, method = "mean", m = 1, maxit = 1)


pca = princomp(imputedValues) 
