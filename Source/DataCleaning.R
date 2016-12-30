if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"reshape2" %in% installed.packages()) install.packages("reshape2")
if (!"corrplot" %in% installed.packages()) install.packages("corrplot")
if (!"mice" %in% installed.packages()) install.packages("mice")
if (!"randomForest" %in% installed.packages()) install.packages("randomForest")
if (!"missForest" %in% installed.packages()) install.packages("missForest")
library(ggplot2)
library(reshape2)
library(corrplot)
library(mice)
library(randomForest)
library(missForest)

trainData = read.csv("data/train.csv", stringsAsFactors = FALSE, strip.white = TRUE)
ncol(trainData) # initially 1934 columns
# still some white space remains for some reason, entirely remove all whitespace
trainData = apply(trainData, 2, function(x) {gsub(" ", "", x)})

source("source/ConvertNAs_Functions.R")
trainData = convertObviousNAs(trainData)

saveRDS(trainData, "data/check1.rds") # backup
# trainData = readRDS("data/check1.rds")

# remove ID column
trainData = trainData[,-1]

# remove target column
target = trainData[,ncol(trainData)]
target = saveRDS(target, "data/target.rds")
trainData = trainData[,-ncol(trainData)]

# perform some initial manual analysis of data
oneFactor = as.vector(which(sapply(apply(trainData, 2, unique), length) == 1)) #8 occurrences
saveRDS(oneFactor, "data/REMOVE_oneFactor.rds")

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
ncol(trainData) # 1924

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
booleanColumns = findBooleans(trainData) 
sum(booleanColumns) #13
dateColumns = findDates(trainData)
sum(dateColumns) #16
stringColumns = findStrings(trainData) 
stringColumns = stringColumns & !booleanColumns & !dateColumns 
sum(stringColumns) #50
numericalColumns = !(stringColumns | booleanColumns | dateColumns) 
sum(numericalColumns) #1874
saveRDS(cbind(booleanColumns, dateColumns, stringColumns, numericalColumns), 
        "attributes1.rds")

# convert datatypes
# convert to logical in trainData
for (i in 1:ncol(trainData)) {
  if (booleanColumns[i] == TRUE) trainData[,i] = as.logical(trainData[,i])
}

# convert to numeric in trainData
for (i in 1:ncol(trainData)) {
  if (numericalColumns[i] == TRUE) trainData[,i] = as.numeric(trainData[,i])
}

#############################################
# NA ANALYIS (consult file ConvertNAs.R for further details on the analysis)
#############################################
naEncodings = c(-99999, 1e+09, 99, 9999, 100, 9996, 9998, 98, 
                999999999, 999999998, 999999997, 999999996, 999999995, 999999994)
trainData = convertNAsFaster(trainData, naEncodings)

saveRDS(trainData, "data/check2.rds") # backup
# trainData = readRDS("data/check2.rds")

# find attributes that only have NA and one value
oneValueAndNAColumns = findOneValueAndNAs(trainData) 
sum(oneValueAndNAColumns) # 52

# convert these into boolean, NA translated to FALSE
trainData[,oneValueAndNAColumns] = convertOneValueAndNA(trainData[,oneValueAndNAColumns])

# add to boolean columns
booleanColumns = booleanColumns | oneValueAndNAColumns 
sum(booleanColumns) # 56

#############################################
# HANDLING OF DIFFERENT DATATYPES
#############################################

# some attributes only seem to have two distinct values
# encode these as boolean
moreBooleanColumns = apply(trainData, 2, function(x) {length(unique(x))}) == 2 
sum(moreBooleanColumns) # 63

# eliminate duplicates with booleanColumns
moreBooleanColumns = moreBooleanColumns & (!booleanColumns) 
sum(moreBooleanColumns) # 11
unique(rapply(trainData[,moreBooleanColumns], unique)) # only 0, 1

# convert to TRUE/FALSE notion
trainData[,moreBooleanColumns] = convert01Booleans(trainData[,moreBooleanColumns])

# append to boolean data
booleanColumns = booleanColumns | moreBooleanColumns
sum(booleanColumns) # 67

numericalColumns = numericalColumns & !booleanColumns
stringColumns = stringColumns & !booleanColumns

sum(numericalColumns) # 1824
sum(booleanColumns) # 67
sum(dateColumns) # 16
sum(stringColumns) # 17

saveRDS(trainData[,booleanColumns], "data/booleanAttributes_cleansed.rds")
saveRDS(trainData[,dateColumns], "data/dateAttributes_cleansed.rds")
saveRDS(trainData[,stringColumns], "data/stringAttributes_cleansed.rds")
saveRDS(trainData[,numericalColumns], "data/numericalAttributes_cleansed.rds")

#booleanData = readRDS("data/booleanAttributes_cleansed.rds")
#dateData = readRDS("data/dateAttributes_cleansed.rds")
#stringData = readRDS("data/stringAttributes_cleansed.rds")
#numericalData = readRDS("data/numericalAttributes_cleansed.rds")

#############################################
# END ANALYSIS OF DIFFERENT ATTRIBUTE TYPES
#############################################

# for the following, get the sample with the lowest amount of NAs in the numerical data
numericalData = readRDS("data/numericalAttributes_cleansed.rds")
naRows = apply(numericalData, 1, is.na)
naRows = apply(naRows, 2, sum)
orderIndices = order(naRows)

# sampleLowestNA = read.csv2("sampleLowestNA.csv", stringsAsFactors = FALSE, strip.white = TRUE)
numericalData_lowestNA = numericalData[orderIndices[1:10000],]

saveRDS(numericalData_lowestNA, "data/numericalData_sampleLowestNA.rds")
numericalData_lowestNA = apply(numericalData_lowestNA, 2, as.numeric)                                  
#############################################
# CORRELATION ANALYSIS
#############################################

# create correlation heatmap
# use numericalData_lowestNA as a proxy for the real correlations (taking the entire 
# dataset simply takes too long, especially for spearman)
correlations = cor(numericalData_lowestNA, use="pairwise.complete.obs")
# produces NA values due to the elimination of values used by "pairwise.complete.obs" (but there is no viable alternative)
# eliminate NAs and replace by 0
correlations[is.na(correlations)] = 0
saveRDS(correlations, "data/pearson.rds")

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

createCorrelationPlots(correlations, "_pearson")

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
saveRDS(correlationsSpearman, "data/spearman.rds")
# correlationsSpearman = readRDS("data/spearman.rds")

createCorrelationPlots(correlations, "_spearman")

png("fig/correlations_barplot_all_spearman.png", height = 800, width = 800)
ggplot(data = plotDataAll, aes(x = id, y = values)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Correlation") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# spearman looks similar to pearson, find entries with a correlation of 1 for pearson
# we are on the safe side if we kick out pearson cors of 1
# everything that has pearson 1, also has spearman 1

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
# 90 perfect correlations (includes both sides, so actually 45) 
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

# 26 perfect negative correlations (includes both sides, so actually 13)
# eliminate doubles
source("source/CorrelationHelper.R")
indices = eliminateDuplicateCorrelationsMoreNAs(indices, numericalData)
indicesNegative = eliminateDuplicateCorrelationsMoreNAs(indicesNegative, numericalData)
# some attributes occur multiple times
removeIndices = c(indices, indicesNegative)
length(unique(removeIndices)) # 49
removeIndices = unique(removeIndices)
saveRDS(removeIndices, "data/REMOVE_1Cor.rds")
# -> we can remove 49 attributes in total
numericalData_lowestNA = numericalData_lowestNA[,-removeIndices]

numericalData = numericalData[,-removeIndices]
saveRDS(numericalData, "data/numericalData_withoutCor1.rds")
numericalData = readRDS("data/numericalData_withoutCor1.rds")

numericalData_lowestNA = readRDS("data/numericalData_sampleLowestNA.rds")
numericalData_lowestNA = numericalData_lowestNA[,-removeIndices]
numericalData_lowestNA = apply(numericalData_lowestNA, 2, as.numeric)
saveRDS(numericalData_lowestNA, "data/numericalData_sampleLowestNA_withoutCor1.rds")

# compute new spearman correlation for value imputation (with removed cor = 1)
correlationsSpearman = cor(numericalData_lowestNA, 
                           use="pairwise.complete.obs", method = "spearman")
correlationsSpearman[is.na(correlationsSpearman)] = 0
saveRDS(correlationsSpearman, "data/spearman_without1.rds")
length(which(correlationsSpearman == 1))

#############################################
# END CORRELATION ANALYSIS
#############################################

#############################################
# HANDLING OF FACTORS (consult file DatatypesAnalysis.R for further details)
#############################################
factorPotentials_uniqueNumericalValues = 
  readRDS("data/factorPotentials.rds") # result of considerations of DatatypesAnalysis.R
factorColumns = which(colnames(numericalData) %in% rownames(factorPotentials_uniqueNumericalValues))
factorData = numericalData[,factorColumns]
sum(is.na(factorData)) / (ncol(factorData) * nrow(factorData)) # 0.1402106
sum(is.na(numericalData)) / (ncol(numericalData) * nrow(numericalData)) # 0.2266638
factorData = apply(factorData, 2, as.numeric) # somehow not all columns are saved as numeric
# -> much lower NA percentage in factor data
# encode factor NAs simply as highest factor level + 1
factorData = apply(factorData, 2, function(x) {
  maximum = max(na.omit(x))
  x[is.na(x)] = maximum + 1
  x = as.factor(x)
})
sum(is.na(factorData)) / (ncol(factorData) * nrow(factorData)) # 0
numericalData = numericalData[,-factorColumns]
numericalData_lowestNA = numericalData_lowestNA[,-factorColumns]
factorData_lowestNA = numericalData_lowestNA[,factorColumns]

saveRDS(numericalData, "data/numericalAttributes_cleansed_withoutFactors.rds")
saveRDS(factorData, "data/factorAttributes.rds")

# re-do pearson to check if it looks any different

amountOfNAs = apply(numericalData, 1, function(x) {
  sum(is.na(x))
})
lowest10000 = order(amountOfNAs)[1:10000]
numericalData = apply(numericalData, 2, as.numeric)

correlations = cor(numericalData_lowestNA, use="pairwise.complete.obs")
correlations[is.na(correlations)] = 0
saveRDS(correlations, "data/pearson_withoutFactors.rds")

createCorrelationPlots(correlations, "_pearsonNoFactors")
# looks similar, dont re-do this for spearman (takes longer)

#############################################
# END HANDLING OF FACTORS
#############################################

# impute values for PCA analysis
# pmm does not work (system is computationally singular)
# according to the MICE paper: study last eigenvector of covmat, variables with high values there
# often cause the singularity problem
# this still does not work for our data
# other approach (to also speed up the imputation: only use the 100 attributes to impute values
# for one attribute with the highest correlation)
# use spearman
# build usage matrix for mice
# numericalData_lowestNA = readRDS("data/numericalData_sampleLowestNA.rds")
# correlationsSpearman = readRDS("data/spearman_without1.rds")
# numericalData_noCor1 = readRDS("data/numericalData_withoutCor1.rds")
# compute pearson for miceMatrix

miceMatrix = buildMiceMatrix(correlations, usedAttributes = 10)
numberOfObs = 1000
imputedValues = mice(numericalData[1:numberOfObs,], predictorMatrix = miceMatrix, method = "fastpmm")
# system is computationally singular
# try to impute numerical values with factor attributes
predictorMatrix = matrix(ncol = (ncol(numericalData) + ncol(factorData)), 
                         nrow = ncol(numericalData) + ncol(factorData))
for (i in 1:ncol(numericalData)) {
  predictorMatrix[i,] = c(rep(0, ncol(numericalData)), rep(1, ncol(factorData)))
} 
for (i in (ncol(numericalData)+1):(ncol(numericalData)+ncol(factorData))) {
  predictorMatrix[i,] = c(rep(1, ncol(numericalData)), rep(0, ncol(factorData)))
}
imputedValues = mice(cbind(numericalData[1:numberOfObs,], factorData[1:numberOfObs,]), 
                     predictorMatrix = predictorMatrix, method = "fastpmm")


# mice takes ridiculously long
imputedValues = missForest(numericalData)

saveRDS(imputedValues, "imputedValues.RDS")

 
# try pmm with the lowest NA sample
# flush environment
# rm(list = ls())

pca = princomp(imputedValues) 
