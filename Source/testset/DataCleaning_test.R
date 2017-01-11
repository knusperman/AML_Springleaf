# this is essentially a trimmed version of DataCleaning.R, including elements of DateDataAnalysis, StringDataAnalysis and BooleanDataAnalysis
# for brevity, these cleaning procedures are not split into differente files again
# since the test set has to be cleaned in the same way to ensure consistency, mostly code is repeated here
testData = as.data.frame(read.csv("data/test.csv", stringsAsFactors = FALSE, strip.white = TRUE))

source("source/dataCleaning/ConvertNAs_Functions.R")
testData = convertObviousNAs(testData)
for (i in 1:ncol(testData)) {
  testData[,i] = gsub(" ", "", testData[,i])
}
testData = testData[,-1]
removeOneFactor = readRDS("data/REMOVE_oneFactor.rds")
testData = testData[,-removeOneFactor]
naEncodings = c(-99999, 1e+09, 99, 9999, 100, 9996, 9998, 98, 
                999999999, 999999998, 999999997, 999999996, 999999995, 999999994)
testData = convertNAsFaster(testData, naEncodings)

trainData[,moreBooleanColumns] = convert01Booleans(trainData[,moreBooleanColumns])

moreBooleanColumns = apply(trainData, 2, function(x) {length(unique(x))}) == 2 
collist = readRDS("data/collist.rds")
saveRDS(testData,"data/testBackup1.rds")
collist <- readRDS("data/collist.rds")
booleanColumns = which(colnames(testData) %in% collist$cols_boolean)
dateColumns = which(colnames(testData) %in% c("VAR_0075","VAR_0217", "VAR_0204",collist$cols_dates))
stringColumns = which(colnames(testData) %in% collist$cols_strings)
numericalColumns =which(colnames(testData) %in% collist$cols_numeric)
factorColumns = which(colnames(testData) %in% collist$cols_factors)
testBoolean = testData[,booleanColumns] 
testString = testData[,stringColumns]
testDate = testData[,dateColumns]
testFactor = testData[,factorColumns]
testNumerical = testData[,numericalColumns]
for (i in 1:ncol(testNumerical)) testNumerical[,i] = as.numeric(testNumerical[,i])
killedNumericsNA = readRDS("data/killedNumericsNA.rds")
testNumerical = testNumerical[,!colnames(testNumerical) %in% killedNumericsNA]
##############################################

##############################################
# FACTOR DATA
factorPotentials_uniqueNumericalValues = 
  readRDS("data/factorPotentials.rds") # result of considerations of DatatypesAnalysis.R
factorColumns = which(colnames(testNumerical) %in% rownames(factorPotentials_uniqueNumericalValues))
testFactor = as.data.frame(testFactor,stringsAsFactors = FALSE)
for (i in 1:ncol(testFactor)) {
  testFactor[,i] = as.numeric(testFactor[,i])
  maximum = max(na.omit(testFactor[,i]))
  testFactor[is.na(testFactor[,i]),i] = maximum + 1
  testFactor[,i] = as.character(  testFactor[,i])
}
saveRDS(testFactor, "data/final/TESTfactors_char.rds")
testNumerical = as.data.frame(testNumerical[,-factorColumns],stringsAsFactors=FALSE)
for(i in 1:ncol(testNumerical)){
  testNumerical[,i] =as.numeric(testNumerical[,i])
}

##############################################
# NUMERICAL DATA
# numerical data has to be imputed
source("source/imputation/mi_imputation_helperFunctions.R")
naCorMat <- getMissingnesPatternCorMat(testNumerical)
pearson = cor(testNumerical, use = "pairwise.complete.obs")
pearson[is.na(pearson)] = 0
miNACorMat <- buildMiceMatrix(pearson,5,naCorMat,0.7)
row.names(miNACorMat) = colnames(testNumerical)
colnames(miNACorMat) = colnames(testNumerical)
miMatrix <- miNACorMat

testNumerical_imputed <- testNumerical
seq = 1:ncol(testNumerical)
###### adjust i up to ncol(df) on all computing devices.
errors = numeric(0)
for(i in seq){
  imp = tryCatch({
    imp <- createimputation(testNumerical, testNumerical_imputed, i)
  }, error = function(e) e)
  if (inherits(imp, "error")) {
    errors = c(errors, i)
    next
  }
  if(class(imp)=="data.frame"){
    testNumerical_imputed[rownames(imp), i] = imp[,1]
  }
}

# if still some NAs left:
# cheap workaround: NAs = median of the attribute
for (i in 1:ncol(testNumerical_imputed)) {
  testNumerical_imputed[is.na(testNumerical_imputed[,i]),i] = median(na.omit(testNumerical_imputed[,i]))
}

######################################################

testBoolean = as.data.frame(testBoolean)
for (i in 1:ncol(testBoolean)) {
  testBoolean[,i] = as.character(testBoolean[,i])
}
twoValues = which(apply(testBoolean, 2, function(x) length(unique(x))) == 2)


# find one value and NAs
oneValueAndNAColumns = findOneValueAndNAs(testBoolean)
testBoolean[,oneValueAndNAColumns] = convertOneValueAndNA(testBoolean[,oneValueAndNAColumns])


nas = which(apply(testBoolean, 2, function(x) {sum(is.na(x)) > 0}))
for (i in nas) {
  testBoolean[,i] = as.character(testBoolean[,i])
  testBoolean[is.na(testBoolean[,i]),i] = "2"
  testBoolean[(testBoolean[,i] == TRUE),i] = "1"
  testBoolean[(testBoolean[,i] == FALSE),i] = "0"
  testBoolean[,i] = factor(testBoolean[,i], levels = unique(testBoolean[,i]))
}
mysavegame = testBoolean
for (i in 1:ncol(testBoolean)) {
  if (!i %in% nas) {
    if (testBoolean[1,i] %in% c("0", "1")) testBoolean[,i] = as.logical(as.numeric(testBoolean[,i]))
    else testBoolean[,i] = as.logical(testBoolean[,i])
  }
}


saveRDS(testBoolean,"data/final/TESTboolean.rds")
######################################################
saveRDS(testString, "data/final/todoTESTstrings.rds")
saveRDS(testDate, "data/final/todoTESTdate.rds")

###Gold 1, übernehmen Sie!
# Achtung, die Daten sind schon auf die relevanten Columns über das Training-Sample reduziert.  
# STRING DATA
#testString <-readRDS("data/final/todoTESTstrings.rds")
#testDate <-readRDS("data/final/todoTESTdate.rds")

testString = as.data.frame(testString)
for (i in 1:ncol(testString)) testString[,i] = as.character(testString[,i])

#testString = testString[,-c(3, 14, 16, 4)] #das dürfe nicht mehr nötig sein. 
stateFilter1 = readRDS("data/stateFilter1.rds") #die fehlen mir.
stateFilter2 = readRDS("data/stateFilter2.rds")
testString[testString[,3] %in% rownames(stateFilter1),3] = "other"
testString[testString[,4] %in% rownames(stateFilter2),4] = "other"

# for all: convert NAs into another factor level
for (i in 1:ncol(testString)) {
  testString[is.na(testString[,i]),i] = "na"
}
# factorize
for (i in 1:ncol(testString)) {
  testString[,i] = factor(testString[,i], levels = unique(testString[,i]))
}
saveRDS(testString,"data/final/TESTstrings.rds")
######################################################
# DATE DATA
savedates <- testDate
source("Source/setBuilding/Date_Functions.R")
for(i in 1:ncol(testDate)){
  testDate[,i] <- as.character(testDate[,i])
}
monthData = extractDateData(testDate, "m")
dayData = as.data.frame(extractDateData(testDate, "d"))
for (i in 1:ncol(dayData)) {
  dayData[,i] = factor(dayData[,i], levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                               "Saturday", "Sunday"))
}
yearData = extractDateData(testDate, "y")

relevantDateData = cbind(yearData[,2], yearData[,16], monthData[,2], monthData[,15], monthData[,16],
                         dayData[,2], dayData[,15], dayData[,16])
colnames(relevantDateData) = c("VAR_0075_YEAR", "VAR_0217_YEAR", "VAR_0075_MONTH", "VAR_0204_MONTH", 
                               "VAR_0217_MONTH", "VAR_0075_DAY", "VAR_0204_DAY", "VAR_0217_DAY")
relevantDateData[is.na(relevantDateData)]="NA"
otherDates = testDate[,-c(2, 15, 16)]
otherDates = as.data.frame(otherDates)

for (i in 1:ncol(otherDates)) {
  otherDates[,i] = as.character(otherDates[,i])
  otherDates[is.na(otherDates[,i]),i] = "0"
  otherDates[!otherDates[,i] == "0",i] = "1"
  otherDates[,i] = as.logical(as.numeric(otherDates[,i]))
}

relevantDateData = cbind(as.data.frame(relevantDateData,stringsAsFactors = FALSE), otherDates)
testDate = as.data.frame(relevantDateData,stringsAsFactors=FALSE)
saveRDS(testDate, "data/final/TESTdates.rds")
dataset = as.data.frame(cbind(testNumerical_imputed, testFactor, testBoolean, testString, testDate, target = target))