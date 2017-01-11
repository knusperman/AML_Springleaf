testData = as.data.frame(read.csv("data/test.csv", stringsAsFactors = FALSE, strip.white = TRUE))
source("source/dataCleaning/ConvertNAs_Functions.R")
collist = readRDS("data/collist.rds")
testData = testData[, which(colnames(testData) %in% collist$cols_extranumerics)] #subset to relevant cols
naEncodings = c(-99999, 1e+09, 99, 9999, 100, 9996, 9998, 98,999999999, 999999998, 999999997, 999999996, 999999995, 999999994)
testData = convertObviousNAs(testData)
for (i in 1:ncol(testData)) {
  testData[,i] = gsub(" ", "", testData[,i])
}
testData = convertNAsFaster(testData, naEncodings)
testData <- as.data.frame(testData,stringsAsFactors=FALSE)
for(i in 1:ncol(testData)){
  testData[,i] = as.character(testData[,i])
  testData[,i] = as.numeric(testData[,i])
}

# median imputation
for (i in 1:ncol(testData)) {
  testData[is.na(testData[,i]),i] = median(na.omit(testData[,i]))
}
saveRDS(testData,"data/TESTmedianImputedExtraNumerics_FINAL.rds")

