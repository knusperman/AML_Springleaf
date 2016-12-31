# build train data set
numericalData = readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")
booleanData = readRDS("data/booleanAttributes_cleansed.rds")
factorData = readRDS("data/factorAttributes.rds")
stringData = readRDS("data/stringAttributes_cleansed.rds")
dateData = readRDS("data/dateAttributes_cleansed.rds")
target = readRDS("data/target.rds")

# first approach: Do learning only based on attributes without NAs
# sort columns by NAs
data = as.data.frame(cbind(numericalData, booleanData, stringData, dateData, factorData))

numLength = ncol(numericalData)
boolLength = numLength + ncol(booleanData)
strLength = boolLength + ncol(stringData)
dateLength = strLength + ncol(dateData)

#ensure datatype correctness
for (i in 1:numLength) {
  data[,i] = as.numeric(data[,i])
}
for (i in (numLength+1):boolLength) {
  data[,i] = as.logical(data[,i])
}
for (i in (boolLength+1):strLength) {
  data[,i] = as.factor(data[,i], levels = unique(data[,i]))
}
for (i in (strLength+1):dateLength) {
  data[,i] = as.Date()
}

nas = apply(data, 2, function(x) {
  sum(is.na(x))
}) 

# get complete obs
completeIndices = complete.cases(lowNAsData)
lowNAsData = lowNAsData[completeIndices,]
nrow(lowNAsData) #133543

target = target[completeIndices]
data = cbind(lowNAsData, target = as.factor(target))

# train with attributes with less than 1000 NAs
sum(nas < 1000) #938
lowNAsData = data[,which(nas < 1000)]

# remove factors with more than 53 levels; randomforest cannot handle them
above53Factors = apply(data, 2, function(x) {
  if (class(x) == "factor" & length(levels(x)) > 53) return(TRUE)
  else return(FALSE)
})

test = apply(data, 2, levels)
test2 = apply(data, 2, class)

model = randomForest(target[1:10000] ~ ., data = data[1:10000,])
