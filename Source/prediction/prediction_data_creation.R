
#merge data sets and get 20k sample from numeric imputed data. 
set.seed(1234)
s <- sample(1:145231, 145231)[10001:30000]#sampling from complete training data for numeric data sample

data_numeric = as.data.frame(readRDS("data/final/numeric20k_predictionsample.rds"))
data_factors = as.data.frame(readRDS("data/final/factorAttributes.rds"))[s,] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[s,]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[s,] #f
data_target  = as.data.frame(read.csv("data/target.csv"))[s,] #f #full train records

#some data for selecting the right columns for predicting
cols_numeric = colnames(data_numeric)
cols_factors = colnames(data_factors)
cols_strings = colnames(data_strings)
cols_dates = colnames(data_dates)
NAstatistics <- readRDS("data/NAstatistics.rds") #since data has no more information about amout of imputation in a column

mydata <- cbind(data_numeric,data_factors,data_strings,data_dates,data_target)
colnames(mydata)[ncol(mydata)]="target"
mydata$target <- as.factor(mydata$target)

dim(mydata)

remove(data_numeric)
remove(data_factors)
remove(data_strings)
remove(data_dates)
remove(data_target)



