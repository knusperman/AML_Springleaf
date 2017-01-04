
#merge data sets and get 20k sample from numeric imputed data. 
set.seed(1234)
s <- sample(1:145232, 145231)
s <- sample(1:145231, 145231)[10001:30000]#sampling from complete training data for numeric data sample

#numericalDataSample <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")
#numericalDataSample <- as.data.frame(numericalDataSample)
data_numeric = as.data.frame(readRDS("data/final/numeric20k_predictionsample.rds"))

data_numeric1 = as.data.frame(readRDS("data/numeric imputations/imp10k.rds"))
data_numeric2 = as.data.frame(readRDS("data/numeric imputations/imp10-30k.rds"))
data_numeric3 = as.data.frame(readRDS("data/numeric imputations/imp30-40k.rds"))
data_numeric4 = as.data.frame(readRDS("data/numeric imputations/imp40-50k.rds"))
data_numeric5 = as.data.frame(readRDS("data/numeric imputations/imp50-70k.rds"))
data_numeric6 = as.data.frame(readRDS("data/numeric imputations/imp70-90k.rds"))
data_numeric7 = as.data.frame(readRDS("data/numeric imputations/imp90-110k.rds"))
data_numeric8 = as.data.frame(readRDS("data/numeric imputations/imp110-130k.rds"))
data_numeric9 = as.data.frame(readRDS("data/numeric imputations/imp130k+.rds"))

data_numeric = rbind(data_numeric1,data_numeric2,data_numeric3,data_numeric4,data_numeric5,data_numeric6,data_numeric7, data_numeric8,data_numeric9)
data_numeric = data_numeric[-c(130001),]
remove(data_numeric1, data_numeric2,data_numeric3,data_numeric4,data_numeric5,data_numeric6,data_numeric7,data_numeric8,data_numeric9)

data_factors = as.data.frame(readRDS("data/final/factorAttributes.rds"))[rownames(data_numeric),] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(data_numeric),]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(data_numeric),] #f
data_target  = as.data.frame(read.csv("data/target.csv"))[rownames(data_numeric),] #f #full train records


#collist = list("cols_numeric"=colnames(data_numeric),"cols_factors"=colnames(data_factors),"cols_strings" = colnames(data_strings), "cols_dates" = colnames(data_dates))
#saveRDS(collist, "data/collist.rds")

NAstatistics <- readRDS("data/NAstatistics.rds") #since data has no more information about amout of imputation in a column
collist <- readRDS("data/collist.rds") #some data for selecting the right columns for predicting

mydata <- cbind(data_numeric,data_factors,data_strings,data_dates, data_target)

colnames(mydata)[ncol(mydata)]="target"
mydata$target <- as.factor(mydata$target)

dim(mydata)
#cleaning the environment
remove(data_numeric)
remove(data_factors)
remove(data_strings)
remove(data_dates)
remove(data_target)

#remove cols you do not want right now
mydata <- mydata[,-which(colnames(mydata) %in% collist$cols_dates)]
