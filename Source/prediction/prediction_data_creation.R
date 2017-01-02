
#merge data sets
NA_statistics = as.data.frame(readRDS("data/NAstatistics.rds"))
data_numeric = as.data.frame(readRDS("data/num10k_1.rds"))
data_numeric$VAR_1444 = NULL 
data_numeric$VAR_1445 = NULL

data_numeric_merged <- (data_numeric)
data_factors = as.data.frame(readRDS("data/factorAttributes.rds")) #full train records  no NAs b/c treated as level
data_target = data.frame(read.csv("data/target.csv")) #full train records

data_factors <- data_factors[as.integer(rownames(data_numeric_merged)),]
data_target <- data_target[as.integer(rownames(data_numeric_merged)),]

mydata <- cbind(data_numeric_merged,data_factors,data_target)
remove(data_numeric)
remove(data_factors)
remove(data_target)
colnames(mydata)[ncol(mydata)]="target"

mydata$target <- as.factor(mydata$target)

dim(mydata)
