
#merge data sets
NA_statistics = as.data.frame(readRDS("data/NAstatistics.rds"))
data_numeric = as.data.frame(readRDS("data/numerics_sample_imputed.rds")) #10k sample no NAs
data_factors = as.data.frame(readRDS("data/factorAttributes.rds")) #full train records  no NAs b/c treated as level
data_target = data.frame(read.csv("data/target.csv")) #full train records

set.seed(1234) #get same sample of factor attributes, target
s = sample(1:nrow(data_factors),10000)
data_factors <- data_factors[s,]
data_target <- data_target[s,]

mydata <- cbind(data_numeric,data_factors,data_target)
remove(data_numeric)
remove(data_factors)
remove(data_target)
colnames(mydata)[ncol(mydata)]="target"

mydata$target <- as.factor(mydata$target)

