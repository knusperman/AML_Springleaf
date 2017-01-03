n1 <- readRDS("data/numeric imputations/imp10k.rds") #1-10000
n2<- readRDS("data/numeric imputations/imp10-30k.rds") #10001-30000 :::: used as PREDICTION SAMPLE
n3 <- readRDS("data/numeric imputations/imp30-40k.rds") #30001-40000
n4 <- readRDS("data/numeric imputations/imp40-50k.rds") # 40001 - 50000
n5 <- readRDS("data/numeric imputations/imp50-70k.rds") #50001 - 70000
n6 <- readRDS("data/numeric imputations/imp70-90k.rds") #700001 - 90000
n7 <- readRDS("data/numeric imputations/imp110-130k.rds") #11001 - 130000
n8 <- readRDS("data/numeric imputations/imp130k-end.rds") 
train_imputed <-rbind(n1,n2,n3,n4,n5,n6,n7,n8)

#perform imputation ...

which(sapply(n3, function(x)sum(is.na(x)))>0)
df_imputed[rownames(n3),] ->n3i
saveRDS(n3i, "data/numeric imputations/imp40-50k.rds")

