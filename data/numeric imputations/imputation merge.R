n1 <- readRDS("data/numeric imputations/imp10k.rds") #1-10000
n2 <- readRDS("data/numeric imputations/imp10-30k.rds") #10001-30000 :::: used as PREDICTION SAMPLE
n3 <- readRDS("data/numeric imputations/imp30-40k.rds") #30001-40000
n4 <- readRDS("data/numeric imputations/imp40-50k.rds") # 40001 - 50000
n5 <- readRDS("data/numeric imputations/imp50-70k.rds") #50001 - 70000
n6 <- readRDS("data/numeric imputations/imp70-90k.rds") #700001 - 90000
n7 <- readRDS("data/numeric imputations/imp90-110k.rds") #90001-11000<<<<
n8 <- readRDS("data/numeric imputations/imp110-130k.rds") #110001 - 130000
n9 <- readRDS("data/numeric imputations/imp130k+.rds") #130001 - 145231
# n9 has one too much (130k. obs double)
n9 = n9[-1,]
train_imputed <-rbind(n1,n2,n3,n4,n5,n6,n7,n8,n9)
saveRDS(train_imputed, "data/final/numericalData_FINAL.rds")
remove(n1,n2,n3,n4,n5,n6,n7,n8,n9)

booleanData = readRDS("data/final/booleanAttributes_FINAL.rds")
stringData = readRDS("data/final/stringData_FINAL.rds")
dateData = readRDS("data/final/dateData_FINAL.rds")
target = readRDS("data/target.rds")
factorData = readRDS("data/final/factorAttributes_FINAL.rds")

dataset = as.data.frame(cbind(train_imputed, factorData, booleanData, stringData, dateData, target = target))
saveRDS(dataset, "data/final/cleansedDataset.rds")
