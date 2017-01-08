n1 = readRDS("data/numeric imputations/impsplit1_done1.rds")
n2 = readRDS("data/numeric imputations/impsplit1_done2.rds")
n3 = readRDS("data/numeric imputations/impsplit2_done1.rds")
n4 = readRDS("data/numeric imputations/impsplit2_done2.rds")
n5 = readRDS("data/numeric imputations/impsplit3_done1.rds")
n6 = readRDS("data/numeric imputations/impsplit3_done2.rds")

train_imputed = rbind(n1,n2,n3, n4, n5, n6)
train_imputed = train_imputed[complete.cases(train_imputed),]

pcaRes = prcomp(train_imputed)

pcaResNormalized = prcomp(train_imputed, scale. = TRUE) # use this for further analysis


# do some plotting
plotData = as.data.frame(cbind(id = seq_along(pcaRes$sdev), sdev = pcaRes$sdev))
png("fig/pcaSdev.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = sdev)) + 
  geom_line() + xlab("Attributes") + ylab("Standard deviation") +
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

plotData = as.data.frame(cbind(id = seq_along(pcaRes$sdev), sdev = cumsum(pcaRes$sdev)))
png("fig/pcaSdev_cumsum.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = sdev)) + 
  geom_line() + xlab("Attributes") + ylab("Standard deviation") +
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

plotData = as.data.frame(cbind(id = seq_along(pcaResNormalized$sdev), sdev = pcaResNormalized$sdev))
png("fig/pcaSdev_normalized.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = sdev)) + 
  geom_line() + xlab("Attributes") + ylab("Standard deviation") +
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

plotData = as.data.frame(cbind(id = seq_along(pcaResNormalized$sdev), sdev = cumsum(pcaResNormalized$sdev)))
png("fig/pcaSdev_cumsum_normalized.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = sdev)) + 
  geom_line() + xlab("Attributes") + ylab("Standard deviation") +
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()

PCs = readRDS("data/PCA_PCs.rds")
sdev = readRDS("data/PCA_sdev.rds")

sumsdev = sum(sdev) * 0.8
which(cumsum(sdev) > sumsdev)[1] #453
# we can use the first 453 PCs to explain 80% of the variance
# transform data
train_transformed = pcaResNormalized$x[,1:453]
saveRDS(train_transformed, "data/PCA_transformed_set.rds")

# try prediction based on our transformed data
library(xgboost)
library(mlr)

data_factors = as.data.frame(readRDS("data/final/factorAttributes_FINAL.rds"))[rownames(train_transformed),] #full train records. no NAs b/c treated as level
data_strings = as.data.frame(readRDS("data/final/stringData_FINAL.rds"))[rownames(train_transformed),]
data_dates   = as.data.frame(readRDS("data/final/dateData_FINAL.rds"))[rownames(train_transformed),] #f
data_boolean = as.data.frame(readRDS("data/final/booleanAttributes_FINAL.rds"))[rownames(train_transformed),]
data_target  = as.data.frame(readRDS("data/target.rds"))[rownames(train_transformed),] #f #full train records
mydata <- cbind(train_transformed,data_factors,data_strings,data_dates,data_boolean, data_target)
colnames(mydata)[ncol(mydata)]="target"

mydata_sample = mydata[sample(1:nrow(train_transformed), 45000),]


classif.task = makeClassifTask(id = "mtc", data = mydata_sample, target = "target", positive="1")

set.seed(1234)
n = getTaskSize(classif.task) #size of data
train.set = sample(n, size = n*0.9)
test.set = 1:n
test.set <- test.set[-which(test.set %in% train.set)]
