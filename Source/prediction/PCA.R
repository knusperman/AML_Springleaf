n1 <- readRDS("data/numeric imputations/imp10k.rds") #1-10000
n2<- readRDS("data/numeric imputations/imp10-30k.rds") #10001-30000 :::: used as PREDICTION SAMPLE
n3 <- readRDS("data/numeric imputations/imp30-40k.rds") #30001-40000
n4 <- readRDS("data/numeric imputations/imp40-50k.rds") # 40001 - 50000
n5 <- readRDS("data/numeric imputations/imp50-70k.rds") #50001 - 70000
n6 <- readRDS("data/numeric imputations/imp70-90k.rds") #700001 - 90000
n7 <- readRDS("data/numeric imputations/imp110-130k.rds") #11001 - 130000
n8 <- readRDS("data/numeric imputations/imp130k-end.rds") 
train_imputed <-rbind(n1,n2,n3,n4,n5,n6,n7,n8)
train_imputed_normalized = train_imputed[complete.cases(train_imputed),]
for (i in 1:ncol(train_imputed)) {
  train_imputed_normalized[,i] = (train_imputed_normalized[,i]-mean(train_imputed_normalized[,i]))/sd(train_imputed_normalized[,i])
}

pcaRes = princomp(train_imputed[complete.cases(train_imputed),], cor = TRUE)
pcaResNormalized = princomp(train_imputed_normalized, cor = FALSE)
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