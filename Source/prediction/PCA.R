n1 = readRDS("data/numeric imputations/impsplit1_done1.rds")
n2 = readRDS("data/numeric imputations/impsplit1_done2.rds")
n3 = readRDS("data/numeric imputations/impsplit2_done1.rds")
n4 = readRDS("data/numeric imputations/impsplit2_done2.rds")
n5 = readRDS("data/numeric imputations/impsplit3_done1.rds")
n6 = readRDS("data/numeric imputations/impsplit3_done2.rds")

train_imputed = rbind(n1,n2,n3, n4, n5, n6)
train_imputed = train_imputed[complete.cases(train_imputed),]
train_imputed_normalized = train_imputed[complete.cases(train_imputed),]
for (i in 1:ncol(train_imputed)) {
  train_imputed_normalized[,i] = (train_imputed_normalized[,i]-mean(train_imputed_normalized[,i]))/sd(train_imputed_normalized[,i])
}

pcaRes = prcomp(train_imputed[complete.cases(train_imputed),], cor = TRUE) # use this for further analysis

pcaResNormalized = prcomp(train_imputed_normalized)

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


pcaRes = readRDS("data/PCA_res.rds")
