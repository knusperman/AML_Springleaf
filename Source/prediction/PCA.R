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
