library(MASS)
samples = mvrnorm(n = 100, mu = c(1,1), Sigma = matrix(c(1, 0.9, 0.9, 1), byrow = TRUE, ncol = 2))
plotData = as.data.frame(samples)
plotData2 = plotData[order(abs(plotData[,1]-1.5))[1:10],]


png("fig/pmm.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = V1, y = V2)) + 
  geom_point(size = 3) + xlab("X1") + ylab("X2") + theme_bw() +
  geom_point(data = plotData2, aes(x = V1, y = V2), colour = "red", shape = 1, size = 5) + 
  geom_vline(xintercept = 1.5, colour = "red", size = 2) + 
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

samples = mvrnorm(n = 100, mu = c(1,1), Sigma = matrix(c(1, 0, 0, 1), byrow = TRUE, ncol = 2))
plotData = as.data.frame(samples)
plotData2 = plotData[order(abs(plotData[,1]-1.5))[1:10],]
png("fig/pmm_uncorrelated.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = V1, y = V2)) + 
  geom_point(size = 3) + xlab("X1") + ylab("X2") + theme_bw() +
  geom_point(data = plotData2, aes(x = V1, y = V2), colour = "red", shape = 1, size = 5) + 
  geom_vline(xintercept = 1.5, colour = "red", size = 2) + 
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()