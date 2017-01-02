numericalDataSample <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")

dfall <- as.data.frame(numericalDataSample) 
as.data.frame(round(sapply(dfall, function(x) sum(is.na(x)))/nrow(dfall),2))->NAratio_all
df <- as.data.frame(numericalDataSample)[s,] 
as.data.frame(round(sapply(df, function(x) sum(is.na(x)))/nrow(df),2))->NAratio_sample
colnames(NAratio_all)="NAs"
colnames(NAratio_sample)="NAs"
saveRDS(NAratio_all, "data/NAstatistics.rds")
NAratio_all$type="Complete (145k)"
NAratio_sample$type="10k sample"

#### proof that sample is large enough for having same NA distribution
png("fig/sampleNAdistribution.png", height = 800, width = 800)
ggplot(rbind(NAratio_all,NAratio_sample),aes(x=NAs,color=type))+geom_density()+ xlab("% NAs")+ theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()
####
##which numeric attributes have more than 80% NAs
NAratio_sample[which(NAratio_sample$NAs > 0.8),1:2]
