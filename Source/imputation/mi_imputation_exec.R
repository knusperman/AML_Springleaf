
############################################################################################################
#not on aws
numericalDataSample <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")

set.seed(1234)
s <- sample(1:nrow(numericalDataSample), 10000)
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

saveRDS(df, "data/newnum.rds")
#END not on aws
############################################################################################################
#on AWS, elsewehere
df <- readRDS("data/newnum.rds")
#END on AWS
############################################################################################################

#cast factors to numerics
df <- as.data.frame(df)
for(i in 1:ncol(df)) df[,i]= as.numeric(df[,i])

df$VAR_1444<-NULL #quasi identical to 1445 and no useful information
df$VAR_1445<-NULL
df$VAR_0445<-NULL #kick at imputation merge
df$VAR_0449<-NULL



spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(df)),which(colnames(spearman) %in% colnames(df))] # to be sure to select only top correlations that are in current set
miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
df_imputed <- df

###### adjust i up to ncol(df) on all computing devices. 
for(i in 800){ 
  imp <- createimputation(i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
  
}

NAs <- apply(df, 2, function(x) sum(is.na(x)))
NAratio <- sum(NAs)/sum(ncol(df)*nrow(df))

afterImputationNAs <- apply(df_imputed[,1000:ncol(df_imputed)], 2, function(x) sum(is.na(x)))
afterImputationNAs <- afterImputationNAs[afterImputationNAs>0]
