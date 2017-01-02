#only for numeric attributes. factor NAs are treated as a level-->no NAs. 
library(ggplot2)
test <- read.csv("/Users/markusheuchert/AML/springleaf_test.csv") 
train <- readRDS("data/numericalAttributes_cleansed_withoutFactors.rds")

test <- as.data.frame(test)
train <- as.data.frame(train)

test <- test[, which(colnames(test) %in% colnames(train))] #just select relevant columns

NAratio_test <- as.data.frame(round(sapply(test, function(x) sum(is.na(x)))/nrow(test),2))
NAratio_train <- as.data.frame(round(sapply(train, function(x) sum(is.na(x)))/nrow(train),2))

colnames(NAratio_test) = ("NAs")
colnames(NAratio_train) = ("NAs")

NAratio_test$type="test"
NAratio_train$type="train"
NAratio_merged = rbind(NAratio_test,NAratio_train)
png("fig/train-test-na-distribution.png", height = 800, width = 800)
ggplot(NAratio_merged, aes(x=NAs,color=type))+geom_density()+ xlab("% NAs")+ theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black"),
        legend.text = element_text(size=40, colour="black"),
        legend.title = element_text(size=40, colour="black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

#Are there VARs that have more %NAs in test than train?
which(NAratio_test$NAs>NAratio_train$NAs) # No


######
NAstatistics <- readRDS("data/NAstatistics.rds")
NAstatistics <- as.data.frame(NAstatistics)
png("fig/ECDF-numerical-attributes-train.png", height = 800, width = 800)
ggplot(NAstatistics, aes(x=NAs))+stat_ecdf()+ xlab("% NAs in attribute")+ ylab("p")+geom_vline(xintercept = 0.54,colour="red", alpha=0.4)+theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black"),
        legend.text = element_text(size=40, colour="black"),
        legend.title = element_text(size=40, colour="black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

quantile(NAstatistics$NAs,seq(0.7,0.8,0.01))#76% sits at elbow.
#-->exlude VARs with more than 54% NAs (76%-quantile value).

killed_numerics_bc_high_NA <- rownames(NAstatistics)[which(NAstatistics$NAs >0.54)] #330 VARs 
saveRDS(killed_numerics_bc_high_NA, "data/killednumericsNA.rds")

n2 <- num[,-which(colnames(num) %in% rownames(NAstatistics)[which(NAstatistics$NAs >0.54)])]
sum(sapply(num,function(x) sum(is.na(x))))/(ncol(num)*nrow(num)) #24.7% NA in numerical set before
sum(sapply(n2,function(x) sum(is.na(x))))/(ncol(n2)*nrow(n2)) #9.3% NA afterwards

getMissingnesPatternCorMat <- function(data ){
  cor(is.na(data),use="everything",method="pearson")
}
spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[ -which(colnames(spearman)%in% killed_numerics_bc_high_NA),-which(colnames(spearman)%in% killed_numerics_bc_high_NA)]
indices <- order(as.vector(spearman))

plot(x=as.vector(spearman)[indices],y=as.vector(temp)[indices])
