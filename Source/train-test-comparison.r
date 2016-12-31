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
