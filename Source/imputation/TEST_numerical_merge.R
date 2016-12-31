p1 <- readRDS("data/TEST_1-100.rds")
p2 <- readRDS("data/TEST_101-200.rds")
p3 <- read.csv("/Users/markusheuchert/AML/springleaf_test.csv") 
p3 <- as.data.frame(p3)
colfilter <- readRDS("data/NAstatistics.rds") #for getting the cols that are in the final set
#colfiter rows contain VARs that were in the initial imputation set
#col $NAs hold NA ratio inside column -- neccessary for filtering out VARs with too many NAs for using their imputed parts later

p3 <- p3[,(which(colnames(p3) %in% rownames(colfilter)))] #take only cols that are in the imputed set (i.e. candidates)
NAratio_all_test <- as.data.frame(round(sapply(p3, function(x) sum(is.na(x)))/nrow(p3),2))
colnames(NAratio_all_test) = ("NAs")

#### proof that sample is large enough for having same NA distribution
png("fig/TESTNAdistribution.png", height = 800, width = 800)
ggplot(NAratio_all_test, aes(x=NAs))+geom_density()+ xlab("% NAs")+ theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()
####
####analyze NAs in test set. Test set might show different NA distribution
which(NAratio_all_test>0.5) #more than 80% NA in test set that is included in train imputation set

rownames(NAratio_all_test)[c(37,68,69)]
# VAR_0074, VAR_0205, VAR_0209
NAratio_all_test[c(37,68,69),] 
NAratio_all[c(37,68,69),] 

dim(p3)
p3 <- p3[,-which(colnames(p3) %in% colnames(p1))] #remove the ones that were imputed and lie in p1
dim(p3)
p3 <- p3[,-which(colnames(p3) %in% colnames(p2))] #remove the ones that were imputed and lie in p2
dim(p3)
testset_imputed <- cbind(p1,p2,p3)
dim(p3)

dim(p3) 


