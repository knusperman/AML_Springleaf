t1 <- readRDS("data/TEST_1-100.rds")
t2 <- readRDS("data/TEST_101-200.rds")
t3 <- read.csv("/Users/markusheuchert/AML/springleaf_test.csv") 
t3 <- as.data.frame(t3)
colfilter <- readRDS("data/NAstatistics.rds") #for getting the cols that are in the final set
#colfiter rows contain VARs that were in the initial imputation set
#col $NAs hold NA ratio inside column -- neccessary for filtering out VARs with too many NAs for using their imputed parts later

t3 <- t3[,(which(colnames(t3) %in% rownames(colfilter)))] #take only cols that are in the imputed set (i.e. candidates)

t3 <- t3[,-which(colnames(t3) %in% colnames(t1))] #remove the ones that were imputed and lie in t1
dim(t3)
t3 <- t3[,-which(colnames(t3) %in% colnames(t2))] #remove the ones that were imputed and lie in t2
dim(t3)
testset_imputed <- cbind(t1,t2,t3)
dim(t3)

dim(t3) 


