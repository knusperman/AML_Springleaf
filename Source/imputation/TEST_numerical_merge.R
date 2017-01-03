t1 <- readRDS("data/TESTimp_1von2rds.rds")
t2 <- readRDS("data/TESTimp_2von2rds.rds")
t3 <- read.csv("/Users/markusheuchert/AML/springleaf_test.csv") 
t3 <- as.data.frame(t3)
colfilter <- readRDS("data/NAstatistics.rds") #for getting the cols that are in the final set
#colfiter rows contain VARs that were in the initial imputation set, before killing VARs due to too high NA ratio
killedNAs <- readRDS("data/killednumericsNA.rds") #which have more than 54% NAs - see train-test-comparison.r 

t3 <- t3[,(which(colnames(t3) %in% rownames(colfilter)))] #take only cols that are in the imputed set (i.e. candidates)
t3 <- t3[,-which(colnames(t3) %in% killedNAs)] # remove ones that were not imputed due to too high NA ratio
t3 <- t3[,-which(colnames(t3) %in% colnames(t1))] #remove the ones that were imputed and lie in t1
t3 <- t3[,-which(colnames(t3) %in% colnames(t2))] #remove the ones that were imputed and lie in t2

testset_imputed <- cbind(t1,t2,t3) 

#to do:
# check which ones of the factor and date columns are part of the final data set
# clean cols as done in the test set (999999 = NA and so on)

