
#check if rows correspond to sample, so that merge with factors etc. is correct:
s <- readRDS("data/sample.rds") #1 = 1:50000, 2 = 500001:100000, 3 = 100001:145231 in train set
#s[c(1:5,50001:50005,100001:100005)] == rownames(data_numeric)[c(1:5,50001:50005,100001:100005)]

#collist = list("cols_numeric"=colnames(data_numeric),"cols_factors"=colnames(data_factors),"cols_strings" = colnames(data_strings), "cols_dates" = colnames(data_dates))
#saveRDS(collist, "data/collist.rds")

NAstatistics <- readRDS("data/NAstatistics.rds") #since data has no more information about amout of imputation in a column
collist <- readRDS("data/collist.rds") #some data for selecting the right columns for predicting
