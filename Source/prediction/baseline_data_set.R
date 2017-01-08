readRDS("/Users/markusheuchert/Downloads/baselineDataset.rds")->baseline
s <- readRDS("data/sample.rds") #1 = 1:50000, 2 = 500001:100000, 3 = 100001:145231 in train set

baseline <- baseline[s[100001:length(s)],] # same sample as used for imputed data sample
saveRDS(baseline, "data/baselinesampleFINAL.rds")

