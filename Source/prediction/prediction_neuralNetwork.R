if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"ggvis" %in% installed.packages()) install.packages("ggvis")
require(mlr)
require(ggvis)

data = readRDS("data/final/cleansedDataset.rds")
# data = readRDS("data/final/baselineDataset.rds") # no datacleaning
set.seed(1234)
samp = sample(1:nrow(baseline), 30000)