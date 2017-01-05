if (!"mlr" %in% installed.packages()) install.packages("mlr")
if (!"ggvis" %in% installed.packages()) install.packages("ggvis")
require(mlr)
require(ggvis)

data = readRDS("data/final/cleansedDataset_subset.rds") # change to final when done

set.seed(1234)
samp = sample(1:nrow(data), 30000)
task_baseline = makeClassifTask(id = "baseline", data = data[samp[1:20000],], target = "target", positive="1")
