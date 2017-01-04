# different underlying data
baseline = readRDS("data/final/baselineDataset.rds") # no datacleaning
undifferentiated = readRDS("data/final/undifferentiatedDataset.rds") # imputation done with factors and numericals combined
cleansed = readRDS("data/final/cleansedDataset.rds") # our datacleaning approach

# work with 20k samples for speedup
# first compare all three datasets on a basic random forest with 100 trees (for quick speed)


classif.task_baseline = makeClassifTask(id = "baseline", data = baseline, target = "target", positive="1")
classif.task_undifferentiated = makeClassifTask(id = "undifferentiated", data = undifferentiated, target = "target", positive="1")
classif.task_cleansed = makeClassifTask(id = "cleansed", data = cleansed, target = "target", positive="1")