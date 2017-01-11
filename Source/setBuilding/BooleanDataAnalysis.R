booleanData = as.data.frame(readRDS("data/booleanAttributes_cleansed.rds"))
# some booleans still have NAs
# convert these to factors with an additional level for the NAs
nas = which(apply(booleanData, 2, function(x) {sum(is.na(x)) > 0}))
for (i in nas) {
  booleanData[,i] = as.character(booleanData[,i])
  booleanData[is.na(booleanData[,i]),i] = "2"
  booleanData[(booleanData[,i] == TRUE),i] = "1"
  booleanData[(booleanData[,i] == FALSE),i] = "0"
}

saveRDS(booleanData, "data/final/booleanAttributes_FINAL.rds")
