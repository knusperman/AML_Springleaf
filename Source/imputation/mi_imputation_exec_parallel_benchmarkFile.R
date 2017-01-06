
############################################################################################################
#not on aws
numericalDataSample <- data.frame(readRDS("data/numericalAttributes_cleansed_withoutFactors.rds"),stringsAsFactors =FALSE)
killedNAs <- readRDS("data/killednumericsNA.rds")
set.seed(1234)
s <- readRDS("data/sample.rds")
#set.seed(1234)
#s <- sample(1:nrow(numericalDataSample), nrow(numericalDataSample))
df <- numericalDataSample
remove(numericalDataSample)
df$VAR_1444<-NULL #quasi identical to 1445 and no useful information
df$VAR_1445<-NULL
df$VAR_0445<-NULL #kick at imputation merge
df$VAR_0449<-NULL
df <- df[, -which(colnames(df) %in% killedNAs)]
for(i in 1:ncol(df)) df[,i] = as.numeric(df[,i])
df1 <- df[s[1:50000],]
df2 <- df[s[50001:100000],]
df3 <- df[s[100001:length(s)],]

saveRDS(df1,"data/numeric imputations/impsplit1.rds")
saveRDS(df2,"data/numeric imputations/impsplit2.rds")
saveRDS(df3,"data/numeric imputations/impsplit3.rds")
#############################################################################
#############################################################################
source("Source/imputation/mi_imputation.R")
df <- readRDS("data/numeric imputations/impsplit1.rds")
naCorMat <- getMissingnesPatternCorMat(df)

spearman <- readRDS("data/spearman_without1.rds")
spearman <- spearman[which(colnames(spearman) %in% colnames(df)),which(colnames(spearman) %in% colnames(df))] # to be sure to select only top correlations that are in current set
miNACorMat <- buildMiceMatrix(spearman,5,naCorMat,0.7)
row.names(miNACorMat) = colnames(df)
colnames(miNACorMat) = colnames(df)

miMatrix <- miNACorMat
#miMatrix <- miCorMatrix(spearman, 5) # top 5 correlations
df_imputed <- df
seq = 1:ncol(df)
remove(miNACorMat)
remove(spearman)
remove(naCorMat)

source("source/imputation/mi_imputation_helperFunctions.R")
if (!"snow" %in% installed.packages()) install.packages("snow")
library(snow)

# do some benchmarking of parallel version

seq = 1:8
time1 = Sys.time()
nCores = parallel::detectCores()
cl = makeCluster(nCores, type = "SOCK")
snow::clusterCall(cl, function() library(mi))

# export currently loaded environment
ex = ls(.GlobalEnv)
snow::clusterExport(cl, ex)
res = snow::clusterApply(cl = cl, x = seq, fun = function(x) {
  imputeWrapper(df_imputed, x, miMatrix)
})
diff1 = Sys.time() - time1
snow::stopCluster(cl)

seq = 1:16
time2 = Sys.time()
nCores = parallel::detectCores()
cl = makeCluster(nCores, type = "SOCK")
snow::clusterCall(cl, function() library(mi))

# export currently loaded environment
ex = ls(.GlobalEnv)
snow::clusterExport(cl, ex)
res = snow::clusterApply(cl = cl, x = seq, fun = function(x) {
  imputeWrapper(df_imputed, x, miMatrix)
})
diff2 = Sys.time() - time2
snow::stopCluster(cl)

seq = 1:32
time3 = Sys.time()
nCores = parallel::detectCores()
cl = makeCluster(nCores, type = "SOCK")
snow::clusterCall(cl, function() library(mi))

# export currently loaded environment
ex = ls(.GlobalEnv)
snow::clusterExport(cl, ex)
res = snow::clusterApply(cl = cl, x = seq, fun = function(x) {
  imputeWrapper(df_imputed, x, miMatrix)
})
diff3 = Sys.time() - time3
snow::stopCluster(cl)

# do some benchmarking of sequential version

seq = 1:8
time4 = Sys.time()
for(i in seq){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff4 = Sys.time() - time4

df_imputed = df

seq = 1:16
time5 = Sys.time()
for(i in seq){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff5 = Sys.time() - time5

df_imputed = df

seq = 1:32
time6 = Sys.time()
for(i in seq){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff6 = Sys.time() - time6

df_imputed = df

differences = c(diff1, diff2, diff3, diff4, diff5, diff6)

require(mice)
# create random mice matrix for testing
mat = buildMiceMatrix(spearman[1:8, 1:8], 5, naCorMat[1:8, 1:8], 1)
time7 = Sys.time()
r = mice(df_imputed[,1:8], predictorMatrix = mat)
diff7 = Sys.time() -time7


mat = buildMiceMatrix(spearman[1:16, 1:16], 5, naCorMat[1:16, 1:16], 1)
time8 = Sys.time()
r = mice(df_imputed[,1:16], predictorMatrix = mat)
diff8 = Sys.time() -time8


mat = buildMiceMatrix(spearman[1:32, 1:32], 5, naCorMat[1:32, 1:32], 1)
time9 = Sys.time()
r = mice(df_imputed[,1:32], predictorMatrix = mat)
diff9 = Sys.time() -time9
differences = c(differences, diff7, diff8, diff9)
differences[-7] = differences[-7]*60

differencesPlotData = as.data.frame(cbind(SetSize = rep(c("8 Attributes", "16 Attributes", "32 Attributes"), times = 3),
                                          Duration = c(differences),
                                          Method = c(rep("MI parallel", 3),  rep("MI sequential", 3), 
                                                     rep("MICE", times = 3))), stringsAsFactors = FALSE)
differencesPlotData[,1] = factor(differencesPlotData[,1], levels = c("8 Attributes", "16 Attributes", "32 Attributes"))
differencesPlotData[,2] = as.numeric(differencesPlotData[,2])
differencesPlotData[,3] = factor(differencesPlotData[,3], levels = unique(differencesPlotData[,3]))

png("fig/imputation_speed.png", height = 800, width = 800)
ggplot(data = differencesPlotData, aes(x = SetSize, y = Duration, group = Method)) + 
  geom_line(aes(colour = Method), size = 2) + geom_abline(slope = 1, intercept = 0, size = 2) +
  xlab("Sample size") + ylab("Duration in seconds") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
  theme(legend.text = element_text(size = 40), legend.title = element_text(size = 40, face = "bold"))
dev.off()
