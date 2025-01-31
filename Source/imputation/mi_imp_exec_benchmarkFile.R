
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
source("source/imputation/mi_imputation_helperFunctions.R")
df <- readRDS("data/numeric imputations/impsplit1.rds")
naCorMat <- getMissingnesPatternCorMat(df)
saveRDS(naCorMat, "data/naCorMat.rds")

plotMat = naCorMat
plotMat[is.na(plotMat)] = 0
require(corrplot)
png("fig/missingnessCorrplot.png", height = 800, width = 800)
corrplot(plotMat, method = "color", tl.pos = "n", ylab = "", xlab = "", 
         order = "AOE", cl.cex = 3)
dev.off()
png("fig/missingnessCorrplot_unordered.png", height = 800, width = 800)
corrplot(plotMat, method = "color", tl.pos = "n", ylab = "", xlab = "", 
         order = "original", cl.cex = 3)
dev.off()


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

seq = 1:64
time4 = Sys.time()
nCores = parallel::detectCores()
cl = makeCluster(nCores, type = "SOCK")
snow::clusterCall(cl, function() library(mi))

# export currently loaded environment
ex = ls(.GlobalEnv)
snow::clusterExport(cl, ex)
res = snow::clusterApply(cl = cl, x = seq, fun = function(x) {
  imputeWrapper(df_imputed, x, miMatrix)
})
diff4 = Sys.time() - time4
snow::stopCluster(cl)

# do some benchmarking of sequential version

seq = 1:8
time5 = Sys.time()
for(i in seq){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff5 = Sys.time() - time5

df_imputed = df

seq = 1:16
time6 = Sys.time()
for(i in seq){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff6 = Sys.time() - time6

df_imputed = df

seq = 1:32
time7 = Sys.time()
for(i in seq){ 
  imp <- createimputation(df, df_imputed, i)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff7 = Sys.time() - time7

df_imputed = df

seq = 1:64
time8 = Sys.time()
for(i in seq){ 
  imp <- tryCatch(createimputation(df, df_imputed, i), error = function(e) e)
  if(class(imp)=="data.frame"){
    df_imputed[rownames(imp), i] = imp[,1]
  }
}
diff8 = Sys.time() - time8

df_imputed = df

require(mice)
# create random mice matrix for testing
mat = buildMiceMatrix(spearman[1:8, 1:8], 5, naCorMat[1:8, 1:8], 1)
time9 = Sys.time()
r = mice(df_imputed[,1:8], predictorMatrix = mat)
diff9 = Sys.time() -time9

mat = buildMiceMatrix(spearman[1:16, 1:16], 5, naCorMat[1:16, 1:16], 1)
time10 = Sys.time()
r = mice(df_imputed[,1:16], predictorMatrix = mat)
diff10 = Sys.time() -time10

mat = buildMiceMatrix(spearman[1:32, 1:32], 5, naCorMat[1:32, 1:32], 1)
time11 = Sys.time()
r = mice(df_imputed[,1:32], predictorMatrix = mat)
diff11 = Sys.time() -time11

mat = buildMiceMatrix(spearman[1:64, 1:64], 5, naCorMat[1:64, 1:64], 1)
time12 = Sys.time()
r = mice(df_imputed[,1:64], predictorMatrix = mat)
diff12 = Sys.time() -time12

differences = c(diff1, diff2, diff3, diff4, diff5, diff6, diff7, diff8, diff9, diff10, diff11, diff12)
# diff9 is in seconds
differences[-9] = differences[-9]*60

saveRDS(differences, "data/differencesImpuSpeed.rds")

differencesPlotData = as.data.frame(cbind(SetSize = rep(c("8 Attributes", "16 Attributes", "32 Attributes", "64 Attributes"), times = 3),
                                          Duration = c(differences),
                                          Method = c(rep("MI parallel", 4),  rep("MI sequential", 4), 
                                                     rep("MICE", times = 4))), stringsAsFactors = FALSE)
differencesPlotData[,1] = factor(differencesPlotData[,1], levels = c("8 Attributes", "16 Attributes", "32 Attributes", "64 Attributes"))
differencesPlotData[,2] = as.numeric(differencesPlotData[,2])
differencesPlotData[,3] = factor(differencesPlotData[,3], levels = unique(differencesPlotData[,3]))

png("fig/imputation_speed.png", height = 800, width = 1500)
ggplot(data = differencesPlotData, aes(x = SetSize, y = Duration, group = Method)) + 
  geom_line(aes(colour = Method), size = 3) + geom_abline(slope = 1, intercept = 0, size = 2) +
  xlab("Sample size") + ylab("Duration in seconds") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) + 
  theme(legend.text = element_text(size = 40), legend.title = element_text(size = 40, face = "bold"))
dev.off()
