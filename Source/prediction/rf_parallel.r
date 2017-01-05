library(randomForest)
library(foreach)
library(doSNOW)

cluster =makeCluster(2,type="SOCK")
registerDoSNOW(makeCluster(2,type="SOCK"))
#run prediction_data_creation...
X = mydata[, -ncol(mydata)]
Y = mydata[, ncol(mydata)]

multiRF <- function(x,...) {
  foreach(i=x,.combine=combine,.packages='randomForest',
          .export=c('X','Y'),.inorder=FALSE) %dopar% {
            randomForest(X,Y,ntree=i,...)
          } 
}
rf = multiRF(rep(50,2))

stopCluster(cluster)