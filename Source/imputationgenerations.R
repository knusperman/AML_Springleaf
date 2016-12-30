set.seed(1234)
s <- sample(1:dim(numericalDataSample)[1], 10000)
s2 <- 1:dim(numericalDataSample)[1]
s2 <- sample(s2, 10000)
df <- as.data.frame(numericalDataSample)[s2,] #add other attribute sets(factors... here)