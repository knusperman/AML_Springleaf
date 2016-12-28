naRows = apply(trainData, 1, is.na)
naRows = apply(naRows, 2, sum)
naRowsSorted = sort(naRows)
barplot(naRowsSorted / ncol(trainData))
orderIndices = order(naRows)

plot(x = naRows[orderIndices], y = target[orderIndices])
# nothing to see
lm(target ~ naRows) # not very high coefficient
sum(target)/length(target) # 0.2325468 target values of 1
# compare to the highest quantile and the lowest quantile of the NA distribution
sum(target[orderIndices[1:2000]])/2000 # 0.185
sum(target[orderIndices[8001:10000]])/2000 # 0.172
sum(target[orderIndices[6001:8000]])/2000 # 0.1895
sum(target[orderIndices[4001:6000]])/2000 # 0.1705
sum(target[orderIndices[2001:4000]])/2000 # 0.177
sum(target[orderIndices[1:10000]])/10000 # 0.1788
