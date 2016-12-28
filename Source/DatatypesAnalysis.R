#############################################
# NA HANDLING
#############################################
# FILE CONTAINS PLOT GENERATION AND REASONING FOR THE IDENTIFICATION OF DIFFERENT DATATYPES
# REQUIRES CERTAIN OBJECTS BEING INITIALIZED FROM DATACLEANING.R

# find out what a likely threshold for factors <-> numerical data might be
uniqueNumericalValues = apply(numericalData, 2, function(x) {
  length(unique(x))
})
uniqueNumericalValues = sort(uniqueNumericalValues)
uniqueNumericalValues = as.data.frame(cbind(id = seq_along(uniqueNumericalValues), uniqueNumericalValues))
png("fig/uniqueValues.png", height = 800, width = 800)
ggplot(data = uniqueNumericalValues, aes(x = id, y = uniqueNumericalValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# not much to see there, drill down to only attributes with 100 unique values or less
uniqueNumericalValues100 = uniqueNumericalValues[uniqueNumericalValues[,2] < 101,]
png("fig/uniqueValues_100.png", height = 800, width = 800)
ggplot(data = uniqueNumericalValues100, aes(x = id, y = uniqueNumericalValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# not much to see here either, look at variances of attribute values
# reasoning: factor attribute values are likely to be close together
# only consider less than 101 unique values, it is unlikely that there are more than 100 factors per attribute
uniqueValuesVariances = apply(numericalData[,which(uniqueNumericalValues[,2] < 101)], 2, function(x) {
  var(na.omit(unique(x)))
})
uniqueValuesVariances = sort(uniqueValuesVariances)
uniqueValuesVariances = as.data.frame(cbind(id = seq_along(uniqueValuesVariances), uniqueValuesVariances))
png("fig/uniqueValuesVariances_500.png", height = 800, width = 800)
ggplot(data = uniqueValuesVariances[1:500,], aes(x = id, y = uniqueValuesVariances)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# elbow somewhere between att 200 and 400 (sorted)
png("fig/uniqueValuesVariances_200_400.png", height = 800, width = 800)
ggplot(data = uniqueValuesVariances[200:400,], aes(x = id, y = uniqueValuesVariances)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()

# find biggest gap
gaps = abs(uniqueValuesVariances[200:399,2] - uniqueValuesVariances[201:400,2])
which(gaps == max(gaps)) # 150
# the biggest gap is between attribute 349 and 350

# analyse the number of unique values for these
factorPotentials = rownames(uniqueValuesVariances[1:349,])
factorPotentials_uniqueNumericalValues = uniqueNumericalValues[rownames(uniqueNumericalValues) %in% factorPotentials,]
factorPotentials_uniqueNumericalValues$id = seq_along(factorPotentials_uniqueNumericalValues$uniqueNumericalValues)
png("fig/uniqueValuesVariances_factorPotentials.png", height = 800, width = 800)
ggplot(data = factorPotentials_uniqueNumericalValues, aes(x = id, y = uniqueNumericalValues)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of unique values") + theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) 
dev.off()
# find biggest gap
gaps = abs(factorPotentials_uniqueNumericalValues[1:348,2] - factorPotentials_uniqueNumericalValues[2:349,2])
which(gaps == max(gaps)) # 339
factorPotentials_uniqueNumericalValues[339,2]
# the biggest gap is between attribute 339 and 340
# we therefore conclude that attributes with less than or equal to 53 unique values are categorical if there variance is also low
# plot these different attribute types
waterfallData = data.frame(desc = c("Total attributes", 
                                    "ID & target", "Only one value", "Boolean", "Dates",
                                    "Strings", "Numerical", "Only 1 value and NA", "Probably categorical",
                                    "Actual numerical"), 
                           amount = c(1934, -2, -8, -24, -16, -21, -1863, -53, -333, -1488))
waterfallData$id = seq_along(waterfallData$amount)
waterfallData$cumsum = cumsum(waterfallData$amount)
# do some ugly manual coding due to reset after "Numerical"
waterfallData$cumsum[8:9] = waterfallData$cumsum[8:9] + 1874
waterfallData$start = c(0, head(waterfallData$cumsum, -1))
waterfallData$start[8:9] = c(1863, 1863-53)
waterfallData$cumsum[10] = 0
waterfallData$desc = factor(waterfallData$desc, levels = waterfallData$desc)

png("fig/typesOfAttributes_waterfall.png", height = 800, width = 800)
ggplot(waterfallData, aes(desc)) + 
  scale_y_continuous(limits = c(0, 2000)) +
  geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, ymin = cumsum, ymax = start), fill = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 40, colour = "black"), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.45)) +
  geom_text(aes(y = rep(2000, 10), label = abs(amount)), size = 10, colour = "black") +
  xlab("") + ylab("Amount")
dev.off()