source("source/DateFunctions.R")

dateData = readRDS("data/dateAttributes_cleansed.rds")
monthData = extractDateData(dateData, "m")
dayData = as.data.frame(extractDateData(dateData, "d"))
for (i in 1:ncol(dayData)) {
  dayData[,i] = factor(dayData[,i], levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                               "Saturday", "Sunday"))
}
yearData = extractDateData(dateData, "y")
hourData = extractDateData(as.data.frame(dateData[,15]), "h")
target = as.numeric(readRDS("data/target.rds"))
apply(dateData, 2, function(x) {
  sum(is.na(x))/length(x)
})

buildDatePlots(monthData, target, path = "fig/months/month", month = TRUE)
buildDatePlots(dayData, target, path = "fig/days/day")
buildDatePlots(yearData, target, path = "fig/years/year")
buildDatePlots(hourData, target, path = "fig/hours/hour")

# build relative plots (target response relative to month occurrences)
buildDatePlots(monthData, target, path = "fig/months/rel", month = TRUE, relative = TRUE)
buildDatePlots(dayData, target, path = "fig/days/rel", relative = TRUE)
buildDatePlots(yearData, target, path = "fig/years/rel", relative = TRUE)
buildDatePlots(hourData, target, path = "fig/hours/rel", relative = TRUE)

# relevant dates are (because the others have very high NA values)
# year 2 + 16 (not year of 15 because there is only one year)
# month 2 + 15 + 16
# day 2 + 15 + 16

relevantDateData = cbind(yearData[,2], yearData[,16], monthData[,2], monthData[,15], monthData[,16],
                         dayData[,2], dayData[,15], dayData[,16])
colnames(relevantDataData) = c("VAR_0075_YEAR", "VAR_0217_YEAR", "VAR_0075_MONTH", "VAR_0204_MONTH", 
                               "VAR_0217_MONTH", "VAR_0075_DAY", "VAR_0204_DAY")

# convert all other attributes into TRUE/FALSE (FALSE if NA)
# plot the difference regarding target between NAs and non-NAs as reasoning
otherDates = dateData[,-c(2, 15, 16)]
nonNAtargetPercentage = apply(otherDates, 2, function(x) {
  sum(target[!is.na(x)])/length(target[!is.na(x)])
})
avgTarget = sum(target)/length(target)
plotData = as.data.frame(cbind(id = seq_along(nonNAtargetPercentage), percentage = nonNAtargetPercentage, avg = rep(avgTarget, length(nonNAtargetPercentage))))

png("fig/nonNADate_vsTarget.png", height = 800, width = 800)
ggplot(data = plotData, aes(x = id, y = percentage)) + 
  geom_bar(stat = "identity") + xlab("Attributes") + ylab("Number of occurrences") + theme_bw() +
  geom_line(data = plotData, aes(x = id, y = avg, group = 1), colour = "red", size = 4) + 
  theme(axis.text = element_text(size = 40, colour = "black", angle = 45, hjust = 1), 
        axis.title = element_text(size = 40, colour = "black")) +
  theme(plot.margin = unit(c(1,2,1,1), "cm"))
dev.off()


# apparently the difference between NAs and non-NAs is huge
for (i in 1:ncol(otherDates)) {
  otherDates[is.na(otherDates[,i]),i] = "0"
  otherDates[!otherDates[,i] == "0",i] = "1"
  otherDates[,i] = as.logical(as.numeric(otherDates[,i]))
}
relevantDateData = cbind(relevantDateData, otherDates)
relevantDateData = as.data.frame(relevantDateData)
saveRDS(relevantDateData, "data/dateData_FINAL.rds")

