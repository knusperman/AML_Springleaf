eliminateDuplicateCorrelations = function(x) {
  previous = data.frame(cbind(numeric(0), numeric(0)))
  remove = numeric(0)
  for(i in 1:nrow(x)) {
    contained = FALSE
    for (j in 1:nrow(previous)) {
      if (nrow(previous) == 0) break
      if (x[i,1] == previous[j,2] & x[i,2] == previous[j,1]) {
        contained = TRUE
        break
      }
    }
    if (!contained) previous = rbind(previous, x[i,])
    else remove = c(remove, i)
  }
  return(x[-remove,])
}

# eliminates the duplicates from the correlations
# but eliminates keeps the one with more NAs
# this results in the column with more NAs being deleted in the end
eliminateDuplicateCorrelationsMoreNAs = function(indices, data) {
  result = numeric((nrow(indices)/2))
  for(i in 1:nrow(indices)) {
    nas1 = sum(is.na(data[,indices[i,1]]))
    nas2 = sum(is.na(data[,indices[i,2]]))
    if (nas1 > nas2) result[i] = indices[i,1]
    else result[i] = indices[i,2]
    
  }
  return(unique(result))
}

# provides the attributes with the highest absolute correlation per attribute
# to limit the amount of attributes used for value imputation for the most
# expressive ones for the attribute of the missing value
buildMiceMatrix = function(correlation, usedAttributes = 5, naCor, naCorThreshold) {
  diag(correlation) = 0
  result = matrix(ncol = ncol(correlation), nrow = nrow(correlation))
  for (i in 1:ncol(correlation)) {
    ord = order(abs(correlation[,i]), decreasing = TRUE)
    ord = ord[!(ord %in% (which(naCor > naCorThreshold)))]
    ord = ord[1:usedAttributes]
    temp = seq(1, ncol(correlation), by = 1)
    temp = temp %in% ord
    temp[temp == TRUE] = 1
    temp[temp == FALSE] = 0
    result[i,] = temp
  }
  return(result)
}

createCorrelationPlots = function(correlations, suffix) {
  library(ggplot2)
  library(corrplot)
  
  # heatmap corplot
  png(paste("fig/corrplot", suffix, ".png", sep = ""), height = 800, width = 800)
  corrplot(correlations, method = "color", tl.pos = "n", ylab = "", xlab = "", 
           order = "AOE", cl.cex = 3)
  dev.off()
  
  # barplot with 5000 highest and lowest cors
  temp = correlations
  diag(temp) = NA
  correlationVector = as.vector(temp)
  correlationVector = sort(correlationVector)
  plotData = as.data.frame(cbind(id = seq(1, 10000, by = 1), 
                                 values = c(correlationVector[1:5000], 
                                            correlationVector[(length(correlationVector)-4999):length(correlationVector)])))
  plotDataAll = as.data.frame(cbind(id = seq_along(correlationVector), 
                                    values = correlationVector))
  
  png(paste("fig/correlations_barplot", suffix, ".png", sep = ""), height = 800, width = 800)
  ggplot(data = plotData, aes(x = id, y = values)) + 
    geom_bar(stat = "identity") + xlab("Attributes") + ylab("Correlation") + theme_bw() +
    theme(axis.text = element_text(size = 40, colour = "black"), 
          axis.title = element_text(size = 40, colour = "black")) +
    theme(plot.margin = unit(c(1,2,1,1), "cm")) 
  dev.off()
}