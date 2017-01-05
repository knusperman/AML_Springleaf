#############################################################################
#ROC Analysis
#############################################################################
#get pred.XX objects to compare first



## example code
predDF.RF = generateThreshVsPerfData(pred.RF, measures = list(fpr, tpr, mmce))
plotROCCurves(predDF.RF)
mlr::performance(pred.RF, auc)
plotThreshVsPerf(predDF.RF)

##compare learners
comparisondf = generateThreshVsPerfData(list(rf = pred.RF, xgb = pred.XG,svm=pred.SVM), measures = list(fpr, tpr))
plotROCCurves(comparisondf)
qplot(x = fpr, y = tpr, color = learner, data = comparisondf$data, geom = "path")
