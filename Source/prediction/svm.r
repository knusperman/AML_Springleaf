classif.lrn.SVM = makeLearner("classif.svm", predict.type = "prob", fix.factors.prediction = TRUE)

pred.SVM = predict(mod.SVM, task = classif.task, subset = test.set)
mod.SVM = train(classif.lrn.SVM, classif.task, subset = train.set)
mlr::performance(pred.SVM, auc)
