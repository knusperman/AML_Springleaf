classif.lrn.kknn = makeLearner("classif.kknn", predict.type = "prob", fix.factors.prediction = TRUE)
classif.lrn.kknn$par.set
mod.kknn = train(classif.lrn.kknn, classif.task, subset = train.set)
pred.kknn  = predict(mod.kknn, task = classif.task, subset = test.set)
mlr::performance(pred.kknn,auc) #0.6187
