library(boot)
##################note characteristics versus entity references
noteCharaUser = read.csv("data/noteAssessmentPerParticipant.csv")
entityRefUser = read.csv("data/entityReferencesPerParticipant.csv")
entityRefNoteChara = merge(noteCharaUser, entityRefUser, by = "user")
tauFunc<-function(data,i){cor(x = data[,1][i], y = data[,2][i], method="kendall")}

#correlations between note characteristics
cor.test(noteCharaUser$detailMean, noteCharaUser$priorMean, method="kendall")
bootTau = boot(noteCharaUser[,c('detailMean', 'priorMean')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(noteCharaUser$detailMean, noteCharaUser$grouping, method="kendall")
bootTau = boot(noteCharaUser[,c('detailMean', 'grouping')], tauFunc, 2000)
boot.ci(bootTau)

#category and the number of unique countries referred
cor.test(entityRefNoteChara$statement, entityRefNoteChara$countryEntity, method="kendall")
bootTau = boot(entityRefNoteChara[,c('statement', 'countryEntity')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$statement, entityRefNoteChara$countryMean, method="kendall")
bootTau = boot(entityRefNoteChara[,c('statement', 'countryMean')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(entityRefNoteChara$comparison, entityRefNoteChara$countryEntity, method="kendall")
bootTau = boot(entityRefNoteChara[,c('comparison', 'countryEntity')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$comparison, entityRefNoteChara$countryMean, method="kendall")
bootTau = boot(entityRefNoteChara[,c('comparison', 'countryMean')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(entityRefNoteChara$grouping, entityRefNoteChara$countryEntity, method="kendall")
bootTau = boot(entityRefNoteChara[,c('grouping', 'countryEntity')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$grouping, entityRefNoteChara$countryMean, method="kendall")
bootTau = boot(entityRefNoteChara[,c('grouping', 'countryMean')], tauFunc, 2000)
boot.ci(bootTau)

#category and chart/visual component references
cor.test(entityRefNoteChara$grouping, entityRefNoteChara$map + entityRefNoteChara$lineChart, method="kendall")
bootTau = boot(data.frame(grouping = entityRefNoteChara$grouping, chartCite = entityRefNoteChara$map + entityRefNoteChara$lineChart), tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$grouping, entityRefNoteChara$mapPoint + entityRefNoteChara$line + entityRefNoteChara$vertical, method="kendall")
bootTau = boot(data.frame(grouping = entityRefNoteChara$grouping, componentCite = entityRefNoteChara$mapPoint + entityRefNoteChara$line + entityRefNoteChara$vertical), tauFunc, 2000)
boot.ci(bootTau)

cor.test(entityRefNoteChara$comparison, entityRefNoteChara$map + entityRefNoteChara$lineChart, method="kendall")
bootTau = boot(data.frame(comparison = entityRefNoteChara$comparison, chartCite = entityRefNoteChara$map + entityRefNoteChara$lineChart), tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$comparison, entityRefNoteChara$mapPoint + entityRefNoteChara$line + entityRefNoteChara$vertical, method="kendall")
bootTau = boot(data.frame(comparison = entityRefNoteChara$comparison, componentCite = entityRefNoteChara$mapPoint + entityRefNoteChara$line + entityRefNoteChara$vertical), tauFunc, 2000)
boot.ci(bootTau)

cor.test(entityRefNoteChara$comparison, entityRefNoteChara$vertical, method="kendall")
bootTau = boot(data.frame(comparison = entityRefNoteChara$comparison, vertical = entityRefNoteChara$vertical), tauFunc, 2000)
boot.ci(bootTau)

#overview versus detail
cor.test(entityRefNoteChara$detailMean, entityRefNoteChara$vertical, method="kendall")
bootTau = boot(entityRefNoteChara[, c('detailMean', 'vertical')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$detailMean, entityRefNoteChara$yearEntity, method="kendall")
bootTau = boot(entityRefNoteChara[, c('detailMean', 'yearEntity')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$detailMean, entityRefNoteChara$note, method="kendall")
bootTau = boot(entityRefNoteChara[, c('detailMean', 'note')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$detailMean, entityRefNoteChara$lineChart, method="kendall")
bootTau = boot(entityRefNoteChara[, c('detailMean', 'lineChart')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(entityRefNoteChara$detailMean, entityRefNoteChara$yearMean, method="kendall")
bootTau = boot(entityRefNoteChara[, c('detailMean', 'yearMean')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$yearMean, entityRefNoteChara$vertical, method="kendall")
bootTau = boot(entityRefNoteChara[, c('yearMean', 'vertical')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(entityRefNoteChara$yearMean, entityRefNoteChara$yearEntity, method="kendall")
bootTau = boot(entityRefNoteChara[, c('yearMean', 'yearEntity')], tauFunc, 2000)
boot.ci(bootTau)

#prior knowledge
cor.test(entityRefNoteChara$priorMean, entityRefNoteChara$note, method="kendall")
bootTau = boot(entityRefNoteChara[, c('priorMean', 'note')], tauFunc, 2000)
boot.ci(bootTau)
###########################

##################using entity references to predict note characteristics
library(randomForest)
library(healthcareai)
library(caret)
library(ROCR)
entityRef = read.csv("data/entityReferences.csv")
noteChara = read.csv("data/noteAssessment.csv")
noteChara$user = NULL
entityNoteEva = merge(entityRef, noteChara, by = "note")

#random forest
entityNoteEva$category = as.factor(entityNoteEva$category)
split = split_train_test(entityNoteEva, category, percent_train = 0.8, seed = 2020, user)
entityNoteEva.trainForest = split$train[, c(3:10, 11)]
entityNoteEva.testForest = split$test[, c(3:10, 11)]
table(entityNoteEva.trainForest$category)

optimalRF = randomForest(category ~., data = entityNoteEva.trainForest, strata = entityNoteEva.trainForest[,9], 
                              sampsize = rep(30,3),
                              ntree = 500,
                              mtry = 3,
                              nodesize = 9,
                              importance = T)

pred = predict(optimalRF, entityNoteEva.testForest)
confusionMatrix(pred, entityNoteEva.testForest[,9])

prediction_for_roc_curve <- predict(optimalRF, entityNoteEva.testForest, type='prob')
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c("s","c","g")
for (i in 1:3){
  # Define which observations belong to classes[i]
  true_values <- ifelse(entityNoteEva.testForest[, 9] == classes[i],1,0)
  # Assess the performance of classifier for classes[i]
  pred <- prediction(prediction_for_roc_curve[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

entityNoteEva$detail = as.factor(entityNoteEva$detail)
split = split_train_test(entityNoteEva[!(entityNoteEva$detail == -1),], detail, percent_train = 0.8, seed = 2020, user)
split$train$detail = factor(split$train$detail)
split$test$detail = factor(split$test$detail)
entityNoteEva.trainForest = split$train[, c(3:10, 12)]
entityNoteEva.testForest = split$test[, c(3:10, 12)]
table(entityNoteEva.trainForest$detail)

optimalRF = randomForest(detail ~., data = entityNoteEva.trainForest, strata = entityNoteEva.trainForest[,9], 
                         sampsize = rep(10,3),
                         ntree = 1000,
                         mtry = 1,
                         nodesize = 12,
                         importance = T)

pred = predict(optimalRF, entityNoteEva.testForest)
confusionMatrix(pred, entityNoteEva.testForest[,9])

prediction_for_roc_curve <- predict(optimalRF, entityNoteEva.testForest, type='prob')
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c("0","0.5","1")
for (i in 1:3){
  # Define which observations belong to classes[i]
  true_values <- ifelse(entityNoteEva.testForest[, 9] == classes[i],1,0)
  # Assess the performance of classifier for classes[i]
  pred <- prediction(prediction_for_roc_curve[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

entityNoteEva$prior = as.factor(entityNoteEva$prior)
split = split_train_test(entityNoteEva, prior, percent_train = 0.8, seed = 2020, user)
entityNoteEva.trainForest = split$train[, c(3:10, 13)]
entityNoteEva.testForest = split$test[, c(3:10, 13)]
table(entityNoteEva.trainForest$prior)

optimalRF = randomForest(prior ~., data = entityNoteEva.trainForest, strata = entityNoteEva.trainForest[,9], 
                         sampsize = rep(150,2),
                         ntree = 2000,
                         mtry = 3,
                         nodesize = 2,
                         importance = T)

pred = predict(optimalRF, entityNoteEva.testForest)
confusionMatrix(pred, entityNoteEva.testForest[,9])

prediction_for_roc_curve <- predict(optimalRF, entityNoteEva.testForest, type='prob')
pred <- prediction(prediction_for_roc_curve[, c('1')], entityNoteEva.testForest[, 9])
perf <- performance(pred, "tpr", "fpr")
plot(perf, main="ROC Curve", col= 'red') 
auc.perf <- performance(pred, measure = "auc")
print(auc.perf@y.values)

#feature importance, 10 rounds of drop-column and get the average
get_drop_RF <- function(formular, sampsize, ntree, mtry, nodesize){
  columns = names(entityNoteEva.trainForest[,1:8])
  featureImportance <- data.frame(Feature = columns)
  for(i in 1:10){
    model <- randomForest(formular, data = entityNoteEva.trainForest, strata = entityNoteEva.trainForest[,9], 
                          sampsize = sampsize,
                          ntree = ntree,
                          mtry = mtry,
                          nodesize = nodesize,
                          importance=T)
    pred_rf  = predict(model, entityNoteEva.testForest)
    com = confusionMatrix(pred_rf, entityNoteEva.testForest[,9])
    fullAcc = com$overall[1]
    print(fullAcc)

    imp <- c()
    for (c in columns){
      train_sub <- entityNoteEva.trainForest[, !(colnames(entityNoteEva.trainForest) == c)]
      test_sub <- entityNoteEva.testForest[, !(colnames(entityNoteEva.testForest) == c)]
      model <- randomForest(formular, data = train_sub, strata = train_sub[, 8], 
                            sampsize = sampsize,
                            ntree = ntree,
                            mtry = mtry,
                            nodesize = nodesize,
                            importance=T)
      pred_rf  = predict(model, test_sub)
      com = confusionMatrix(pred_rf, test_sub[, 8])
      subAcc = com$overall[1]
      difAcc = fullAcc - subAcc
      imp <- c(imp, difAcc)
    }
    featureImportance = cbind(featureImportance, imp)
  }
  featureImportance$mean = rowMeans(featureImportance[,2:11])
  print(featureImportance[order(-featureImportance$mean),])
}

get_drop_RF(category ~., rep(30,3), 500, 3, 9)
get_drop_RF(detail ~., rep(10,3), 1000, 1, 12)
get_drop_RF(prior ~., rep(150,2), 2000, 3, 2)

#SVM
library(e1071)
entityNoteEva$category = as.factor(entityNoteEva$category)
split = split_train_test(entityNoteEva, category, percent_train = 0.8, seed = 2020, user)
entityNoteEva.train = split$train[, c(3:10, 11)]
entityNoteEva.test = split$test[, c(3:10, 11)]
table(entityNoteEva.train$category)

bestSVM <- svm(category ~., data=entityNoteEva.train, method = "C-classification", kernel = "radial", cost = .25, gamma = 4, class.weights = c("c" = 1, "g" = 3.862, "s" = 1.737), cross = 10)
pred = predict(bestSVM, entityNoteEva.test)
confusionMatrix(pred, entityNoteEva.test[,9])

bestSVM <- svm(category ~., data=entityNoteEva.train, method = "C-classification", kernel = "radial", cost = .25, gamma = 4, class.weights = c("c" = 1, "g" = 3.862, "s" = 1.737), cross = 10, probability = T)

prediction_for_roc_curve <- predict(bestSVM, entityNoteEva.test, probability = T)
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c('s', "c", "g")

for (i in 1:3){
  true_values <- ifelse(entityNoteEva.test[, 9] == classes[i],1,0)
  pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i == 1)
  {
    plot(perf, main ="ROC Curve", col = pretty_colours[i]) 
  }
  else
  {
    plot(perf, main = "ROC Curve", col = pretty_colours[i], add = TRUE) 
  }
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

entityNoteEva$detail = as.factor(entityNoteEva$detail)
split = split_train_test(entityNoteEva[!(entityNoteEva$detail == -1),], detail, percent_train = 0.8, seed = 2020, user)
split$train$detail = factor(split$train$detail)
split$test$detail = factor(split$test$detail)
entityNoteEva.train = split$train[, c(3:10, 12)]
entityNoteEva.test = split$test[, c(3:10, 12)]
table(entityNoteEva.train$detail)

bestSVM <- svm(detail ~., data = entityNoteEva.train, method = "C-classification", kernel = "radial", cost = 1024, gamma = .00390625, class.weights = c("0" = 1, "0.5" = 1.412, "1" = 1.441), cross = 10)
pred = predict(bestSVM, entityNoteEva.test)
confusionMatrix(pred, entityNoteEva.test[, 9])

bestSVM <- svm(detail ~., data = entityNoteEva.train, method = "C-classification", kernel = "radial", cost = 1024, gamma = .00390625, class.weights = c("0" = 1, "0.5" = 1.412, "1" = 1.441), cross = 10, probability = T)

prediction_for_roc_curve <- predict(bestSVM, entityNoteEva.test, probability = T)
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c('0', "0.5", "1")

for (i in 1:3){
  true_values <- ifelse(entityNoteEva.test[, 9] == classes[i],1,0)
  pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i == 1)
  {
    plot(perf, main ="ROC Curve", col = pretty_colours[i]) 
  }
  else
  {
    plot(perf, main = "ROC Curve", col = pretty_colours[i], add = TRUE) 
  }
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

entityNoteEva$prior = as.factor(entityNoteEva$prior)
split = split_train_test(entityNoteEva, prior, percent_train = 0.8, seed = 2020, user)
entityNoteEva.train = split$train[, c(3:10, 13)]
entityNoteEva.test = split$test[, c(3:10, 13)]
table(entityNoteEva.train$prior)

bestSVM <- svm(prior ~., data=entityNoteEva.train, method = "C-classification", kernel = "radial", cost = 16, gamma = .03125, class.weights = c("0" = 1, "1" = 2.362), cross = 10)
pred = predict(bestSVM, entityNoteEva.test)
confusionMatrix(pred, entityNoteEva.test[, 9])

bestSVM <- svm(prior ~., data = entityNoteEva.train, method = "C-classification", kernel = "radial", cost = 16, gamma = .03125, class.weights = c("0" = 1, "1" = 2.362), cross = 10, probability = T)
prediction_for_roc_curve <- predict(bestSVM, entityNoteEva.test, probability = T)
pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, c('1')], entityNoteEva.test[, 9])
perf <- performance(pred, "tpr", "fpr")
plot(perf,main="ROC Curve",col = "red") 
auc.perf <- performance(pred, measure = "auc")
print(auc.perf@y.values)

#feature importance
get_drop_SVM <- function(formular, kernel, cost, gamma, weights){
  columns = names(entityNoteEva.train[, 1:8])
  model <- svm(formular, data = entityNoteEva.train, method = "C-classification", kernel = kernel, cost = cost, gamma = gamma, class.weights = weights, cross = 10)
  pred_rf = predict(model, entityNoteEva.test)
  com = confusionMatrix(pred_rf, entityNoteEva.test[,9])
  fullAcc = com$overall[1]
  print(fullAcc)
  
  imp <- c()
  for (c in columns){
    train_sub <- entityNoteEva.train[, !(colnames(entityNoteEva.train) == c)]
    test_sub <- entityNoteEva.test[, !(colnames(entityNoteEva.test) == c)]
    
    model <- svm(formular, data=train_sub, method = "C-classification", kernel = kernel, cost = cost, gamma = gamma, class.weights = weights, cross = 10)
    
    pred_rf = predict(model, test_sub)
    com = confusionMatrix(pred_rf, test_sub[, 8])
    subAcc = com$overall[1]
    difAcc = fullAcc - subAcc
    imp <- c(imp, difAcc)
  }
  featureImportance <- data.frame(Feature=columns, Importance=imp)
  print(featureImportance[order(-featureImportance$Importance),])
}

get_drop_SVM(category ~., "radial", .25, 4, c("c" = 1, "g" = 3.862, "s" = 1.737))
get_drop_SVM(detail ~., "radial", 1024, .00390625, c("0" = 1, "0.5" = 1.412, "1" = 1.441) )
get_drop_SVM(prior ~., "radial", 16, .03125, c("0" = 1, "1" = 2.362))