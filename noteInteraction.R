##################note characteristics versus interaction
library(boot)
noteCharaUser = read.csv("data/noteAssessmentPerParticipant.csv")
actionUser = read.csv("data/actionUser.csv")
actionUser$exploreData = rowSums(actionUser[, c('selectCountry','deselectCountry', 'deselectAll', 'hoverCountryLabel', 'selectYear', 'hoverYearLabel', 'play', 'stop', 'hoverMapPoint', 'hoverSpecificYear', 'hoverMouseline', 'hoverTimeline')])
actionUser$exploreNote = rowSums(actionUser[, c('showNotes','hideNotes','selectYearNote', 'hoverYearLabelNote', 'playNote', 'stopNote', 'hoverMapPointNote', 'hoverSpecificYearNote', 'hoverMouselineNote', 'hoverTimelineNote', 'viewCountryNote', 'viewYearNote', 'dragNode', 'scrollToView', 'viewDiscussion', 'removeDiscussion', 'hoverNode', 'hoverNoteText', 'hoverNoteInNote', 'onlyShowMine', 'alsoShowPublic', 'removeFilter')])
actionUser$editAction = rowSums(actionUser[, c('clickInputNote', 'cancelNote', 'clickEditNote', 'saveNote', 'updateNote', 'replyToNote', 'deleteNote', 'hoverTextarea', 'hoverEntityInNote', 'addEntity', 'removeEntity', 'addEntityRepeat', 'removeFromYear', 'removeFromChart')])

actionNoteChara = merge(noteCharaUser, actionUser, by = "user")
tauFunc<-function(data,i){cor(x = data[,1][i], y = data[,2][i], method="kendall")}

#category
cor.test(actionNoteChara$statement, actionNoteChara$exploreData, method="kendall")
bootTau = boot(actionNoteChara[, c('statement', 'exploreData')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(actionNoteChara$grouping, actionNoteChara$exploreData, method="kendall")
bootTau = boot(actionNoteChara[, c('grouping', 'exploreData')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(actionNoteChara$grouping, actionNoteChara$exploreNote, method="kendall")
bootTau = boot(actionNoteChara[, c('grouping', 'exploreNote')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(actionNoteChara$grouping, actionNoteChara$editAction, method="kendall")
bootTau = boot(actionNoteChara[, c('grouping', 'editAction')], tauFunc, 2000)
boot.ci(bootTau)

#overview versus detail
actionNoteChara$mouseovers = actionNoteChara$hoverMapPoint + actionNoteChara$hoverMouseline + actionNoteChara$hoverSpecificYear + actionNoteChara$hoverTimeline
cor.test(actionNoteChara$detailMean, actionNoteChara$mouseovers, method="kendall")
bootTau = boot(actionNoteChara[, c('detailMean', 'mouseovers')], tauFunc, 2000)
boot.ci(bootTau)

#prior knowledge
cor.test(actionNoteChara$priorMean, actionNoteChara$exploreNote, method="kendall")
bootTau = boot(actionNoteChara[, c('priorMean', 'exploreNote')], tauFunc, 2000)
boot.ci(bootTau)

##################using interaction types to predict note characteristics
library(randomForest)
library(healthcareai)
library(caret)
library(ROCR)
actionNote = read.csv("data/actionNote.csv")
noteChara = read.csv("data/noteAssessment.csv")
noteChara$user = NULL
actionNoteEva = merge(actionNote, noteChara, by = "note")

actionNoteEva$toHidden = NULL
actionNoteEva$stop = NULL
actionNoteEva$stopNote = NULL
actionNoteEva$removeDiscussion = NULL
actionNoteEva$gotoQuestionnaire = NULL
actionNoteEva$startSession = NULL
#less than two users:
actionNoteEva$hoverNode = NULL

actionNoteEva$saveNote = NULL
actionNoteEva$clickInputNote = NULL
actionNoteEva$cancelNote = NULL
actionNoteEva$checkTask = NULL
actionNoteEva$checkQuestionnaire = NULL
actionNoteEva$checkTutorial = NULL
actionNoteEva$toVisible = NULL
actionNoteEva$clickEditNote = NULL
actionNoteEva$hideNotes = NULL
actionNoteEva$addEntityRepeat = NULL
actionNoteEva$deleteNote = NULL
actionNoteEva$onlyShowMine = NULL
actionNoteEva$alsoShowPublic = NULL
#random forest
actionNoteEva$category = as.factor(actionNoteEva$category)
split = split_train_test(actionNoteEva, category, percent_train = 0.8, seed = 2020, user)
actionNoteEva.trainForest = split$train[, c(3:37, 38)]
actionNoteEva.testForest = split$test[, c(3:37, 38)]
table(actionNoteEva.trainForest$category)

optimalRF = randomForest(category ~., data = actionNoteEva.trainForest, strata = actionNoteEva.trainForest[, 36], sampsize = rep(60,3), 
                              ntree = 2000,
                              mtry = 1,
                              nodesize = 9
)
pred = predict(optimalRF, actionNoteEva.testForest)
confusionMatrix(pred, actionNoteEva.testForest[, 36])

prediction_for_roc_curve <- predict(optimalRF, actionNoteEva.testForest, type = 'prob')
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c("s","c","g")
for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(actionNoteEva.testForest[, 36]==classes[i], 1, 0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  
  if (i == 1){
    plot(perf, main = "ROC Curve", col = pretty_colours[i]) 
  }
  else{
    plot(perf, main = "ROC Curve", col = pretty_colours[i], add = TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

actionNoteEva$detail = as.factor(actionNoteEva$detail)
split = split_train_test(actionNoteEva[!(actionNoteEva$detail == -1),], detail, percent_train = 0.8, seed = 2020, user)
split$train$detail = factor(split$train$detail)
split$test$detail = factor(split$test$detail)
actionNoteEva.trainForest = split$train[, c(3:37, 39)]
actionNoteEva.testForest = split$test[, c(3:37, 39)]
table(actionNoteEva.trainForest$detail)

optimalRF = randomForest(detail ~., data = actionNoteEva.trainForest, strata = actionNoteEva.trainForest[, 36], sampsize = rep(150,3), 
                         ntree = 2000,
                         mtry = 9,
                         nodesize = 7
)
pred = predict(optimalRF, actionNoteEva.testForest)
confusionMatrix(pred, actionNoteEva.testForest[, 36])

prediction_for_roc_curve <- predict(optimalRF, actionNoteEva.testForest, type = 'prob')
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c("0","0.5","1")
for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(actionNoteEva.testForest[, 36]==classes[i], 1, 0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  
  if (i == 1){
    plot(perf, main = "ROC Curve", col = pretty_colours[i]) 
  }
  else{
    plot(perf, main = "ROC Curve", col = pretty_colours[i], add = TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

actionNoteEva$prior = as.factor(actionNoteEva$prior)
split = split_train_test(actionNoteEva, prior, percent_train = 0.8, seed = 2020, user)
actionNoteEva.trainForest = split$train[, c(3:37, 40)]
actionNoteEva.testForest = split$test[, c(3:37, 40)]
table(actionNoteEva.trainForest$prior)

optimalRF = randomForest(prior ~., data = actionNoteEva.trainForest, strata = actionNoteEva.trainForest[, 36], sampsize = rep(199, 2), 
                              ntree = 2000,
                              mtry = 7,
                              nodesize = 1)
pred = predict(optimalRF, actionNoteEva.testForest)
confusionMatrix(pred, actionNoteEva.testForest[, 36])

prediction_for_roc_curve <- predict(optimalRF, actionNoteEva.testForest, type='prob')
pred <- prediction(prediction_for_roc_curve[,c('1')], actionNoteEva.testForest[, 36])
perf <- performance(pred, "tpr", "fpr")
plot(perf,main="ROC Curve",col = 'red') 
auc.perf <- performance(pred, measure = "auc")
auc.perf@y.values

get_drop_RF <- function(formular, sampsize, ntree, mtry, nodesize){
  
  columns = names(actionNoteEva.trainForest[, 1:35])
  featureImportance <- data.frame(Feature = columns)
  
  for(i in 1:10){
    model <- randomForest(formular, data = actionNoteEva.trainForest, strata = actionNoteEva.trainForest[, 36], 
                          sampsize = sampsize, #20
                          ntree = ntree,#5000,
                          mtry = mtry,#5,
                          nodesize = nodesize,#8,, 
                          importance=T)
    pred_rf  = predict(model, actionNoteEva.testForest)
    com = confusionMatrix(pred_rf, actionNoteEva.testForest[, 36])
    fullAcc = com$overall[1]
    print(fullAcc)
    
    imp <- c()
    for (c in columns){
      train_sub <- actionNoteEva.trainForest[, !(colnames(actionNoteEva.trainForest) == c)]
      test_sub <- actionNoteEva.testForest[, !(colnames(actionNoteEva.testForest) == c)]
      model <- randomForest(formular, data = train_sub, strata = train_sub[,35], 
                            sampsize = sampsize, #20
                            ntree = ntree,#5000,
                            mtry = mtry,#5,
                            nodesize = nodesize,#8, 
                            importance=T)
      pred_rf  = predict(model, test_sub)
      com = confusionMatrix(pred_rf, test_sub[,35])
      subAcc = com$overall[1]
      difAcc = fullAcc - subAcc
      imp <- c(imp, difAcc)
    }
    featureImportance = cbind(featureImportance, imp)
  }
  featureImportance$mean = rowMeans(featureImportance[, 2:11])
  print(featureImportance[order(-featureImportance$mean),])
}

get_drop_RF(category ~., rep(60, 3), 2000, 1, 9)
get_drop_RF(detail ~., rep(150, 3), 2000, 9, 7)
get_drop_RF(prior~., rep(199, 2), 2000, 7, 1)

#SVM
actionNoteEva$category = as.factor(actionNoteEva$category)
split = split_train_test(actionNoteEva, category, percent_train = 0.8, seed = 2020, user)
actionNoteEva.train = as.matrix(split$train[, c(3:37)])
actionNoteEva.trainTarget = split$train[, 38]
actionNoteEva.test = as.matrix(split$test[, c(3:37)])
actionNoteEva.testTarget = split$test[, 38]
table(actionNoteEva.trainTarget)

actionNoteEva.trainFeatures <- scale(actionNoteEva.train)
col_means_train <- attr(actionNoteEva.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actionNoteEva.trainFeatures, "scaled:scale")
actionNoteEva.testFeatures <- scale(actionNoteEva.test, center = col_means_train, scale = col_stddevs_train)
actionNoteEva.trainFeatures = actionNoteEva.trainFeatures[, !(colSums(is.na(actionNoteEva.trainFeatures)) > 100)]
actionNoteEva.testFeatures = actionNoteEva.testFeatures[, !(colSums(is.na(actionNoteEva.testFeatures)) > 20)]

model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = .25, gamma = 2^-6,
             class.weights = c("c" = 1, "g" = 3.862, "s" = 1.737), cross = 10, scale = F)
pred = predict(model, actionNoteEva.testFeatures)
confusionMatrix(pred, actionNoteEva.testTarget)

model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = .25, gamma = 2^-6,
             class.weights = c("c" = 1, "g" = 3.862, "s" = 1.737), cross = 10, scale = F, probability = T)
prediction_for_roc_curve <- predict(model, actionNoteEva.testFeatures, probability = T)
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c('s', 'c', 'g')
for (i in 1:3){
  true_values <- ifelse(actionNoteEva.testTarget == classes[i],1,0)
  pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, main = "ROC Curve", col = pretty_colours[i]) 
  }
  else
  {
    plot(perf, main = "ROC Curve", col = pretty_colours[i], add = TRUE) 
  }
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

actionNoteEva$detail = as.factor(actionNoteEva$detail)
split = split_train_test(actionNoteEva[!(actionNoteEva$detail == -1),], detail, percent_train = 0.8, seed = 2020, user)
split$train$detail = factor(split$train$detail)
split$test$detail = factor(split$test$detail)
actionNoteEva.train = as.matrix(split$train[, c(3:37)])
actionNoteEva.trainTarget = split$train[, 39]
actionNoteEva.test = as.matrix(split$test[, c(3:37)])
actionNoteEva.testTarget = split$test[, 39]
table(actionNoteEva.trainTarget)

actionNoteEva.trainFeatures <- scale(actionNoteEva.train)
col_means_train <- attr(actionNoteEva.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actionNoteEva.trainFeatures, "scaled:scale")
actionNoteEva.testFeatures <- scale(actionNoteEva.test, center = col_means_train, scale = col_stddevs_train)
actionNoteEva.trainFeatures = actionNoteEva.trainFeatures[, !(colSums(is.na(actionNoteEva.trainFeatures)) > 100)]
actionNoteEva.testFeatures = actionNoteEva.testFeatures[, !(colSums(is.na(actionNoteEva.testFeatures)) > 20)]

model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = 2^10, gamma = 2^-6,
             class.weights = c("0" = 1, "0.5" = 1.412, "1" = 1.441), cross = 10, scale = F)
pred = predict(model, actionNoteEva.testFeatures)
confusionMatrix(pred, actionNoteEva.testTarget)

model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = 2^10, gamma = 2^-6,
             class.weights = c("0" = 1, "0.5" = 1.412, "1" = 1.441), cross = 10, probability = T, scale = F)
prediction_for_roc_curve <- predict(model, actionNoteEva.testFeatures, probability = T)
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
classes = c('0', '0.5', '1')
for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(actionNoteEva.testTarget == classes[i], 1, 0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, classes[i]], true_values)
  perf <- performance(pred, "tpr", "fpr")

  if (i==1)
  {
    plot(perf, main = "ROC Curve", col = pretty_colours[i]) 
  }
  else
  {
    plot(perf, main = "ROC Curve", col = pretty_colours[i], add = TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

actionNoteEva$prior = as.factor(actionNoteEva$prior)
split = split_train_test(actionNoteEva, prior, percent_train = 0.8, seed = 2020, user)
actionNoteEva.train = as.matrix(split$train[, c(3:37)])
actionNoteEva.trainTarget = split$train[, 40]
actionNoteEva.test = as.matrix(split$test[, c(3:37)])
actionNoteEva.testTarget = split$test[, 40]
table(actionNoteEva.trainTarget)

actionNoteEva.trainFeatures <- scale(actionNoteEva.train)
col_means_train <- attr(actionNoteEva.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actionNoteEva.trainFeatures, "scaled:scale")
actionNoteEva.testFeatures <- scale(actionNoteEva.test, center = col_means_train, scale = col_stddevs_train)
actionNoteEva.trainFeatures = actionNoteEva.trainFeatures[, !(colSums(is.na(actionNoteEva.trainFeatures)) > 100)]
actionNoteEva.testFeatures = actionNoteEva.testFeatures[, !(colSums(is.na(actionNoteEva.testFeatures)) > 20)]

model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = 2^6, gamma = 2^-5, class.weights = c("0" = 1, "1" = 2.362), cross = 10, scale = F)
pred = predict(model, actionNoteEva.testFeatures)
confusionMatrix(pred, actionNoteEva.testTarget)

model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = 2^6, gamma = 2^-5, class.weights = c("0" = 1, "1" = 2.362), cross = 10, scale = F, probability = T)
prediction_for_roc_curve <- predict(model, actionNoteEva.testFeatures, probability = T)

pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, c('1')], actionNoteEva.testTarget)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve", col = 'red') 
auc.perf <- performance(pred, measure = "auc")
print(auc.perf@y.values)

get_drop_SVM <- function(kernel, cost, gamma, weights){

  columns = colnames(actionNoteEva.trainFeatures)
  model <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = kernel, cost = cost, gamma = gamma, class.weights = weights, cross = 10)
  pred_rf = predict(model, actionNoteEva.testFeatures)
  com = confusionMatrix(pred_rf, actionNoteEva.testTarget)
  fullAcc = com$overall[1]
  print(fullAcc)
  
  imp <- c()
  for (c in columns){
    train_sub <- actionNoteEva.trainFeatures[, !(colnames(actionNoteEva.trainFeatures) == c)]
    test_sub <- actionNoteEva.testFeatures[, !(colnames(actionNoteEva.testFeatures) == c)]
    
    model <- svm(train_sub, actionNoteEva.trainTarget, method = "C-classification", kernel = kernel, cost = cost, gamma = gamma, class.weights = weights, cross = 10)
    
    pred_rf = predict(model, test_sub)
    com = confusionMatrix(pred_rf, actionNoteEva.testTarget)
    subAcc = com$overall[1]
    difAcc = fullAcc - subAcc
    imp <- c(imp, difAcc)
  }
  featureImportance <- data.frame(Feature=columns, Importance=imp)
  print(featureImportance[order(-featureImportance$Importance),])
}

get_drop_SVM("radial", .25, 2^-6, c("c" = 1, "g" = 3.862, "s" = 1.737))
get_drop_SVM("radial", 2^10, 2^-6, c("0" = 1, "0.5" = 1.412, "1" = 1.441) )
get_drop_SVM("radial", 2^6, 2^-5, c("0" = 1, "1" = 2.362))

###########################