other = read.csv("data/other.csv")
actionUser = read.csv("data/actionUser.csv")
actionUser$exploreData = rowSums(actionUser[, c('selectCountry','deselectCountry', 'deselectAll', 'hoverCountryLabel', 'selectYear', 'hoverYearLabel', 'play', 'stop', 'hoverMapPoint', 'hoverSpecificYear', 'hoverMouseline', 'hoverTimeline')])
actionUser$exploreNote = rowSums(actionUser[, c('showNotes','hideNotes','selectYearNote', 'hoverYearLabelNote', 'playNote', 'stopNote', 'hoverMapPointNote', 'hoverSpecificYearNote', 'hoverMouselineNote', 'hoverTimelineNote', 'viewCountryNote', 'viewYearNote', 'dragNode', 'scrollToView', 'viewDiscussion', 'removeDiscussion', 'hoverNode', 'hoverNoteText', 'hoverNoteInNote', 'onlyShowMine', 'alsoShowPublic', 'removeFilter')])
actionUser$editAction = rowSums(actionUser[, c('clickInputNote', 'cancelNote', 'clickEditNote', 'saveNote', 'updateNote', 'replyToNote', 'deleteNote', 'hoverTextarea', 'hoverEntityInNote', 'addEntity', 'removeEntity', 'addEntityRepeat', 'removeFromYear', 'removeFromChart')])

actOther = merge(other, actionUser, by = "user")
tauFunc<-function(data,i){cor(x = data[,1][i], y = data[,2][i], method="kendall")}

cor.test(actOther$Age, actOther$totalTime, method="kendall")
bootTau = boot(actOther[, c('Age', 'totalTime')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(actOther$Age, actOther$totalActions, method="kendall")
bootTau = boot(actOther[, c('Age', 'totalTime')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(actOther$Age, actOther$exploreNote, method="kendall")
bootTau = boot(actOther[, c('Age', 'exploreNote')], tauFunc, 2000)
boot.ci(bootTau)


##################using interactions to predict gender
library(randomForest)
library(healthcareai)
library(caret)
library(ROCR)
actionNote = read.csv("data/actionNote.csv")
actGenderbyNote = merge(actionNote, other, sort = F)
actGenderbyNote = actGenderbyNote[(actGenderbyNote$Gender == 'Female' | actGenderbyNote$Gender == 'Male'),]

actGenderbyNote$toHidden = NULL
actGenderbyNote$stop = NULL
actGenderbyNote$stopNote = NULL
actGenderbyNote$removeDiscussion = NULL
actGenderbyNote$gotoQuestionnaire = NULL
actGenderbyNote$startSession = NULL
#less than two users:
actGenderbyNote$hoverNode = NULL

#random forest
actGenderbyNote$Gender = as.factor(actGenderbyNote$Gender)
split = split_train_test(actGenderbyNote, Gender, percent_train = 0.8, seed = 2020, user)
actGenderbyNote.trainForest = split$train[, c(3:50, 52)]
actGenderbyNote.testForest = split$test[, c(3:50, 52)]
table(actGenderbyNote.trainForest$Gender)

optimalRF = randomForest(Gender ~., data = actGenderbyNote.trainForest, strata = actGenderbyNote.trainForest[, 49], sampsize = rep(50,2), 
                              ntree = 500,
                              mtry = 3,
                              nodesize = 7)
pred = predict(optimalRF, actGenderbyNote.testForest)
confusionMatrix(pred, actGenderbyNote.testForest[, 49])

prediction_for_roc_curve <- predict(optimalRF, actGenderbyNote.testForest, type='prob')
true_values <- ifelse(actGenderbyNote.testForest[, 49] == 'Female', 1, 0)
pred <- prediction(prediction_for_roc_curve[, c('Female')], true_values)
perf <- performance(pred, "tpr", "fpr")
plot(perf,main="ROC Curve",col = 'red') 
auc.perf <- performance(pred, measure = "auc")
auc.perf@y.values

#SVM
actGenderbyNote$Gender = as.factor(actGenderbyNote$Gender)
split = split_train_test(actGenderbyNote, Gender, percent_train = 0.8, seed = 2020, user)
actGenderbyNote.train = as.matrix(split$train[, c(3:50)])
actGenderbyNote.trainTarget = split$train[, 52]
actGenderbyNote.test = as.matrix(split$test[, c(3:50)])
actGenderbyNote.testTarget = split$test[, 52]

actGenderbyNote.trainFeatures <- scale(actGenderbyNote.train)
col_means_train <- attr(actGenderbyNote.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actGenderbyNote.trainFeatures, "scaled:scale")
actGenderbyNote.testFeatures <- scale(actGenderbyNote.test, center = col_means_train, scale = col_stddevs_train)
actGenderbyNote.trainFeatures = actGenderbyNote.trainFeatures[, !(colSums(is.na(actGenderbyNote.trainFeatures)) > 100)]
actGenderbyNote.testFeatures = actGenderbyNote.testFeatures[, !(colSums(is.na(actGenderbyNote.testFeatures)) > 20)]

model <- svm(actGenderbyNote.trainFeatures, actGenderbyNote.trainTarget, method = "C-classification", kernel = "radial", cost = 2^-4, gamma = 2^-6, class.weights = c("Male" = 1, "Female" = 4.288), cross = 10, scale = F)
pred = predict(model, actGenderbyNote.testFeatures)
confusionMatrix(pred, actGenderbyNote.testTarget)

model <- svm(actGenderbyNote.trainFeatures, actGenderbyNote.trainTarget, method = "C-classification", kernel = "radial", cost = 2^-4, gamma = 2^-6, class.weights = c("Male" = 1, "Female" = 4.288), cross = 10, scale = F, probability = T)
prediction_for_roc_curve <- predict(model, actGenderbyNote.testFeatures, probability = T)
true_values <- ifelse(actGenderbyNote.testTarget == 'Female', 1, 0)
pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, c('Female')], true_values)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve", col = 'red') 
auc.perf <- performance(pred, measure = "auc")
print(auc.perf@y.values)


#interaction patterns versus gender
patternNote = read.csv("data/patternNote.csv")
patGenderbyNote = merge(patternNote, other, sort = F)
patGenderbyNote = patGenderbyNote[(patGenderbyNote$Gender == 'Female' | patGenderbyNote$Gender == 'Male'),]

patGenderbyNote$Gender = as.factor(patGenderbyNote$Gender)
split = split_train_test(patGenderbyNote, Gender, percent_train = 0.8, seed = 2020, user)
patGenderbyNote.trainForest = split$train[, c(3:31, 33)]
patGenderbyNote.testForest = split$test[, c(3:31, 33)]
table(patGenderbyNote.trainForest$Gender)

optimalRF = randomForest(Gender ~., data = patGenderbyNote.trainForest, strata = patGenderbyNote.trainForest[,30], sampsize = rep(125,2), 
                         ntree = 2000,
                         mtry = 3,
                         nodesize = 3)
pred = predict(optimalRF, patGenderbyNote.testForest)
confusionMatrix(pred, patGenderbyNote.testForest[, 30])

prediction_for_roc_curve <- predict(optimalRF, patGenderbyNote.testForest, type='prob')
true_values <- ifelse(patGenderbyNote.testForest[, 30] == 'Female', 1, 0)
pred <- prediction(prediction_for_roc_curve[, c('Female')], true_values)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve",col = 'red') 
auc.perf <- performance(pred, measure = "auc")
auc.perf@y.values

patGenderbyNote$Gender = as.factor(patGenderbyNote$Gender)
split = split_train_test(patGenderbyNote, Gender, percent_train = 0.8, seed = 2020, user)
patGenderbyNote.train = as.matrix(split$train[, c(3:31)])
patGenderbyNote.trainTarget = split$train[, 33]
patGenderbyNote.test = as.matrix(split$test[, c(3:31)])
patGenderbyNote.testTarget = split$test[, 33]
table(patGenderbyNote.trainTarget)

patGenderbyNote.trainFeatures <- scale(patGenderbyNote.train)
col_means_train <- attr(patGenderbyNote.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(patGenderbyNote.trainFeatures, "scaled:scale")
patGenderbyNote.testFeatures <- scale(patGenderbyNote.test, center = col_means_train, scale = col_stddevs_train)
patGenderbyNote.trainFeatures = patGenderbyNote.trainFeatures[, !(colSums(is.na(patGenderbyNote.trainFeatures)) > 100)]
patGenderbyNote.testFeatures = patGenderbyNote.testFeatures[, !(colSums(is.na(patGenderbyNote.testFeatures)) > 20)]

model <- svm(patGenderbyNote.trainFeatures, patGenderbyNote.trainTarget, method = "C-classification", kernel = "radial", cost = 1, gamma = 2^-5, class.weights = c("Male" = 1, "Female" = 4.288), cross = 10, scale = F)
pred = predict(model, patGenderbyNote.testFeatures)
confusionMatrix(pred, patGenderbyNote.testTarget)

model <- svm(patGenderbyNote.trainFeatures, patGenderbyNote.trainTarget, method = "C-classification", kernel = "radial", cost = 1, gamma = 2^-5, class.weights = c("Male" = 1, "Female" = 4.288), cross = 10, scale = F, probability = T)
prediction_for_roc_curve <- predict(model,patGenderbyNote.testFeatures, probability = T)

true_values <- ifelse(patGenderbyNote.testTarget == 'Female', 1, 0)
pred <- prediction(attr(prediction_for_roc_curve, "probabilities")[, c('Female')], true_values)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve",col = 'red') 
auc.perf <- performance(pred, measure = "auc")
print(auc.perf@y.values)
