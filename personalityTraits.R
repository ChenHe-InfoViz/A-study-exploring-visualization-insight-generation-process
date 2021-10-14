library(glmnet)
library(e1071)
library(caret)
library(healthcareai)
library(boot)
personality = read.csv("data/personality.csv")
#Fig.13 export as PDF: portrait 6.2*4 inches
ggplot(stack(personality[,c(2:7)]), aes(x = ind, y = values)) + scale_y_continuous(limits=c(-8, 8), breaks=c(-8,-4, 0, 4,8), expand = c(0.02, 0.02)) + theme_bw() + 
  geom_boxplot() + theme(text = element_text(size=22, face="bold"), axis.text.x = element_text(angle = 30, vjust = 1, hjust=.9),axis.title.x = element_blank(),panel.border = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank())

#personality traits versus note characteristics
noteCharaUser = read.csv("data/noteAssessmentPerParticipant.csv")
personNoteChara = merge(personality, noteCharaUser, by = "user")
tauFunc<-function(data,i){cor(x = data[,1][i], y = data[,2][i], method="kendall")}

cor.test(personNoteChara$ExternalLoC, personNoteChara$priorMean, method="kendall")
bootTau = boot(personNoteChara[, c('ExternalLoC', 'priorMean')], tauFunc, 2000)
boot.ci(bootTau)

cor.test(personNoteChara$Imagination, personNoteChara$comparison, method="kendall")
bootTau = boot(personNoteChara[, c('Imagination', 'comparison')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(personNoteChara$Imagination, personNoteChara$detailMean, method="kendall")
bootTau = boot(personNoteChara[, c('Imagination', 'detailMean')], tauFunc, 2000)
boot.ci(bootTau)

#personality traits versus interaction
selectUser = read.csv("data/entitySelectionUser.csv")
selectPerson = merge(selectUser, personality, by = "user")

cor.test(selectPerson$Extraversion, selectPerson$uniqCountry, method="kendall")
bootTau = boot(selectPerson[, c('Extraversion', 'uniqCountry')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(selectPerson$ExternalLoC, selectPerson$uniqCountry, method="kendall")
bootTau = boot(selectPerson[, c('ExternalLoC', 'uniqCountry')], tauFunc, 2000)
boot.ci(bootTau)
cor.test(selectPerson$Conscientiousness, selectPerson$uniqCountry + selectPerson$uniqYear, method="kendall")
bootTau = boot(data.frame(cons = selectPerson$Conscientiousness, entity = selectPerson$uniqCountry + selectPerson$uniqYear), tauFunc, 2000)
boot.ci(bootTau)

actionUser = read.csv("data/actionUser.csv")
actionUser$exploreData = rowSums(actionUser[, c('selectCountry','deselectCountry', 'deselectAll', 'hoverCountryLabel', 'selectYear', 'hoverYearLabel', 'play', 'stop', 'hoverMapPoint', 'hoverSpecificYear', 'hoverMouseline', 'hoverTimeline')])
actionUser$exploreNote = rowSums(actionUser[, c('showNotes','hideNotes','selectYearNote', 'hoverYearLabelNote', 'playNote', 'stopNote', 'hoverMapPointNote', 'hoverSpecificYearNote', 'hoverMouselineNote', 'hoverTimelineNote', 'viewCountryNote', 'viewYearNote', 'dragNode', 'scrollToView', 'viewDiscussion', 'removeDiscussion', 'hoverNode', 'hoverNoteText', 'hoverNoteInNote', 'onlyShowMine', 'alsoShowPublic', 'removeFilter')])
actionUser$editAction = rowSums(actionUser[, c('clickInputNote', 'cancelNote', 'clickEditNote', 'saveNote', 'updateNote', 'replyToNote', 'deleteNote', 'hoverTextarea', 'hoverEntityInNote', 'addEntity', 'removeEntity', 'addEntityRepeat', 'removeFromYear', 'removeFromChart')])

actionPerson = merge(personality, actionUser, by = "user")
cor.test(actionPerson$exploreNote, actionPerson$Imagination, method="kendall")
bootTau = boot(actionPerson[, c('exploreNote', 'Imagination')], tauFunc, 2000)
boot.ci(bootTau)

########################using interaction types to predict personality traits
actionNote = read.csv("data/actionNote.csv")
actPersonbyNote = merge(actionNote, personality, by = "user", sort = F)
actPersonbyNote$toHidden = NULL
actPersonbyNote$stop = NULL
actPersonbyNote$stopNote = NULL
actPersonbyNote$removeDiscussion = NULL
actPersonbyNote$gotoQuestionnaire = NULL
actPersonbyNote$startSession = NULL
#less than two users:
actPersonbyNote$hoverNode = NULL

split = split_train_test(actPersonbyNote, Agreeableness, percent_train = 0.8, seed = 2020, user)
actPersonbyNote.train = as.matrix(split$train[, c(3:50)])
actPersonbyNote.trainTarget = split$train[, 52]
actPersonbyNote.test = as.matrix(split$test[, c(3:50)])
actPersonbyNote.testTarget = split$test[, 52]
table(actPersonbyNote.trainTarget)

actPersonbyNote.trainFeatures <- scale(actPersonbyNote.train)
col_means_train <- attr(actPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actPersonbyNote.trainFeatures, "scaled:scale")
actPersonbyNote.testFeatures <- scale(actPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
actPersonbyNote.trainFeatures = actPersonbyNote.trainFeatures[, !(colSums(is.na(actPersonbyNote.trainFeatures)) > 100)]
actPersonbyNote.testFeatures = actPersonbyNote.testFeatures[, !(colSums(is.na(actPersonbyNote.testFeatures)) > 20)]

#SVM
model <- svm(x = actPersonbyNote.trainFeatures, y = actPersonbyNote.trainTarget, kernel = "radial", cost = 2^9, gamma = 2^-8, epsilon = .1, cross = 10, scale = F)
predic <- predict(model, actPersonbyNote.testFeatures)
postResample(predic, actPersonbyNote.testTarget)
cor.test(predic, actPersonbyNote.testTarget, method="kendall")

split = split_train_test(actPersonbyNote, Neuroticism, percent_train = 0.8, seed = 2020, user)
actPersonbyNote.train = as.matrix(split$train[, c(3:50)])
actPersonbyNote.trainTarget = split$train[, 54]
actPersonbyNote.test = as.matrix(split$test[, c(3:50)])
actPersonbyNote.testTarget = split$test[, 54]
table(actPersonbyNote.trainTarget)

actPersonbyNote.trainFeatures <- scale(actPersonbyNote.train)
col_means_train <- attr(actPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actPersonbyNote.trainFeatures, "scaled:scale")
actPersonbyNote.testFeatures <- scale(actPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
actPersonbyNote.trainFeatures = actPersonbyNote.trainFeatures[, !(colSums(is.na(actPersonbyNote.trainFeatures)) > 100)]
actPersonbyNote.testFeatures = actPersonbyNote.testFeatures[, !(colSums(is.na(actPersonbyNote.testFeatures)) > 20)]

#elastic Net
lambda = 0.03349292
model <- glmnet(actPersonbyNote.trainFeatures, actPersonbyNote.trainTarget, alpha = 0.5, lambda = lambda, standardize = F)
pred_rf = predict(model, s = lambda, newx = actPersonbyNote.testFeatures) 
postResample(pred_rf, actPersonbyNote.testTarget)
cor.test(pred_rf, actPersonbyNote.testTarget, method="kendall")

#SVM
model <- svm(x = actPersonbyNote.trainFeatures, y = actPersonbyNote.trainTarget, kernel = "radial", cost = 8, gamma = .5, epsilon = 2, cross = 10, scale = F)
predic <- predict(model, actPersonbyNote.testFeatures)
postResample(predic, actPersonbyNote.testTarget)
cor.test(predic, actPersonbyNote.testTarget, method="kendall")

split = split_train_test(actPersonbyNote, Imagination, percent_train = 0.8, seed = 2020, user)
actPersonbyNote.train = as.matrix(split$train[, c(3:50)])
actPersonbyNote.trainTarget = split$train[, 55]
actPersonbyNote.test = as.matrix(split$test[, c(3:50)])
actPersonbyNote.testTarget = split$test[, 55]
table(actPersonbyNote.trainTarget)

actPersonbyNote.trainFeatures <- scale(actPersonbyNote.train)
col_means_train <- attr(actPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actPersonbyNote.trainFeatures, "scaled:scale")
actPersonbyNote.testFeatures <- scale(actPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
actPersonbyNote.trainFeatures = actPersonbyNote.trainFeatures[, !(colSums(is.na(actPersonbyNote.trainFeatures)) > 100)]
actPersonbyNote.testFeatures = actPersonbyNote.testFeatures[, !(colSums(is.na(actPersonbyNote.testFeatures)) > 20)]

#imagination elastic net
lambda = 0.3298770
lasso_model <- glmnet(noteActionPerson.trainFeatures, noteActionPerson.trainTarget, alpha = .85, lambda = lambda, standardize = F)
pred_rf = predict(lasso_model, s = lambda, noteActionPerson.testFeatures) 
postResample(pred_rf, noteActionPerson.testTarget)
cor.test(pred_rf, noteActionPerson.testTarget, method="kendall")

#imagination SVM
model <- svm(x = actPersonbyNote.trainFeatures, y = actPersonbyNote.trainTarget, kernel = "radial", cost = .5, gamma = 2^-7, epsilon = .00001, cross = 10, scale = F)
predic <- predict(model, actPersonbyNote.testFeatures)
postResample(predic, actPersonbyNote.testTarget)
cor.test(predic, actPersonbyNote.testTarget, method="kendall")

split = split_train_test(actPersonbyNote, ExternalLoC, percent_train = 0.8, seed = 2020, user)
actPersonbyNote.train = as.matrix(split$train[, c(3:50)])
actPersonbyNote.trainTarget = split$train[, 56]
actPersonbyNote.test = as.matrix(split$test[, c(3:50)])
actPersonbyNote.testTarget = split$test[, 56]
table(actPersonbyNote.trainTarget)

actPersonbyNote.trainFeatures <- scale(actPersonbyNote.train)
col_means_train <- attr(actPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actPersonbyNote.trainFeatures, "scaled:scale")
actPersonbyNote.testFeatures <- scale(actPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
actPersonbyNote.trainFeatures = actPersonbyNote.trainFeatures[, !(colSums(is.na(actPersonbyNote.trainFeatures)) > 100)]
actPersonbyNote.testFeatures = actPersonbyNote.testFeatures[, !(colSums(is.na(actPersonbyNote.testFeatures)) > 20)]

#external LoC SVM
model <- svm(x = actPersonbyNote.trainFeatures, y = actPersonbyNote.trainTarget, kernel = "radial", cost = 2^-9, gamma = 2^-4, epsilon = .1, cross = 10, scale = F)
predic <- predict(model, actPersonbyNote.testFeatures)
postResample(predic, actPersonbyNote.testTarget)
cor.test(predic, actPersonbyNote.testTarget, method="kendall")

########################using interaction patterns to predict personality traits
patternNote = read.csv("data/patternNote.csv")
patPersonbyNote = merge(patternNote, personality, by = "user", sort = F)

split = split_train_test(patPersonbyNote, Extraversion, percent_train = 0.8, seed = 2020, user)
patPersonbyNote.train = as.matrix(split$train[, c(3:31)])
patPersonbyNote.trainTarget = split$train[, 32]
patPersonbyNote.test = as.matrix(split$test[, c(3:31)])
patPersonbyNote.testTarget = split$test[, 32]
table(patPersonbyNote.trainTarget)

#normalize features
patPersonbyNote.trainFeatures = scale(patPersonbyNote.train)
col_means_train = attr(patPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train = attr(patPersonbyNote.trainFeatures, "scaled:scale")
patPersonbyNote.testFeatures = scale(patPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
patPersonbyNote.trainFeatures = patPersonbyNote.trainFeatures[, !(colSums(is.na(patPersonbyNote.trainFeatures)) > 100)]
patPersonbyNote.testFeatures = patPersonbyNote.testFeatures[, !(colSums(is.na(patPersonbyNote.testFeatures)) > 20)]

#extraversion SVM
model <- svm(x = patPersonbyNote.trainFeatures, y = patPersonbyNote.trainTarget, kernel = "radial", cost = 16, gamma = 4, epsilon = .001, cross = 10, scale = F)
predic <- predict(model, patPersonbyNote.testFeatures)
postResample(predic, patPersonbyNote.testTarget)
cor.test(predic, patPersonbyNote.testTarget, method="kendall")


#conscientiousness
split = split_train_test(patPersonbyNote, Conscientiousness, percent_train = 0.8, seed = 2020, user)
patPersonbyNote.train = as.matrix(split$train[, c(3:31)])
patPersonbyNote.trainTarget = split$train[, 34]
patPersonbyNote.test = as.matrix(split$test[, c(3:31)])
patPersonbyNote.testTarget = split$test[, 34]
table(patPersonbyNote.trainTarget)

#normalize features
patPersonbyNote.trainFeatures = scale(patPersonbyNote.train)
col_means_train = attr(patPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train = attr(patPersonbyNote.trainFeatures, "scaled:scale")
patPersonbyNote.testFeatures = scale(patPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
patPersonbyNote.trainFeatures = patPersonbyNote.trainFeatures[, !(colSums(is.na(patPersonbyNote.trainFeatures)) > 100)]
patPersonbyNote.testFeatures = patPersonbyNote.testFeatures[, !(colSums(is.na(patPersonbyNote.testFeatures)) > 20)]

#conscientiousness SVM
model <- svm(x = patPersonbyNote.trainFeatures, y = patPersonbyNote.trainTarget, kernel = "radial", cost = 2, gamma = .5, epsilon = .001, cross = 10, scale = F)
predic <- predict(model, patPersonbyNote.testFeatures)
postResample(predic, patPersonbyNote.testTarget)
cor.test(predic, patPersonbyNote.testTarget, method="kendall")

#neuroticism
split = split_train_test(patPersonbyNote, Neuroticism, percent_train = 0.8, seed = 2020, user)
patPersonbyNote.train = as.matrix(split$train[, c(3:31)])
patPersonbyNote.trainTarget = split$train[, 35]
patPersonbyNote.test = as.matrix(split$test[, c(3:31)])
patPersonbyNote.testTarget = split$test[, 35]
table(patPersonbyNote.trainTarget)

#normalize features
patPersonbyNote.trainFeatures = scale(patPersonbyNote.train)
col_means_train = attr(patPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train = attr(patPersonbyNote.trainFeatures, "scaled:scale")
patPersonbyNote.testFeatures = scale(patPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
patPersonbyNote.trainFeatures = patPersonbyNote.trainFeatures[, !(colSums(is.na(patPersonbyNote.trainFeatures)) > 100)]
patPersonbyNote.testFeatures = patPersonbyNote.testFeatures[, !(colSums(is.na(patPersonbyNote.testFeatures)) > 20)]

#neuroticism EN
lambda = 0.81225240
model <- glmnet(patPersonbyNote.trainFeatures, patPersonbyNote.trainTarget, alpha = 0, lambda = lambda, standardize = F)
pred_rf = predict(model, s = lambda, newx = patPersonbyNote.testFeatures) 
postResample(pred_rf, patPersonbyNote.testTarget)
cor.test(pred_rf, patPersonbyNote.testTarget, method="kendall")

#neuroticism SVM
model <- svm(x = patPersonbyNote.trainFeatures, y = patPersonbyNote.trainTarget, kernel = "radial", cost = 2^7, gamma = 2^-18, epsilon = .001, cross = 10, scale = F)
predic <- predict(model, patPersonbyNote.testFeatures)
postResample(predic, patPersonbyNote.testTarget)
cor.test(predic, patPersonbyNote.testTarget, method="kendall")

#imagination
split = split_train_test(patPersonbyNote, Imagination, percent_train = 0.8, seed = 2020, user)
patPersonbyNote.train = as.matrix(split$train[, c(3:31)])
patPersonbyNote.trainTarget = split$train[, 36]
patPersonbyNote.test = as.matrix(split$test[, c(3:31)])
patPersonbyNote.testTarget = split$test[, 36]
table(patPersonbyNote.trainTarget)

#normalize features
patPersonbyNote.trainFeatures = scale(patPersonbyNote.train)
col_means_train = attr(patPersonbyNote.trainFeatures, "scaled:center") 
col_stddevs_train = attr(patPersonbyNote.trainFeatures, "scaled:scale")
patPersonbyNote.testFeatures = scale(patPersonbyNote.test, center = col_means_train, scale = col_stddevs_train)
patPersonbyNote.trainFeatures = patPersonbyNote.trainFeatures[, !(colSums(is.na(patPersonbyNote.trainFeatures)) > 100)]
patPersonbyNote.testFeatures = patPersonbyNote.testFeatures[, !(colSums(is.na(patPersonbyNote.testFeatures)) > 20)]

#imagination EN
lambda = 1351.176
model <- glmnet(patPersonbyNote.trainFeatures, patPersonbyNote.trainTarget, alpha = 0, lambda = lambda, standardize = F)
pred_rf = predict(model, s = lambda, newx = patPersonbyNote.testFeatures) 
postResample(pred_rf, patPersonbyNote.testTarget)
cor.test(pred_rf, patPersonbyNote.testTarget, method="kendall")

#imagination SVM
model <- svm(x = patPersonbyNote.trainFeatures, y = patPersonbyNote.trainTarget, kernel = "radial", cost = 2^-4, gamma = 2^-15, epsilon = 2, cross = 10, scale = F)
predic <- predict(model, patPersonbyNote.testFeatures)
postResample(predic, patPersonbyNote.testTarget)
cor.test(predic, patPersonbyNote.testTarget, method="kendall")
