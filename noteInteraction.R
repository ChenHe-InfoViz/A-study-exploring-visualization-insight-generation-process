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
library(DALEX)
library(shapper)
install_shap()
library(e1071)#SVM
library(patchwork)
library(DescTools)
library(ggbeeswarm)
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
cmRF = confusionMatrix(pred, actionNoteEva.testForest[, 36])
cmRF[["byClass"]][ , "F1"]

#SHAP
exp_rf <- explain(optimalRF, data = actionNoteEva.trainForest[,-36])
shapRFCateInt = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("level", "probability", "dim", "value", "shap")
colnames(shapRFCateInt) <- cnames
write.csv(shapRFCateInt, "shap/shapRFCateInt.csv", row.names = F)

for(i in 1:nrow(actionNoteEva.testForest)){
  shapRF = shap(exp_rf, actionNoteEva.testForest[i, -36])
  print(i)
  shapRFCateInt = shapRFCateInt[0,]
  for(j in 1:nrow(shapRF)){
    temp = shapRF[j, '_vname_']
    shapRFCateInt = rbind(shapRFCateInt, data.frame(level = shapRF[j, '_ylevel_'], probability = shapRF[j, '_yhat_'], dim = temp, value = shapRF[j, temp], shap = shapRF[j, '_attribution_']))
  }
  write.table(shapRFCateInt, file = "shap/shapRFCateInt.csv", append = T, sep = ',', col.names = F, row.names = F)
}

actionNoteEva.train = as.matrix(split$train[, c(3:37)])
actionNoteEva.trainTarget = split$train[, 38]
actionNoteEva.test = as.matrix(split$test[, c(3:37)])
actionNoteEva.testTarget = split$test[, 38]
table(actionNoteEva.trainTarget)

actionNoteEva.trainFeatures <- scale(actionNoteEva.train)
col_means_train <- attr(actionNoteEva.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actionNoteEva.trainFeatures, "scaled:scale")
actionNoteEva.testFeatures <- scale(actionNoteEva.test, center = col_means_train, scale = col_stddevs_train)
actionNoteEva.trainFeatures = as.data.frame(actionNoteEva.trainFeatures[, !(colSums(is.na(actionNoteEva.trainFeatures)) > 100)])
actionNoteEva.testFeatures = as.data.frame(actionNoteEva.testFeatures[, !(colSums(is.na(actionNoteEva.testFeatures)) > 20)])

bestSVM <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = .25, gamma = 2^-6, class.weights = c("c" = 1, "g" = 3.862, "s" = 1.737), cross = 10, scale = F, probability = T)
pred = predict(bestSVM, actionNoteEva.testFeatures)
cmSVM = confusionMatrix(pred, actionNoteEva.testTarget)
cmSVM[["byClass"]][ , "F1"]

exp_svm <- explain(bestSVM, data = actionNoteEva.trainFeatures)
shapSVMCateInt = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("level", "probability", "dim", "value", "shap")
colnames(shapSVMCateInt) <- cnames
write.csv(shapSVMCateInt, "shap/shapSVMCateInt.csv", row.names = F)

for(i in 1:nrow(actionNoteEva.testFeatures)){
  shapSVM = shap(exp_svm, actionNoteEva.testFeatures[i, ])
  print(i)
  shapSVMCateInt = shapSVMCateInt[0,]
  for(j in 1:nrow(shapSVM)){
    temp = shapSVM[j, '_vname_']
    shapSVMCateInt = rbind(shapSVMCateInt, data.frame(level = shapSVM[j, '_ylevel_'], probability = shapSVM[j, '_yhat_'], dim = temp, value = shapSVM[j, temp], shap = shapSVM[j, '_attribution_']))
  }
  write.table(shapSVMCateInt, file = "shap/shapSVMCateInt.csv", append = T, sep = ',', col.names = F, row.names = F)
}

#Fig. 9
shapRFCateInt = read.csv("shap/shapRFCateInt.csv")
featureImpRF = aggregate(.~dim, FUN = MeanAD, data = shapRFCateInt[, c('dim', 'shap')])
featureImpRF = featureImpRF[order(featureImpRF$shap, decreasing = T), ]
resultSubset = shapRFCateInt[shapRFCateInt$dim %in% featureImpRF[1:5, 'dim'], ]

plotFeature = 
  ggplot(featureImpRF[1:5,], aes(x = shap, y = factor(dim, levels = featureImpRF[5:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Select a year', 'Mouse-over a country label', 'Select a country', 'Show notes', 'Mouse-over a vertical line'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 3), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Interaction type')

plotStatement = 
  ggplot(resultSubset[resultSubset$level == 's', ], aes(x = shap, y = factor(dim, levels = featureImpRF[5:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.08, 0.045)) + scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 20, limit = c(0,40)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(color = "Count", title = "Statement") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), axis.title = element_blank(), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank())#element_text(size = 18))

plotS = plotFeature + plotStatement + plot_layout(widths = c(1, 2))

plotComparison = ggplot(resultSubset[resultSubset$level == 'c', ], aes(x = shap, y = factor(dim, levels = featureImpRF[5:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.08, 0.045)) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 20, limit = c(0,40)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(x = 'SHAP value', color = "Count", title = "Comparison") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title.y = element_blank())

plotC = plotFeature + plotComparison + plot_layout(widths = c(1, 2))

plotS / plotC
#export 11*7

####Overview versus detail
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
cmRF = confusionMatrix(pred, actionNoteEva.testForest[, 36])
cmRF[["byClass"]][ , "F1"]

#SHAP feature importance
exp_rf <- explain(optimalRF, data = actionNoteEva.trainForest[,-36])
shapRFOverviewInt = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("level", "probability","dim", "value", "shap")
colnames(shapRFOverviewInt) <- cnames
write.csv(shapRFOverviewInt, "shap/shapRFOverviewInt.csv", row.names = F)

for(i in 1:nrow(actionNoteEva.testForest)){
  shapRF = shap(exp_rf, actionNoteEva.testForest[i, -36])
  print(i)
  shapRFOverviewInt = shapRFOverviewInt[0,]
  for(j in 1:nrow(shapRF)){
    temp = dim = shapRF[j, '_vname_']
    shapRFOverviewInt = rbind(shapRFOverviewInt, data.frame(level = shapRF[j, '_ylevel_'], probability = shapRF[j, '_yhat_'], dim = temp, value = shapRF[j, temp], shap = shapRF[j, '_attribution_']))
  }
  write.table(shapRFOverviewInt, file = "shap/shapRFOverviewInt.csv", append = T, sep = ',', col.names = F, row.names = F)
}

actionNoteEva.train = as.matrix(split$train[, c(3:37)])
actionNoteEva.trainTarget = split$train[, 39]
actionNoteEva.test = as.matrix(split$test[, c(3:37)])
actionNoteEva.testTarget = split$test[, 39]
table(actionNoteEva.trainTarget)

actionNoteEva.trainFeatures <- scale(actionNoteEva.train)
col_means_train <- attr(actionNoteEva.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actionNoteEva.trainFeatures, "scaled:scale")
actionNoteEva.testFeatures <- scale(actionNoteEva.test, center = col_means_train, scale = col_stddevs_train)
actionNoteEva.trainFeatures = as.data.frame(actionNoteEva.trainFeatures[, !(colSums(is.na(actionNoteEva.trainFeatures)) > 100)])
actionNoteEva.testFeatures = as.data.frame(actionNoteEva.testFeatures[, !(colSums(is.na(actionNoteEva.testFeatures)) > 20)])

bestSVM <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = 2^10, gamma = 2^-6,
             class.weights = c("0" = 1, "0.5" = 1.412, "1" = 1.441), cross = 10, scale = F, probability = T)
pred = predict(bestSVM, actionNoteEva.testFeatures)
cmSVM = confusionMatrix(pred, actionNoteEva.testTarget)
cmSVM[["byClass"]][ , "F1"]

exp_svm <- explain(bestSVM, data = actionNoteEva.trainFeatures)
shapSVMOverviewInt = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("level", "probability", "dim", "value", "shap")
colnames(shapSVMOverviewInt) <- cnames
write.csv(shapSVMOverviewInt, "shap/shapSVMOverviewInt.csv", row.names = F)

for(i in 1:nrow(actionNoteEva.testFeatures)){
  shapSVM = shap(exp_svm, actionNoteEva.testFeatures[i, ])
  print(i)
  shapSVMOverviewInt = shapSVMOverviewInt[0,]
  for(j in 1:nrow(shapSVM)){
    temp = dim = shapSVM[j, '_vname_']
    shapSVMOverviewInt = rbind(shapSVMOverviewInt, data.frame(level = shapSVM[j, '_ylevel_'], probability = shapSVM[j, '_yhat_'], dim = temp, value = shapSVM[j, temp], shap = shapSVM[j, '_attribution_']))
  }
  write.table(shapSVMOverviewInt, file = "shap/shapSVMOverviewInt.csv", append = T, sep = ',', col.names = F, row.names = F)
}

#Fig. 10
shapRFOverviewInt = read.csv("shap/shapRFOverviewInt.csv")
featureImpRF = aggregate(.~dim, FUN = MeanAD, data = shapRFOverviewInt[, c('dim', 'shap')])
featureImpRF = featureImpRF[order(featureImpRF$shap, decreasing = T), ]
resultSubset = shapRFOverviewInt[shapRFOverviewInt$dim %in% featureImpRF[1:3, 'dim'], ]

plotFeature = 
  ggplot(featureImpRF[1:3,], aes(x = shap, y = factor(dim, levels = featureImpRF[3:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Select a country', 'Reply to a note', 'Mouse-over a line'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 3), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Interaction type')

plotOverview = 
  ggplot(resultSubset[resultSubset$level == '0', ], aes(x = shap, y = factor(dim, levels = featureImpRF[3:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.2, 0.2)) + scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 20, limit = c(0,40)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(color = "Count", title = "Overview") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), axis.title = element_blank(), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank())#element_text(size = 18))

plot0 = plotFeature + plotOverview + plot_layout(widths = c(1, 3))

plotDetail = ggplot(resultSubset[resultSubset$level == '1', ], aes(x = shap, y = factor(dim, levels = featureImpRF[3:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.2, 0.2)) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 20, limit = c(0,40)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(x = 'SHAP value', color = "Count", title = "Detail") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title.y = element_blank())

plot1 = plotFeature + plotDetail + plot_layout(widths = c(1, 3))

plot0 / plot1


######prior knowledge
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
cmRF = confusionMatrix(pred, actionNoteEva.testForest[, 36])
cmRF$byClass["F1"]

exp_rf <- explain(optimalRF, data = actionNoteEva.trainForest[,-36])
shapRFPriorInt = data.frame(matrix(ncol = 4, nrow = 0))
cnames <- c("probability", "dim", "value", "shap")
colnames(shapRFPriorInt) <- cnames
write.csv(shapRFPriorInt, "shap/shapRFPriorInt.csv", row.names = F)

for(i in 1:nrow(actionNoteEva.testForest)){
  shapRF = shap(exp_rf, actionNoteEva.testForest[i, -36])
  print(i)
  shapRFPriorInt = shapRFPriorInt[0,]
  for(j in 1:nrow(shapRF)){
    temp = shapRF[j, '_vname_']
    shapRFPriorInt = rbind(shapRFPriorInt, data.frame(probability = shapRF[j, '_yhat_'], dim = temp, value = shapRF[j, temp], shap = shapRF[j, '_attribution_']))
  }
  write.table(shapRFPriorInt, file = "shap/shapRFPriorInt.csv", append = T, sep = ',', col.names = F, row.names = F)
}

#Fig. 11
shapRFPriorInt = read.csv("shap/shapRFPriorInt.csv")
featureImpRF = aggregate(.~dim, FUN = MeanAD, data = shapRFPriorInt[, c('dim', 'shap')])
featureImpRF = featureImpRF[order(featureImpRF$shap, decreasing = T), ]
resultSubset = shapRFPriorInt[shapRFPriorInt$dim %in% featureImpRF[1:2, 'dim'], ]

plotFeature = 
  ggplot(featureImpRF[1:2,], aes(x = shap, y = factor(dim, levels = featureImpRF[2:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Mouse-over the text area', 'View notes of a country'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 2), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Interaction type')

plotWith = 
  ggplot(resultSubset[resultSubset$probability > 0.4280635, ], aes(x = shap, y = factor(dim, levels = featureImpRF[2:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.1, 0.2)) + scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 20, limit = c(0,40)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(color = "Count", title = "With prior knowledge") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), axis.title = element_blank(), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank())#element_text(size = 18))

plot0 = plotFeature + plotWith + plot_layout(widths = c(1, 2.5))

plotOut = ggplot(resultSubset[resultSubset$probability < 0.4280635, ], aes(x = shap, y = factor(dim, levels = featureImpRF[2:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.1, 0.2)) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 20, limit = c(0,40)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(x = 'SHAP value', color = "Count", title = "Without prior knowledge") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title.y = element_blank())

plot1 = plotFeature + plotOut + plot_layout(widths = c(1, 2.5))

plot0 / plot1
#export 11*4.2

#SVM
actionNoteEva.train = as.matrix(split$train[, c(3:37)])
actionNoteEva.trainTarget = split$train[, 40]
actionNoteEva.test = as.matrix(split$test[, c(3:37)])
actionNoteEva.testTarget = split$test[, 40]
table(actionNoteEva.trainTarget)

actionNoteEva.trainFeatures <- scale(actionNoteEva.train)
col_means_train <- attr(actionNoteEva.trainFeatures, "scaled:center") 
col_stddevs_train <- attr(actionNoteEva.trainFeatures, "scaled:scale")
actionNoteEva.testFeatures <- scale(actionNoteEva.test, center = col_means_train, scale = col_stddevs_train)
actionNoteEva.trainFeatures = as.data.frame(actionNoteEva.trainFeatures[, !(colSums(is.na(actionNoteEva.trainFeatures)) > 100)])
actionNoteEva.testFeatures = as.data.frame(actionNoteEva.testFeatures[, !(colSums(is.na(actionNoteEva.testFeatures)) > 20)])

bestSVM <- svm(actionNoteEva.trainFeatures, actionNoteEva.trainTarget, method = "C-classification", kernel = "radial", cost = 2^6, gamma = 2^-5, class.weights = c("0" = 1, "1" = 2.362), cross = 10, scale = F, probability = T)
pred = predict(bestSVM, actionNoteEva.testFeatures)
cmSVM = confusionMatrix(pred, actionNoteEva.testTarget)
cmSVM$byClass["F1"]

exp_svm <- explain(bestSVM, data = actionNoteEva.trainFeatures)
shapSVMPriorInt = data.frame(matrix(ncol = 4, nrow = 0))
cnames <- c("probability", "dim", "value", "shap")
colnames(shapSVMPriorInt) <- cnames
write.csv(shapSVMPriorInt, "shap/shapSVMPriorInt.csv", row.names = F)

for(i in 1:nrow(actionNoteEva.testFeatures)){
  shapSVM = shap(exp_svm, actionNoteEva.testFeatures[i, ])
  print(i)
  shapSVMPriorInt = shapSVMPriorInt[0,]
  for(j in 1:nrow(shapSVM)){
    temp = shapSVM[j, '_vname_']
    shapSVMPriorInt = rbind(shapSVMPriorInt, data.frame(probability = shapSVM[j, '_yhat_'], dim = temp, value = shapSVM[j, temp], shap = shapSVM[j, '_attribution_']))
  }
  write.table(shapSVMPriorInt, file = "shap/shapSVMPriorInt.csv", append = T, sep = ',', row.names = F, col.names = F)
}