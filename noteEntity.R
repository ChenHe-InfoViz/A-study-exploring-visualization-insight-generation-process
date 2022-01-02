##################using entity references to predict note characteristics
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
entityRef = read.csv("data/entityReferences.csv")
noteChara = read.csv("data/noteAssessment.csv")
noteChara$user = NULL
entityNoteEva = merge(entityRef, noteChara, by = "note")

#random forest
entityNoteEva$category = as.factor(entityNoteEva$category)
split = split_train_test(entityNoteEva, category, percent_train = 0.8, seed = 2020, user)
entityNoteEva.train = split$train[, c(3:10, 11)]
entityNoteEva.test = split$test[, c(3:10, 11)]
table(entityNoteEva.train$category)

#RF
optimalRF = randomForest(category ~., data = entityNoteEva.train, strata = entityNoteEva.train[,9], 
                              sampsize = rep(30,3),
                              ntree = 500,
                              mtry = 3,
                              nodesize = 9,
                              importance = T)

pred = predict(optimalRF, entityNoteEva.test)
cmRF = confusionMatrix(pred, entityNoteEva.test[,9])
cmRF[["byClass"]][ , "F1"]

#SHAP feature importance
exp_rf <- explain(optimalRF, data = entityNoteEva.train[,-9])
shapRFCate = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("probability", "level", "dim", "value", "shap")
colnames(shapRFCate) <- cnames
write.csv(shapRFCate, "shap/shapRFCateEntity.csv", row.names=FALSE)

for(i in 1:nrow(entityNoteEva.test)){
  shapRF = shap(exp_rf, entityNoteEva.test[i,-9])
  shapRFCate = shapRFCate[0,]
  for(j in 1:nrow(shapRF)){
    temp = dim = shapRF[j, '_vname_']
    shapRFCate = rbind(shapRFCate, data.frame(probability = shapRF[j, '_yhat_'], level = shapRF[j, '_ylevel_'], dim = temp, value = shapRF[j, temp], shap = shapRF[j, '_attribution_']))
  }
  write.table(shapRFCate, file = "shap/shapRFCateEntity.csv", append = T, sep = ',', col.names = F, row.names = F)
}

#SVM
bestSVM <- svm(category ~., data=entityNoteEva.train, method = "C-classification", kernel = "radial", cost = .25, gamma = 4, class.weights = c("c" = 1, "g" = 3.862, "s" = 1.737), cross = 10, probability = T)
pred = predict(bestSVM, entityNoteEva.test)
cmSVM = confusionMatrix(pred, entityNoteEva.test[,9])
cmSVM[["byClass"]][ , "F1"]

exp_svm <- explain(bestSVM, data = entityNoteEva.train[,-9])
shapSVMCate = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("probability", "level", "dim", "value", "shap")
colnames(shapSVMCate) <- cnames
write.csv(shapSVMCate, "shapSVMCateEntity.csv", row.names=FALSE)

for(i in 1:nrow(entityNoteEva.test)){
  shapSVM = shap(exp_svm, entityNoteEva.test[i,-9])
  shapSVMCate = shapSVMCate[0,]
  for(j in 1:nrow(shapSVM)){
    temp = dim = shapSVM[j, '_vname_']
    shapSVMCate = rbind(shapSVMCate, data.frame(probability = shapSVM[j, '_yhat_'], level = shapSVM[j, '_ylevel_'], dim = temp, value = shapSVM[j, temp], shap = shapSVM[j, '_attribution_']))
  }
  write.table(shapSVMCate, file = "shapSVMCateEntity.csv", append = T, sep = ',', col.names = F, row.names = F)
}


#Fig. 7 export 11 * 5.5 inches
shapRFCate = read.csv("shap/shapRFCateEntity.csv")
featureImpRF = aggregate(.~dim, FUN = MeanAD, data = shapRFCate[, c('dim', 'shap')])
featureImpRF = featureImpRF[order(featureImpRF$shap, decreasing = T), ]
resultSubset = shapRFCate[shapRFCate$dim %in% featureImpRF[1:3, 'dim'], ]

plotFeature = 
  ggplot(featureImpRF[1:3,], aes(x = shap, y = factor(dim, levels = featureImpRF[3:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Country', 'Vertical reference line', 'Line chart'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 2), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Entity type')

plotGrouping = 
  ggplot(resultSubset[resultSubset$level == 'g', ], aes(x = shap, y = factor(dim, levels = featureImpRF[3:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.5, 0.5)) + scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 4, limit = c(0,8)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(color = "Count", title = "Grouping") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), axis.title = element_blank(), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank())#element_text(size = 18))

plotG = plotFeature + plotGrouping + plot_layout(widths = c(1, 3.3))

plotComparison = ggplot(resultSubset[resultSubset$level == 'c', ], aes(x = shap, y = factor(dim, levels = featureImpRF[3:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.5, 0.5)) + scale_y_discrete(labels = c('Line chart', 'Vertical reference line', 'Country')) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 4, limit = c(0,8)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(x = 'SHAP value', color = "Count", title = "Comparison") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title.y = element_blank())

plotC = plotFeature + plotComparison + plot_layout(widths = c(1, 3.3))

plotG / plotC

####detail
entityNoteEva$detail = as.factor(entityNoteEva$detail)
split = split_train_test(entityNoteEva[!(entityNoteEva$detail == -1),], detail, percent_train = 0.8, seed = 2020, user)
split$train$detail = factor(split$train$detail)
split$test$detail = factor(split$test$detail)
entityNoteEva.train = split$train[, c(3:10, 12)]
entityNoteEva.test = split$test[, c(3:10, 12)]
table(entityNoteEva.train$detail)

optimalRF = randomForest(detail ~., data = entityNoteEva.train, strata = entityNoteEva.train[,9], 
                         sampsize = rep(10,3),
                         ntree = 1000,
                         mtry = 1,
                         nodesize = 12,
                         importance = T)

pred = predict(optimalRF, entityNoteEva.test)
cmRF = confusionMatrix(pred, entityNoteEva.test[,9])
cmRF[["byClass"]][ , "F1"]

exp_rf <- explain(optimalRF, data = entityNoteEva.train[,-9])
shapRFDetail = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("probability", "level", "dim", "value", "shap")
colnames(shapRFDetail) <- cnames
write.csv(shapRFDetail, "shap/shapRFDetailEntity.csv", row.names = F)

for(i in 1:nrow(entityNoteEva.test)){
  shapRF = shap(exp_rf, entityNoteEva.test[i,-9])
  shapRFDetail = shapRFDetail[0,]
  for(j in 1:nrow(shapRF)){
    temp = dim = shapRF[j, '_vname_']
    shapRFDetail = rbind(shapRFDetail, data.frame(probability = shapRF[j, '_yhat_'], level = shapRF[j, '_ylevel_'], dim = temp, value = shapRF[j, temp], shap = shapRF[j, '_attribution_']))
  }
  write.table(shapRFDetail, file = "shap/shapRFDetailEntity.csv", append = T, sep = ',', col.names = F, row.names = F)
}

bestSVM <- svm(detail ~., data = entityNoteEva.train, method = "C-classification", kernel = "radial", cost = 1024, gamma = .00390625, class.weights = c("0" = 1, "0.5" = 1.412, "1" = 1.441), cross = 10, probability = T)
pred = predict(bestSVM, entityNoteEva.test)
cmSVM = confusionMatrix(pred, entityNoteEva.test[, 9])
cmSVM[["byClass"]][ , "F1"]

exp_svm <- explain(bestSVM, data = entityNoteEva.train[,-9])
shapSVMDetail = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("probability", "level", "dim", "value", "shap")
colnames(shapSVMDetail) <- cnames
write.csv(shapSVMDetail, "shap/shapSVMDetailEntity.csv", row.names = F)

for(i in 1:nrow(entityNoteEva.test)){
  shapSVM = shap(exp_svm, entityNoteEva.test[i,-9])
  shapSVMDetail = shapSVMDetail[0,]
  for(j in 1:nrow(shapSVM)){
    temp = dim = shapSVM[j, '_vname_']
    shapSVMDetail = rbind(shapSVMDetail, data.frame(probability = shapSVM[j, '_yhat_'], level = shapSVM[j, '_ylevel_'], dim = temp, value = shapSVM[j, temp], shap = shapSVM[j, '_attribution_']))
  }
  write.table(shapSVMDetail, file = "shap/shapSVMDetailEntity.csv", append = T, sep = ',', col.names = F, row.names = F)
}

#Fig. 8
shapRFDetail = read.csv("shap/shapRFDetailEntity.csv")
featureImpRF = aggregate(.~dim, FUN = MeanAD, data = shapRFDetail[, c('dim', 'shap')])
featureImpRF = featureImpRF[order(featureImpRF$shap, decreasing = T), ]
resultSubset = shapRFDetail[shapRFDetail$dim %in% featureImpRF[1:5, 'dim'], ]

plotRFFeature = 
  ggplot(featureImpRF[1:5,], aes(x = shap, y = factor(dim, levels = featureImpRF[5:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Year', 'Vertical reference line', 'Line chart', 'Note', 'Line'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 3), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Entity type')

plotRFOverview = 
  ggplot(resultSubset[resultSubset$level == '0', ], aes(x = shap, y = factor(dim, levels = featureImpRF[5:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.25, 0.25)) + scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 4, limit = c(0,8)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(color = "Count", title = "Overview (RF)") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), axis.title = element_blank(), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank())#element_text(size = 18))

plot1 = plotRFFeature + plotRFOverview + plot_layout(widths = c(1, 3.3))

shapSVMDetail = read.csv("shap/shapSVMDetailEntity.csv")
featureImpSVM = aggregate(.~dim, FUN = MeanAD, data = shapSVMDetail[, c('dim', 'shap')])
featureImpSVM = featureImpSVM[order(featureImpSVM$shap, decreasing = T), ]
resultSubset = shapSVMDetail[shapSVMDetail$dim %in% featureImpSVM[1:5, 'dim'], ]

plotSVMFeature = 
  ggplot(featureImpSVM[1:5,], aes(x = shap, y = factor(dim, levels = featureImpSVM[5:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Year', 'Line', 'Line chart', 'Note', 'Vertical reference line'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 3), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Entity type')

plotSVMOverview = ggplot(resultSubset[resultSubset$level == 0, ], aes(x = shap, y = factor(dim, levels = featureImpSVM[5:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.25, 0.25)) + scale_y_discrete(labels = c('Line chart', 'Vertical reference line', 'Country')) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 4, limit = c(0,8)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(x = 'SHAP value', color = "Count", title = "Overview (SVM)") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title.y = element_blank())

plot2 = plotSVMFeature + plotSVMOverview + plot_layout(widths = c(1, 3.3))

plot1 / plot2
#export: 11 * 7

####Prior knowledge
entityNoteEva$prior = as.factor(entityNoteEva$prior)
split = split_train_test(entityNoteEva, prior, percent_train = 0.8, seed = 2020, user)
entityNoteEva.train = split$train[, c(3:10, 13)]
entityNoteEva.test = split$test[, c(3:10, 13)]
table(entityNoteEva.train$prior)

optimalRF = randomForest(prior ~., data = entityNoteEva.train, strata = entityNoteEva.train[,9], 
                         sampsize = rep(150,2),
                         ntree = 2000,
                         mtry = 3,
                         nodesize = 2,
                         importance = T)

pred = predict(optimalRF, entityNoteEva.test)
cmRF = confusionMatrix(pred, entityNoteEva.test[,9])
cmRF$byClass["F1"]

exp_rf <- explain(optimalRF, data = entityNoteEva.train[,-9])
shapRFPrior = data.frame(matrix(ncol = 4, nrow = 0))
cnames <- c("probability", "dim", "value", "shap")
colnames(shapRFPrior) <- cnames
write.csv(shapRFPrior, "shap/shapRFPriorEntity.csv", row.names = F)

for(i in 1:nrow(entityNoteEva.test)){
  shapRF = shap(exp_rf, entityNoteEva.test[i,-9])
  shapRFPrior = shapRFPrior[0,]
  for(j in 1:nrow(shapRF)){
    temp = dim = shapRF[j, '_vname_']
    shapRFPrior = rbind(shapRFPrior, data.frame(probability = shapRF[j, '_yhat_'], dim = temp, value = shapRF[j, temp], shap = shapRF[j, '_attribution_']))
  }
  write.table(shapRFPrior, file = "shap/shapRFPriorEntity.csv", append = T, sep = ',', col.names = F, row.names = F)
}

bestSVM <- svm(prior ~., data=entityNoteEva.train, method = "C-classification", kernel = "radial", cost = 16, gamma = .03125, class.weights = c("0" = 1, "1" = 2.362), cross = 10, probability = T)
pred = predict(bestSVM, entityNoteEva.test)
cmSVM = confusionMatrix(pred, entityNoteEva.test[, 9])
cmSVM$byClass["F1"]

exp_svm <- explain(bestSVM, data = entityNoteEva.train[,-9])
shapSVMPrior = data.frame(matrix(ncol = 4, nrow = 0))
cnames <- c("probability", "dim", "value", "shap")
colnames(shapSVMPrior) <- cnames
write.csv(shapSVMPrior, "shap/shapSVMPriorEntity.csv", row.names = F)

for(i in 1:nrow(entityNoteEva.test)){
  shapSVM = shap(exp_svm, entityNoteEva.test[i,-9])
  shapSVMPrior = shapSVMPrior[0,]
  for(j in 1:nrow(shapSVM)){
    temp = dim = shapSVM[j, '_vname_']
    shapSVMPrior = rbind(shapSVMPrior, data.frame(probability = shapSVM[j, '_yhat_'], dim = temp, value = shapSVM[j, temp], shap = shapSVM[j, '_attribution_']))
  }
  write.table(shapSVMPrior, file = "shap/shapSVMPriorEntity.csv", append = T, sep = ',', col.names = F, row.names = F)
}

#Fig. 9
shapSVMPrior = read.csv("shap/shapSVMPriorEntity.csv")
featureImpSVM = aggregate(.~dim, FUN = MeanAD, data = shapSVMPrior[, c('dim', 'shap')])
featureImpSVM = featureImpSVM[order(featureImpSVM$shap, decreasing = T), ]
resultSubset = shapSVMPrior[shapSVMPrior$dim %in% featureImpSVM[1:4, 'dim'], ]

plotSVMFeature = 
  ggplot(featureImpSVM[1:4,], aes(x = shap, y = factor(dim, levels = featureImpSVM[4:1, 'dim']))) + geom_bar(stat="identity", position = position_nudge(y = -.2), width = .4, fill = "#f8766d") + geom_text(aes(x = 0, label = c('Year', 'Vertical reference line', 'Country', 'Note'), fontface = 2), size = 5, vjust = -.3, hjust = "right") + geom_text(aes(x = 0, label = round(shap, 3), fontface = 2), size = 5, vjust = 1.2, hjust = "right") + theme(axis.title.x = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(face="bold", size = 18)) + scale_x_reverse() + labs(y = 'Entity type')

plotSVMPrior = ggplot(resultSubset[resultSubset$probability > 0.2996, ], aes(x = shap, y = factor(dim, levels = featureImpSVM[4:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.1, 0.25)) + scale_y_discrete(labels = c('Line chart', 'Vertical reference line', 'Country')) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 4, limit = c(0,8)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(color = "Count", title = "With prior knowledge") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title = element_blank())

plotSVMOut = ggplot(resultSubset[resultSubset$probability < 0.2996, ], aes(x = shap, y = factor(dim, levels = featureImpSVM[4:1, 'dim']), color = value)) + geom_vline(xintercept = 0, color = "red") + scale_x_continuous(limit = c(-0.1, 0.25)) + scale_y_discrete(labels = c('Line chart', 'Vertical reference line', 'Country')) +
  scale_color_gradient2(low = "#f6dc02", high = "#345f8d", mid = "#38b977", midpoint = 4, limit = c(0,8)) + geom_beeswarm(alpha = .5, cex = 1, priority = "random") +
  geom_jitter(width = 0, height = 0.3, alpha = .5) + labs(x = 'SHAP value', color = "Count", title = "Without prior knowledge") + theme(panel.background = element_blank(), text = element_text(size=18, face="bold"), plot.title = element_text(hjust = 1, vjust = -2.5), axis.text.y = element_blank(), axis.title.y = element_blank())

plot1 = plotSVMFeature + plotSVMPrior + plot_layout(widths = c(1, 3.3))
plot2 = plotSVMFeature + plotSVMOut + plot_layout(widths = c(1, 3.3))

plot1 / plot2


##################Correlation analysis: note characteristics versus entity references
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
