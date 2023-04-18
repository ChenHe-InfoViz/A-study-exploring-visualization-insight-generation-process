library(boot)
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
