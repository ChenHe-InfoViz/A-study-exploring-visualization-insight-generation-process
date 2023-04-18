##################Correlation analysis: note characteristics versus interaction
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
