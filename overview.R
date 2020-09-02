actionUser = read.csv("data/actionUser.csv")
selectUser = read.csv("data/entitySelectionUser.csv")

#number of data exploration actions performed
actionUser$exploreData = rowSums(actionUser[, c('selectCountry','deselectCountry', 'deselectAll', 'hoverCountryLabel', 'selectYear', 'hoverYearLabel', 'play', 'stop', 'hoverMapPoint', 'hoverSpecificYear', 'hoverMouseline', 'hoverTimeline')])
max(actionUser$exploreData)
min(actionUser$exploreData)
mean(actionUser$exploreData)
sd(actionUser$exploreData)

#total time spent
max(actionUser$totalTime)
min(actionUser$totalTime)
mean(actionUser$totalTime)
sd(actionUser$totalTime)

#total actions performed
max(actionUser$totalActions)
min(actionUser$totalActions)
mean(actionUser$totalActions)
sd(actionUser$totalActions)

#number of entities explored
max(selectUser$countryTotal)
min(selectUser$countryTotal)
mean(selectUser$countryTotal)
sd(selectUser$countryTotal)

max(selectUser$yearTotal)
min(selectUser$yearTotal)
mean(selectUser$yearTotal)
sd(selectUser$yearTotal)

library(rcompanion)
wilcox.test(selectUser$countryTotal, selectUser$yearTotal, paired = T)
t = data.frame(entity = c(selectUser$countryTotal, selectUser$yearTotal), group = c(rep(0,158), rep(1,158)))
wilcoxonPairedR(t$entity, t$group, ci = T)

#number of unique entities explored
max(selectUser$uniqCountry)
min(selectUser$uniqCountry)
mean(selectUser$uniqCountry)
sd(selectUser$uniqCountry)

max(selectUser$uniqYear)
min(selectUser$uniqYear)
mean(selectUser$uniqYear)
sd(selectUser$uniqYear)
