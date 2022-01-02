actionUser = read.csv("data/actionUser.csv")
selectUser = read.csv("data/entitySelectionUser.csv")

#number of data exploration actions performed
actionUser$exploreData = rowSums(actionUser[, c('selectCountry','deselectCountry', 'deselectAll', 'hoverCountryLabel', 'selectYear', 'hoverYearLabel', 'play', 'stop', 'hoverMapPoint', 'hoverSpecificYear', 'hoverMouseline', 'hoverTimeline')])
max(actionUser$exploreData)
min(actionUser$exploreData)
median(actionUser$exploreData)

#total time spent
max(actionUser$totalTime)
min(actionUser$totalTime)
median(actionUser$totalTime)

#total actions performed
max(actionUser$totalActions)
min(actionUser$totalActions)
median(actionUser$totalActions)

######################Exploration coverage##################
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

################Note characteristics###################
library(boot)
library(ggplot2)
noteChara = read.csv("data/noteAssessment.csv")
noteChara$user = NULL

#Fig. 4
temp = noteChara
temp$detail[temp$detail == 0] <- "Overview/trend"
temp$detail[temp$detail == 1] <- "Detail"
temp$detail[temp$detail == -1] <- "No data description"
temp$detail[temp$detail == 0.5] <- "Overview/trend + detail"
temp$category[temp$category == "s"] <- "Statement"
temp$category[temp$category == "c"] <- "Comparison"
temp$category[temp$category == "g"] <- "Grouping"
temp$relation[temp$relation == -1] <- "No chart"
temp$relation[temp$relation == 1] <- "Relevant"
temp$relation[temp$relation == 0] <- "Irrelevant"
temp$prior[temp$prior == 1] <- "With"
temp$prior[temp$prior == 0] <- "Without"
temp$correctness[temp$correctness == 0] <- "Incorrect"
temp$correctness[temp$correctness == 1] <- "Correct"
temp$correctness[temp$correctness == -1] <- "No data description"

names(temp)[names(temp) == "relation"] <- "Relation"
names(temp)[names(temp) == "category"] <- "Category"
names(temp)[names(temp) == "prior"] <- "Prior knowledge"
names(temp)[names(temp) == "correctness"] <- "Correctness"
names(temp)[names(temp) == "detail"] <- "Overview versus detail"
names(temp)[names(temp) == "countries"] <- "Countries"
names(temp)[names(temp) == "years"] <- "Years"
names(temp)[names(temp) == "values"] <- "Values"

#export as PDF: portrait 16*5 inches
ggplot(stack(temp[,c(2,3,4,5,6)])) + stat_count(aes(x=factor(values, levels=c( "Statement", "Comparison", "Grouping", "Overview/trend", "Overview/trend + detail", "Detail", "With", "Without", "Correct", "Incorrect", "Relevant", "Irrelevant", "No chart","No data description")), y=..prop.., group=ind)) + facet_wrap(. ~ ind, scales = "free_x", nrow = 1) + scale_y_continuous(limits=c(0.0, 1.05), labels = scales::percent, expand = c(0.0, 0)) + theme_grey(base_size = 24) + geom_text(aes(label=scales::percent(round(..prop..,4)), x=values, y=..prop.., group=ind), stat="count", vjust=-.5, size = 5, fontface = "bold") + theme(text = element_text(size=24, face="bold"), axis.text.x = element_text(angle = 30, vjust = 1, hjust=.9), axis.title = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = unit(c(12,12,12,25), "pt"))

#export as PDF: portrait 3.5*3.5 inches
ggplot(stack(temp[,c(7,8,9)]), aes(x = ind, y = values)) + ylab("Count") +
  theme_grey(base_size = 24) + geom_boxplot() + theme(text = element_text(size=24), axis.text.x = element_text(angle = 30, vjust = 1, hjust=.9, face="bold"), axis.title.x = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank(), axis.title=element_text(size=16))

################Entity references###################
library(BSDA)
library(ggplot2)
library(boot)
entityRefUser = read.csv("data/entityReferencesPerParticipant.csv")
results = data.frame(matrix(ncol = 5, nrow = 0))
cnames <- c("Type", "Mean", "min", "max", "group")
colnames(results) <- cnames

percentileCI = function(data){
  Mboot = boot(data, function(x,i) mean(x[i]), R=2000)
  CI = boot.ci(Mboot, conf = 0.95, type = c("norm", "perc"))
  return(c(CI$percent[, 4], CI$percent[, 5]))
}

#chart component references
mean = mean(entityRefUser$vertical + entityRefUser$mapPoint + entityRefUser$line)
CI = percentileCI(entityRefUser$vertical + entityRefUser$mapPoint + entityRefUser$line)
results = rbind(results, data.frame(Type = "Chart component", Mean = mean, min = CI[1], max = CI[2], group = "1"))

mean = mean(entityRefUser$vertical)
CI = percentileCI(entityRefUser$vertical)
results = rbind(results, data.frame(Type = "Vertical reference line", Mean = mean, min = CI[1], max = CI[2], group = "2"))

mean = mean(entityRefUser$line)
CI = percentileCI(entityRefUser$line)
results = rbind(results, data.frame(Type = "Line", Mean = mean, min = CI[1], max = CI[2], group = "2"))

median = mean(entityRefUser$mapPoint)
CI = percentileCI(entityRefUser$mapPoint)
results = rbind(results, data.frame(Type = "Map point", Mean = mean, min = CI[1], max = CI[2], group = "2"))

#chart references
mean = mean(entityRefUser$lineChart + entityRefUser$map)
CI = percentileCI(entityRefUser$lineChart + entityRefUser$map)
results = rbind(results, data.frame(Type = "Chart", Mean = mean, min = CI[1], max = CI[2], group = "1"))

mean = mean(entityRefUser$lineChart)
CI = percentileCI(entityRefUser$lineChart)
results = rbind(results, data.frame(Type = "Line chart", Mean = mean, min = CI[1], max = CI[2], group = "2"))

mean = mean(entityRefUser$map)
CI = percentileCI(entityRefUser$map)
results = rbind(results, data.frame(Type = "Map", Mean = mean, min = CI[1], max = CI[2], group = "2"))

mean = mean(entityRefUser$line + entityRefUser$vertical + entityRefUser$lineChart)
CI = percentileCI(entityRefUser$line + entityRefUser$vertical + entityRefUser$lineChart)
results = rbind(results, data.frame(Type = "Line chart related", Mean = mean, min = CI[1], max = CI[2], group = "3"))

mean = mean(entityRefUser$map + entityRefUser$mapPoint)
CI = percentileCI(entityRefUser$map + entityRefUser$mapPoint)
results = rbind(results, data.frame(Type = "Map related", Mean = mean, min = CI[1], max = CI[2], group = "3"))

#Fig. 6 export pdf 6.5*4.5
ggplot(results, aes(factor(Type, levels = rev(c("Chart component", "Vertical reference line", "Line", "Map point", "Chart", "Line chart", "Map", "Line chart related", "Map related"))), Mean)) +
  geom_bar(stat="identity", aes(fill = group), width = 0.8) + geom_text(size = 5, vjust = -0.4, hjust = -0.1, data= subset(results, Mean != 0), aes(label = round(Mean, 2), fontface = 2)) +
  scale_fill_manual("legend", values = c("1" = "#f8766d", "2" = "#aaaaaa", "3" = "#00bfc4")) + 
  geom_errorbar(aes(x=Type, ymin=min, ymax=max), color="black", width = 0.05) +
  theme(text = element_text(size = 24), panel.background = element_blank(), legend.position = "none", axis.ticks = element_blank()) + labs(x = "Entity type", y = "Mean") + coord_flip()


#char component versus chart
SIGN.test(entityRefUser$vertical + entityRefUser$mapPoint + entityRefUser$line, entityRefUser$lineChart + entityRefUser$map)
#pair wise
SIGN.test(entityRefUser$lineChart, entityRefUser$line)
SIGN.test(entityRefUser$map, entityRefUser$mapPoint)
SIGN.test(entityRefUser$vertical, entityRefUser$line)
SIGN.test(entityRefUser$vertical, entityRefUser$lineChart)

#line chart-related versus map-related references
SIGN.test(entityRefUser$lineChart + entityRefUser$vertical + entityRefUser$line, entityRefUser$map + entityRefUser$mapPoint)
