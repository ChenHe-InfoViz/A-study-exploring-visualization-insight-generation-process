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

#Fig. 4 export pdf 6.5*4.5
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