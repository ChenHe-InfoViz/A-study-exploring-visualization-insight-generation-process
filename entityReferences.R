library(BSDA)
entityRefUser = read.csv("data/entityReferencesPerParticipant.csv")
#visual component references
mean(entityRefUser$vertical + entityRefUser$mapPoint + entityRefUser$line)
sd(entityRefUser$vertical + entityRefUser$mapPoint + entityRefUser$line)

#chart references
mean(entityRefUser$lineChart + entityRefUser$map)
sd(entityRefUser$lineChart + entityRefUser$map)

SIGN.test(entityRefUser$vertical + entityRefUser$mapPoint + entityRefUser$line, entityRefUser$lineChart + entityRefUser$map)
SIGN.test(entityRefUser$vertical, entityRefUser$line)
SIGN.test(entityRefUser$vertical, entityRefUser$lineChart)

SIGN.test(entityRefUser$lineChart, entityRefUser$line)
SIGN.test(entityRefUser$map, entityRefUser$mapPoint)

SIGN.test(entityRefUser$lineChart + entityRefUser$vertical + entityRefUser$line, entityRefUser$map + entityRefUser$mapPoint)