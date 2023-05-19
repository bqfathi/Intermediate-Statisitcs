# Packages
install.packages("rcompanion")
install.packages("IDPmisc")

library(rcompanion)
library(IDPmisc)

#Load Data
ship <- read_excel("~/Downloads/cruise_ship 3.xlsx")
head(ship)

colnames(ship)

#Exploring Skewness

plotNormalHistogram(ship$YearBlt)
skewness(ship$YearBlt)
#Skewed left (negatively) -1.222841

plotNormalHistogram(ship$Tonnage)
skewness(ship$Tonnage)
#Skewed right (positively) 0.4935251

plotNormalHistogram(ship$passngrs)
skewness(ship$passngrs)
#Skewed right (positively) 0.4052865

plotNormalHistogram(ship$Length)
skewness(ship$Length)
#Skewed left (negatively) -.7777847

plotNormalHistogram(ship$Cabins)
skewness(ship$Cabins)
#Skewed right (positively) 0.2677185

plotNormalHistogram(ship$Crew)
skewness(ship$Crew)
#Skewed right (positively) 0.192933

plotNormalHistogram(ship$PassSpcR)
skewness(ship$PassSpcR)
#Skewed right (positively) 0.9320179

plotNormalHistogram(ship$outcab)
skewness(ship$outcab)
#Skewed right (positively) 0.1391828

#Transforming Positively Skewed Variables
ship$TonnageSQRT <- sqrt(ship$Tonnage)
ship$passngrsSQRT <- sqrt(ship$passngrs)
ship$CabinsSQRT <- sqrt(ship$Cabins)
ship$CrewSQRT <- sqrt(ship$Crew)
ship$PassSpcRSQRT <- sqrt(ship$PassSpcR)
ship$outcabSQRT <- sqrt(ship$outcab)

#Checking 
plotNormalHistogram(ship$Tonnage)
plotNormalHistogram(ship$passngrs)
plotNormalHistogram(ship$Cabins)
plotNormalHistogram(ship$Crew)
plotNormalHistogram(ship$PassSpcR)
plotNormalHistogram(ship$outcab)

#Transforming negatively Skewed Variables
ship$YearBltSQ <- ship$YearBlt * ship$YearBlt
ship$LengthSQ <- ship$Length * ship$Length

#Checking
plotNormalHistogram(ship$YearBltSQ)
plotNormalHistogram(ship$LengthSQ)

#YearBlt still needs help
ship$YearBltCUBE <- ship$YearBlt^3

#Checking again
plotNormalHistogram(ship$YearBltCUBE)

install.packages("moments")
library(moments)

