# Libraries

library("rcompanion")
library("fastR2")
library("car")

# Data
honey <- read.csv("~/Downloads/honey.csv")
# year is in its own column. What's the variety?? 2008-2012

colnames(honey)
keeps <- c("state", "totalprod", "year")
honeyT <- honey[keeps]

honey2008 <- honeyT[1:41,]
honey2009 <- honeyT[42:81,]
honey2010 <- honeyT[82:121,]
honey2011 <- honeyT[122:161,]
honey2012 <- honeyT[162:201,]

honey2008$repdat <- honey2008$totalprod
honey2008$contrasts <- "T1"

honey2009$repdat <- honey2009$totalprod
honey2009$contrasts <- "T2"

honey2010$repdat <- honey2010$totalprod
honey2010$contrasts <- "T3"

honey2011$repdat <- honey2011$totalprod
honey2011$contrasts <- "T4"

honey2012$repdat <- honey2012$totalprod
honey2012$contrasts <- "T5"

honeyALL <- rbind(honey2008, honey2009, honey2010, honey2011, honey2012)

## Here I thought I was doing clever stuff. 

#Data wrangling
honey$year <- as.character(honey$year)
honey$year <- as.factor(honey$year)

plotNormalHistogram(honey$totalprod)
#skewed right(positively)

honey$totalprodLOG <- log(honey$totalprod)
plotNormalHistogram(honey$totalprodLOG)
#Nicely normal = PASSED

leveneTest(totalprodLOG ~ year, data = honey)
#not significant with a p-value of .998 so PASSED Homogeneity of Variance

#Run the analysis
RManova <- aov(totalprodLOG ~ year + Error(state), honey)
summary(RManova)

## According to what I can see there IS a difference between them from within from 2008 to 2012,
### with the p-value being < 0.05 .... so msoderately significant. 

