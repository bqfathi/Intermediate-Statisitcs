library("rcompanion")
library("fastR2")
library("car")

# Data
breakfastM <- read.csv("~/Downloads/breakfast.csv")

breakfastM1 <- breakfastM[1:33,1:7]

keeps = c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")

breakfastM2 <- breakfastM1[keeps]
### An error keeps on happening here*****

breakfastM3 <- breakfastM1[,1:5]
breakfastM3$repdat <- breakfastM1$Baseline.Resting.Metabolic.Rate..kcal.d
breakfastM3$contrasts <- "T1"

breakfastM4 <- breakfastM1[,1:5]
breakfastM4$repdat <- breakfastM1$Follow.Up.Resting.Metabolic.Rate..kcal.d
breakfastM4$contrasts <- "T2"

breakfastM5 <- rbind(breakfastM3, breakfastM4)

# Test assumptions
## Normality
plotNormalHistogram(breakfastM1$Baseline.Body.Mass..kg.)
plotNormalHistogram(breakfastM1$Follow.Up.Body.Mass..kg.)
###Both normal

## Homogeneity of Variance*** Error gets in the way of this happening?

leveneTest(repdat ~ Treatment.Group*contrasts, data=breakfastM5)