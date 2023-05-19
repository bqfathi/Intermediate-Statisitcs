# Libraries

install.package("fastR2")

library("rcompanion")
library("fastR2")
library("car")

# Data
breakfast <- read.csv("~/Downloads/breakfast.csv")
# (I looked at the table. Which columns are considered the weight? Let's look at the columns. 

colnames(breakfast)
# Maybe body mass, lean tissue mass, bmi, body fat is what the weight is considered? Going with body mass. 

#Wrangling  
breakfast1 <- breakfast[1:33,]
#Data only found in rows 1-33

#Keeping only important columns (maybe they are?)
keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Body.Mass..kg.", "Follow.Up.Body.Mass..kg.")
breakfast2 <- breakfast1[keeps]

#Keeping the info on the particpants
breakfast3 <- breakfast2[,1:5]

#Filtering the data for the two that will be compared. 
breakfast3$repdat <- breakfast2$Baseline.Body.Mass..kg
# contrasts column to hold th info from the baseline timepoint
breakfast3$contrasts <- "T1"

#Same thing with the follow up: 
breakfast4 <- breakfast2[,1:5]
breakfast4$repdat <- breakfast2$Follow.Up.Body.Mass..kg
breakfast4$contrasts <- "T2"

#Combining them back together
breakfast5 <- rbind(breakfast3, breakfast4)

#Now testing for the ANOVA assumptions
## Normality
plotNormalHistogram(breakfast2$Baseline.Body.Mass..kg.)
### Nicely normal
plotNormalHistogram(breakfast2$Follow.Up.Body.Mass..kg.)
### Also normal

## Homogeneity of Variance
leveneTest(repdat ~ Treatment.Group*contrasts, data=breakfast5)
### Gives a p-value of 0.9795 which is non-significant

## Sample size: 33, so it passes

## Sphericity, not doing now. 

RManova <- aov(repdat ~ contrasts + Error(Participant.Code), breakfast5)
summary(RManova)

#With the f-value and p-value non, significant, there was no effect of time on body mass. 


