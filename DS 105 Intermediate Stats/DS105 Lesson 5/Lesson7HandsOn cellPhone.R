library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
library("dplyr")
library("psych")

# Data 
cellPhone <- read.csv("~/Downloads/cellPhone.csv")

#Clean the data
NaRV.omit(cellPhone)

#Test Assumptions
plotNormalHistogram(cellPhone$Night.Mins)
#The most perfect normal yet

leveneTest(Night.Mins~International.Plan, data=cellPhone)
#Results not significant, so assumption of homogeneity of variance is met. 

Homogeneity_RegrSlp = lm(Night.Mins~vMail.Plan, data=cellPhone)
anova(Homogeneity_RegrSlp)
#Results also not significant, so assumption of homogeneity of regression slope is met

#Sample size is met even after cleaning

#Analysis
ANCOVA = lm(Night.Mins~vMail.Plan + International.Plan*vMail.Plan, data=cellPhone)
anova(ANCOVA)
## It seems they all lack significant influence over each other. Whether a person has an international
## plan or not doesn't influence the number of night minutes he/she uses, nor does having a voice mail plan. 