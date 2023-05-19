library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
library("dplyr")
library("psych")

# Data
graduate_admissions <- read.csv("~/Downloads/graduate_admissions.csv")
#For good measure
NaRV.omit(graduate_admissions)

str(graduate_admissions)
##I believe all relevant columns are integer. 

#Check Assumptions: Normality of GPA
plotNormalHistogram(graduate_admissions$CGPA)
# quite normal, but a little left skewed. Squaring it?

graduate_admissions$CGPAsq <- graduate_admissions$CGPA * graduate_admissions$CGPA
plotNormalHistogram(graduate_admissions$CGPAsq)
# Looks better. 

# Normality of TOEFL
plotNormalHistogram(graduate_admissions$TOEFL.Score)
# Also quite normal but a little left? 
graduate_admissions$TOEFL.Scoresq <- graduate_admissions$TOEFL.Score *graduate_admissions$TOEFL.Score
plotNormalHistogram(graduate_admissions$TOEFL.Scoresq)
# Not hugely different, but I'll accept it. 

graduate_admissions$University.Rating <- as.factor(graduate_admissions$University.Rating)
#Check Assumptions: Homogeneity of Variance of GPA
leveneTest(CGPAsq ~ University.Rating, data = graduate_admissions)
## Gives me an error about not appropriate for explanatory variables. Ran the as.factor code above. 
##* Levene's p-value was not significant, so the assumption is met

#Homogeneity of Regression Slopes
Homogeneity_RegrSlp = lm(CGPA~TOEFL.Score, data=graduate_admissions)
anova(Homogeneity_RegrSlp)
## Violaiton of the assumption since p = 2.2e-16

#Sample size is more than met

ANCOVA <- lm(CGPA~TOEFL.Score + University.Rating*TOEFL.Score, data=graduate_admissions)
anova(ANCOVA)
##*Insignificant difference between TOEFL score and University rating, and CGPA and TOEFL.Score, 
##*but significant difference between University rating and TOEFL by itself. 

# Post Hocs

postHocs <- glht(ANCOVA, linfct = mcp(Univeristy.Rating = "Tukey"))

adjMeans <- effect("University.Rating", ANCOVA)
adjMeans

##* All University ratings have a GPA that is about the same. 
