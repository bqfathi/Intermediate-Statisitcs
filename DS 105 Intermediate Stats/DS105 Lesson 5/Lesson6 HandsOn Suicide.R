# Load libraries
library("rcompanion")
library("car")
library("IDPmisc")
library("dplyr")

# Data
suicide <- read.csv("~/Downloads/suicide.csv")

#Question: Have suicide rates (suicides/100k pop) changed over the years (year)? And does the generation have any influence?

#Can't see any obvious needs for data wrangling as there are only 12 variables... and we will use most of them???

#Test for assumptions
#Normality
plotNormalHistogram(suicide$suicides.100k.pop)
## Skewed right
suicide$suicides.100k.popLOG <- log(suicide$suicides.100k.pop)
plotNormalHistogram(suicide$suicides.100k.popLOG)
## Now I notice the NA's
suicide1 <- NaRV.omit(suicide)
#Try again
suicide1$suicides.100k.popLOG <- log(suicide1$suicides.100k.pop)
plotNormalHistogram(suicide1$suicides.100k.popLOG)
# Pretty normal

#Homogeneity of Variance
leveneTest(suicides.100k.popLOG ~ generation, data=suicide1)
## p-value is <0.05 so it's significant and this assumption is VIOLATED

# Sample size = thousands


# Run the analysis
RManova <- aov(suicides.100k.popLOG~(generation*year)+Error(country/(year)), suicide1)
summary(RManova)

##** There is a significant difference between the country and the generation & generation:year
##** There is also a significant difference within between generation and year.

## Post Hocs
pairwise.t.test(suicide1$suicides.100k.popLOG, suicide1$generation, p.adjust="bonferroni")
##** Difference in suicide rates between all generations

suicideMeans <- suicide1 %>% group_by(generation, year) %>% summarize(Mean=mean(suicides.100k.pop))
##** Looking at the means, the group with the lwwest average suicide rate per 100k was Generation Z
##* and the G.I. Generation had the highest rates. 
