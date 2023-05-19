# Load Libraries

library('rcompanion')
library('car')
library('tidyverse')


# Import data
data <- read.csv("~/Downloads/avocados.csv")

# Filter the Data and Remove Missing Values

avocados <- na.omit(data %>% filter(region %in% c("Albany", "Houston", "Seattle")))
### leaves us with 676 observations of 14 variables... 

avocados$AveragePrice = as.numeric(avocados$AveragePrice)

# Testing assumptions
## Normality

plotNormalHistogram(avocados$AveragePrice)
## Looks slightly skewed right. Square root?

avocados$AveragePriceSQRT = sqrt(avocados$AveragePrice)

plotNormalHistogram(avocados$AveragePriceSQRT)
## Looks nice enough. 

# Homogeneity of Variance
bartlett.test(AveragePriceSQRT ~ region, data= avocados)
## p-value < 4.333e-16, so it's significant and fails to meet the assumption of homogeneity.

fligner.test(AveragePriceSQRT ~ region, data= avocados)
## Also fails. p-value 3.522 e-11


# ANOVA
ANOVA = lm(AveragePriceSQRT ~ region, data = avocados)
Anova(ANOVA, type = "II", white.adjust = TRUE)
## With a Pr(>F) of 2.2 e-16, this is a difference in the price of avocados by regions. What is the difference?

#Post Hoc
pairwise.t.test(avocados$AveragePriceSQRT, avocados$region, p.adjust = "bonferroni", pool.sd = FALSE)
# The difference between average price of avocados in Seatle and Albany, Seattle and Houston, and Albany and Houston is significant. 

salesMeans = avocados %>% group_by(region) %>% summarize(Mean = mean(AveragePrice))
salesMeans

## The Average Price Varies amongst all 3 regions of Albany, Houston and Seattle. 
# Albany has an average price of $1.56
# Houston has an average price of $1.05
# Seattle has an average price of $1.44

# So for this person to leave their parents basement and afford live on their own, 
## with Avocado toast, Houston looks most promising. 




