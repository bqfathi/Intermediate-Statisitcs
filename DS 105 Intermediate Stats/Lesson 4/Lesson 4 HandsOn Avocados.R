# Load Libraries

library('rcompanion')
library('car')
library('tidyverse')


# Import data
avocados <- read.csv("~/Downloads/avocados.csv")

# Filter the Data and Remove Missing Values

sales <- na.omit(avocados %>% filter(region %in% c("Albany", "Huston", "Seattle")))
### leaves us with 676 observations of 14 variables... 

sales$AveragePrice = as.numeric(sales$AveragePrice)

# Testing assumptions
## Normality

plotNormalHistogram(sales$AveragePrice)
## Looks slightly skewed right. Square root?

sales$AveragePriceSQRT = sqrt(sales$AveragePrice)

plotNormalHistogram(sales$AveragePriceSQRT)
## Looks nice enough. 

# Homogeneity of Variance
bartlett.test(AveragePriceSQRT ~ region, data= sales)
## p-value < 2.2e-16, so it's significant and fails to meet the assumption of homogeneity.

fligner.test(AveragePriceSQRT ~ region, data= sales)
## Also fails. p-value 3.411 e-11


# ANOVA
ANOVA = lm(AveragePriceSQRT ~ region, data = sales)
Anova(ANOVA, type = "II", white.adjust = TRUE)
## With a Pr(>F) of 2.69 e-06, this is a difference in the price of avocados by regions. What is the difference?

#Post Hoc
pairwise.t.test(sales$AveragePriceSQRT, sales$region, p.adjust = "bonferroni", pool.sd = FALSE)




