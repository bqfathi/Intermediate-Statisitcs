##*** How does gender influence someo f the heart attack predectors like resting blood pressure and cholosterol?

# Load Packages

install.packages("mvnormtest")

library("mvnormtest")
library("car")
library("ggplot2")

# Load Data

heartAttacks <- read.csv("~/Downloads/heartAttacks.csv")

head(heartAttacks)

sapply(heartAttacks, function(x)sum(is.na(x)))
# Data cleaning

## Removing columns after chol 
heartAttacks1 = subset(heartAttacks, select = -c(fbs, restecg, thalach, exang, oldpeak, slope, ca, thal, target))
head(heartAttacks1)

# Data wrangling

str(heartAttacks1$trestbps)
str(heartAttacks1$chol)
## values are numeric

# Subset the data
keepers <- c("trestbps", "chol")
heartAttacks2 <-heartAttacks1[keepers]
## The two columns we need
head(heartAttacks2)
## check for NA again.. 
sapply(heartAttacks2, function(x)sum(is.na(x)))

# Change to numeric... 
heartAttacks2$trestbps <- as.numeric(heartAttacks2$trestbps)
heartAttacks2$chol <- as.numeric(heartAttacks2$chol)

# Format as a Matrix
heartAttacks3 <- as.matrix(heartAttacks2)

# Test assumptions
# Sample size is more than 20 cases per IV. 
# Multivariate Normality
mshapiro.test(t(heartAttacks3))
## Violated as the p value is significant at 3.93 < 0.05. Does not meet the assumption for the MANOVA, but we preoceed 

# Homogeneity of Variance
leveneTest(heartAttacks$trestbps, heartAttacks$chol, data=heartAttacks)
## Meets the homogeneity of variance test as the p value is not significant. 

# Abscence of Multicollinearity
cor.test(heartAttacks$trestbps, heartAttacks$chol, method="pearson", use="complete.obs")
## The correlation coefficient is <.7, so no strong multicollinearity

# MANOVA
MANOVA <- manova(cbind(trestbps, chol) ~ sex, data = heartAttacks)
summary(MANOVA)
summary.aov(MANOVA, test = "wilks")

## There is a significant difference in cholesterol in relation to sex (with a significant p-value)

aggregate(chol ~ sex, heartAttacks, mean)
aggregate(chol ~ sex, heartAttacks, sum)

ggplot(heartAttacks, aes(x=sex, y = chol)) + geom_bar(stat = "identity")

## It appears that the sex represented with 0 has a higher average cholesterol rate