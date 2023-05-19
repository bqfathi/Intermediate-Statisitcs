# Load Libraries

library('gmodels')
library('dplyr')
library('tidyr')

# Part 1: Does the term of the loan influence loan status? If so, how? Running an independent Chi-Squared

CrossTable(loans$loan_status, loans$term, chisq=TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

# I like this better. I can compare the terms easier. 
# The p-value and expected values all are within the requirements to proceed. 
# All standardized residuals are greater than abs(2) meaning they are all significant. 
# For charged off, the 36 month term is greatly below the mean, while the 60 month is greatly above the mean
# For Current, 36 month is greatly below the mean, while 60 month is greatly above the mean
# For Fully Paid, 36 month is greatly above the mean, while 60 month is greatly below the mean. 

# My interpretation of this is that more borrowers are current with a 60 month term than the 36. 
# More borrowers have been charged off for 60 month than the 36 month terms. 
# More borrowers have paid their loans off for the 36 month than the 60 month term. 

#Part 2: How has the ability to own a home changed after 2009. McNemar 
# Start with wrangling
loans$DateR <- as.Date(paste(loans$Date), "%m/%d/%Y")

loans1 <- separate(loans, DateR, c("Year", "Month", "Day"), sep="-")

loans1$YearR <- NA
loans1$YearR[loans1$Year <= 2009] <- 0
loans1$YearR[loans1$Year > 2009] <- 1

loans1$RentvOwn <- NA
loans1$RentvOwn[loans1$home_ownership == "RENT"] <- 0
loans1$RentvOwn[loans1$home_ownership == "OWN"] <- 1

CrossTable(loans1$RentvOwn, loans1$YearR, chisq=TRUE, 
           mcnemar=TRUE, fisher = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

## With the p-value for McNemar's Chi-squared test, the p value is less than .05, so the test is significant. 
## According to the row and column percents, the percent renting to owning remained roughly the same
## whether you look before or after 2009 with about 14% owning and about 86% renting. 

#Part 3: Based on the news story, does it seem likely that the data for this hands on came from the 
## larger population of America?  (will be doing a Goodness of Fit Chi Squared )

loans %>% group_by(loan_status) %>% summarize(count=n())

observed = c(3382, 502, 18173)
expected = c(0.1, 0.75, 0.15)

chisq.test(x = observed, p = expected)

## The p-value shows that there is significant difference between our sample and the news story population. 

