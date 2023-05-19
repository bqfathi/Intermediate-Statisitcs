#Packages
install.packages("rcompanion")
install.packages("IDPmisc")

library(rcompanion)
library(IDPmisc)


#Data
parks = read.csv('/Users/britfathi/Downloads/Seattle_ParksnRec.csv')
head(parks)
colnames(parks)

#Fall Trips
plotNormalHistogram(parks$X..of.trips.Fall)


