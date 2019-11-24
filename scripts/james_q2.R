# Add relevent libraries
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(corrplot)
library(caret)
library(rpart)
library(rattle)	
library(pROC)
library(caTools)
library(leaps)
library(ROSE)
library(dplyr)

# Load Terrorism Dataset
data <- read.csv('../data/gtdb_cleansed.csv')

# Plot hist of fatalities
plot(hist(na.omit(data$nkill)))

# Crop off higher values so you can see what is going on
crop <- data$nkill[data$nkill < 20]

plot(hist(na.omit(crop)))

# Wounds data seems fairly similar
plot(hist(na.omit(data$nwound[data$nwound < 20])))

# Create casualties by adding the two columns together
data[, "ncasualties"] <- data$nkill + data$nwound

plot(hist(na.omit(data$ncasualties[data$ncasualties < 20])))

# Select only the rows that have recorded data
data.cropped <- subset(data, !is.na(ncasualties))

# remove columns so linear regression will work
data.cropped <- data.cropped[,sapply(sapply(data.cropped, table), length) > 1]
data.cropped$nkill <- NULL
data.cropped$nwound <- NULL
data.cropped$N <- NULL
data.cropped$eventid <- NULL
data.cropped$country <- NULL
data.cropped$natlty1 <- NULL
# Remove columns with an excessive amount of NAs
NA.per.column <- sapply(data.cropped, function(x) sum(is.na(x)))
data.reduced <- data.cropped[,which(NA.per.column < 10000)]

for (i in 4:ncol(data.reduced)) {
  if (colnames(data.reduced)[i] != "longitude" && colnames(data.reduced)[i] != "latitude" && colnames(data.reduced)[i] != "ncasualties"){
    data.reduced[,colnames(data.reduced)[i]] <- as.factor(data.reduced[,colnames(data.reduced)[i]])
  }
}


# First model attempt
#model1 <- lm(ncasualties~ ., data.reduced)
# Error: cannot allocate vector of size 41.1 Gb

# Lets use a subset of the data
set.seed(42)
data.subset <- sample_frac(data.reduced, .2)

model1 <- lm(ncasualties~., data.subset)
summary(model1)
sink("model1_1.txt")
print(summary(model1))
sink()

data.reduced <- subset(data.reduced, data.reduced$success == 1)
data.reduced$provstate <- NULL
data.reduced$city <- NULL
data.reduced$gname <- NULL
data.reduced$success <- NULL
rownames(data.reduced) <- NULL
# This model doesn't take 50,000 years to run
model2 <- lm(ncasualties~., data.reduced)
summary(model2)
# The R-squared value is miserably low

# Quick backwards selection
model3 <- lm(ncasualties~ iyear + region + doubtterr + suicide + attacktype1 + targsubtype1 + weaptype1 + property, data.reduced)
summary(model3)
plot(model3)

# Remove outliers. I don't think that we will be able to accuratly predict them (there are only 12 points out of over 100,000 greater than 1000)
data.reduced.2 <- subset(data.reduced, data.reduced$ncasualties < 1000)
model4 <- lm(ncasualties~ iyear + region + doubtterr + suicide + attacktype1 + targsubtype1 + weaptype1 + property, data.reduced.2)
summary(model4)
plot(model4)
