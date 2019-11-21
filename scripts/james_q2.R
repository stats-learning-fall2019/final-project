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

data.cropped <- data.cropped[,sapply(sapply(data.cropped, table), length) > 2]
#length(table(data.cropped$ncasualties))
model1 <- lm(ncasualties~ ., data.cropped)
