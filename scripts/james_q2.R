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
# Remove columns with an excessive amount of NAs
NA.per.column <- sapply(data.cropped, function(x) sum(is.na(x)))
data.reduced <- data.cropped[,which(NA.per.column < 10000)]

for (i in 4:ncol(data.reduced)) {
  if (colnames(data.reduced)[i] != "longitude" && colnames(data.reduced)[i] != "latitude" && colnames(data.reduced)[i] != "ncasualties"){
    data.reduced[,colnames(data.reduced)[i]] <- as.factor(data.reduced[,colnames(data.reduced)[i]])
  }
}


# First model attempt
model1 <- lm(ncasualties~ ., data.reduced)
# Error: cannot allocate vector of size 41.1 Gb

# Backwards selection will not work apparently
#foward.select <- regsubsets(ncasualties~., data=data.reduced, method="forward", nvmax=20)
#Error: cannot allocate vector of size 48.1 Gb

# Try a different approach 

# for (i in 4:ncol(data.reduced)-1) {
#   print(colnames(data.reduced)[i])
#   print(chisq.test(data.reduced[,i], data.reduced$ncasualties))
# }
