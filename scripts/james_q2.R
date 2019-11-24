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
library(e1071)

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
# 
# model1 <- lm(ncasualties~., data.subset)
# summary(model1)
# sink("model1_1.txt")
# print(summary(model1))
# sink()

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
#plot(model3)

# Remove outliers. I don't think that we will be able to accuratly predict them (there are only 12 points out of over 100,000 greater than 1000)
data.reduced.2 <- subset(data.reduced, data.reduced$ncasualties < 1000)
data.reduced.2 <- subset(data.reduced.2, data.reduced.2$ncasualties != 0)
model4 <- lm(log(ncasualties)~ iyear + region + doubtterr + suicide + attacktype1 + weaptype1, data.reduced.2)
summary(model4)

#plot(model4)

# Add 0s back in
data.reduced.2 <- subset(data.reduced, data.reduced$ncasualties < 1000)

# Try a non-linear model
model5 <- rpart(ncasualties~ iyear + region + doubtterr + attacktype1 + weaptype1, data.reduced.2)
summary(model5)
fancyRpartPlot(model5)

 model6 <- rpart(ncasualties~., data.reduced.2)
summary(model6) 
fancyRpartPlot(model6)
rsq.rpart(model6)

data.subset <- sample_frac(data.reduced.2, .2)
# Try SVMs 
model7 <- svm(ncasualties~., data.subset, kernel="radial")
summary(model7)

tune.model7 <- tune(svm,ncasualties~.,data=data.subset, kernal="radial",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.model7)

# - Detailed performance results:
#   cost    error dispersion
# 1 1e-03 548.8293   211.8781
# 2 1e-02 537.8306   210.8174
# 3 1e-01 520.8024   209.0696
# 4 1e+00 505.5878   207.4679
# 5 5e+00 498.3797   206.8093
# 6 1e+01 494.5318   206.1339
# 7 1e+02 480.6113   205.4937

summary(tune.model7$best.model)

rsq <- function (x, y) cor(x, y) ^ 2

rsq(na.omit(data.subset)$ncasualties, predict(tune.model7$best.model))
rsq(na.omit(data.subset)$ncasualties, predict(model7))
    
model8 <- svm(ncasualties~., data.subset, kernel="radial", cost=1000)
summary(model8)

model9 <- svm(ncasualties~., data.subset, kernel="radial", cost=10)

rsq(na.omit(data.subset)$ncasualties, predict(model8))

# Try on test data
set.seed(123)
data.subset.2 <- sample_frac(data.reduced.2, .2)

rsq(na.omit(data.subset.2)$ncasualties, predict(model8, data.subset.2)) # 0.1308047
rsq(na.omit(data.subset.2)$ncasualties, predict(tune.model7$best.model,data.subset.2)) # 0.1612188
rsq(na.omit(data.subset.2)$ncasualties, predict(model9,data.subset.2)) # 0.1539635
rsq(na.omit(data.subset.2)$ncasualties, predict(model7,data.subset.2)) # 0.1338144


# Try random forest
rf.data <- data.subset
rf.data$targsubtype1 <- NULL

model10 <- randomForest(na.omit(rf.data)[,1:26], na.omit(rf.data)$ncasualties)
plot(model10)
summary(model10)

varImpPlot(model10)

rsq(na.omit(data.subset.2)$ncasualties, predict(model10,na.omit(data.subset.2))) # 0.2327896
