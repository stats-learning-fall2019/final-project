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

# Load Terrorism Dataset
data <- read.csv('../data/gtdb_cleansed.csv')

### General Data Exploration
colnames(data)

data.numeric <- data[,sapply(data, is.numeric)]
corr <- cor(data.numeric)
corrplot(corr, method = "square")

# No super noteable results. "weapontype1" is correlated with "attacktype1" so they probably measure similar things.

# Create a simple tree model to find which variables seem the most important.

#tree.model1=tree(success~.,data)
# Causes error as there are to many empty values.

# Remove columns with an excessive amount of NAs
NA.per.column <- sapply(data.numeric, function(x) sum(is.na(x)))
data.reduced <- data.numeric[,which(NA.per.column < 10000)]

data.reduced <- data.frame(data.reduced,success.text=ifelse(data.reduced$success==0,"No","Yes"))
tree.model1=tree(success.text~.-success,data.reduced)
summary(tree.model1)

# Used variables "property", "targtype1", "ishostkid", "weaptype1", attacktype1", "N"

plot(tree.model1)
text(tree.model1,pretty=0)
tree.model1

# We have several issues. 
# 1. Variables such as "property" and "ishostkid" are supposed to be categorical 0, 1, but they also have -9 representing unknown
# 2. "N" is the index, it shouldn't be relevant.
# 3. "targetype1" and "weaptype1" are categorical, the numbers represent different categories. They should be one hot encoded.
# 4. Using variables such as "property" and "ishostkid" seems a bit cheap and unhelpful; of course if there's hostages or property 
#    damage the attack was more likely successful. 


# Solve Problem 1
data.cleaned <- data.reduced
values.per.column <- sapply(data.cleaned, function(x) length(unique(x)))

# > unique(data.cleaned[,"vicinity"])
# [1]  0  1 -9
# > unique(data.cleaned[,"doubtterr"])
# [1]  0 -9  1 NA
# > unique(data.cleaned[,"multiple"])
# [1]  0  1 NA
# > unique(data.cleaned[,"guncertain1"])
# [1]  0  1 NA
# > unique(data.cleaned[,"ishostkid"])
# [1]  0  1 NA -9
# > unique(data.cleaned[,"INT_LOG"])
# [1]  0 -9  1
# > unique(data.cleaned[,"INT_IDEO"])
# [1]  0  1 -9
# > unique(data.cleaned[,"INT_MISC"])
# [1]  0  1 -9
# > unique(data.cleaned[,"INT_ANY"])

to.fix <- c("vicinity", "doubtterr", "multiple", "ishostkid", "property", "INT_LOG", "INT_IDEO", "INT_MISC", "INT_ANY")
data.cleaned[,to.fix][data.cleaned[,to.fix] == -9] <- NA


# Solve problem 2
data.cleaned$N <- NULL
data.cleaned$eventid <- NULL

# Solve problem 3
data.cleaned$success.text <- NULL # To prevent us from encoding it.
# "country"  and "natlty1" not included as over 100 columns for one factor seems a bit excessive
to.hot.encode <- c("region", "specificity", "vicinity", "attacktype1", "targtype1", "weaptype1")
for (column in to.hot.encode) {
  data.cleaned[,column] <- as.factor(data.cleaned[,column])
}
dmy <- dummyVars(~., data=data.cleaned)
data.encoded <- data.frame(predict(dmy, newdata = data.cleaned))
colnames(data.encoded)

data.encoded[,"success"] <- as.factor(data.encoded[,"success"])


# Second Attempt at a model
tree.model2 <- tree(success~.,data.encoded)
summary(tree.model2)

plot(tree.model2)
text(tree.model2,pretty=0)

# Used variables "property"      "ishostkid"     "weaptype1.5"   "targtype1.20"  "attacktype1.1" "iyear" 

# Looking at the results, it seems that that "Problem 4" from the last model is still a major issue here.
# Also "targtype1.20" is a useless variable since it corresponds to an unknown target type.
data.encoded$targtype1.20 <- NULL

tree.model3 <- tree(success~.,data.encoded)
summary(tree.model3)


plot(tree.model3)
text(tree.model3,pretty=0)

# This is now predicting everything to be a success. The problem seems to be that the data skewed to much that way.

table(data.encoded$success)[1] / nrow(data.encoded) # precentage of failed attacks

# One idea is to just predict the probability of success instead of just 1 or 0
tree.model3
# 1) root 74694 47560.0 1 ( 0.096956 0.903044 )  
# 2) property < 0.5 28810 28150.0 1 ( 0.191635 0.808365 )  
# 4) ishostkid < 0.5 23194 25330.0 1 ( 0.235621 0.764379 )  
# 8) weaptype1.5 < 0.5 11429 14700.0 1 ( 0.343162 0.656838 ) *
#   9) weaptype1.5 > 0.5 11765  9143.0 1 ( 0.131152 0.868848 ) *
#   5) ishostkid > 0.5 5616   627.5 1 ( 0.009972 0.990028 ) *
#   3) property > 0.5 45884 14680.0 1 ( 0.037508 0.962492 )  
# 6) attacktype1.1 < 0.5 44802 11700.0 1 ( 0.028816 0.971184 )  
# 12) iyear < 2004.5 28820 10150.0 1 ( 0.042575 0.957425 ) *
#   13) iyear > 2004.5 15982   834.3 1 ( 0.004005 0.995995 ) *
#   7) attacktype1.1 > 0.5 1082  1454.0 1 ( 0.397412 0.602588 ) *
tree.model4 <- rpart(success~., data.encoded)
tree.model4
fancyRpartPlot(tree.model4)
summary(tree.model4)
# Variable importance
# attacktype1.3   weaptype1.6      property   weaptype1.5   targtype1.3  targtype1.14   targtype1.4         crit3 attacktype1.1     doubtterr 
# 18            18            18            10             6             5             4             4             3             3 
# iyear attacktype1.6       suicide  targtype1.17 
# 3             2             2             2 

## Testing models 

# Testing on training data

# Predictions for Tree model 4 -- better model 
pred <- rep("0", nrow(data.encoded))
pred[predict(tree.model4)[,1] <.5] <- "1"
actual <- as.factor(data.encoded$success)
table(actual, pred)

recall <- 6758 / (6758 + 13301) # 0.3369061
precision <- 6758 / (6758 + 3101) # 0.6854651

# Predictions for Tree model 3 -- model that doesn't work well
pred <- rep(0, nrow(data.encoded))
pred[predict(tree.model3)[,1] <.5] <- 1
actual <- data.encoded$success
table(actual, pred)

# Testing w/ testing data

# Test model 4 on training data
set.seed(42)
sample <- sample.split(data.encoded, SplitRatio=.80)
train <- subset(data.encoded, sample==TRUE)
test <- subset(data.encoded, sample==FALSE)

tree.model4.train <- rpart(success~., train)
pred <- rep(0, nrow(test))
pred[predict(tree.model4.train, test)[,1] <.5] <- 1
actual <- test$success
results <- table(actual, pred)

recall <- results[1] / (results[1] + results[1,2]) #1363 / (1363 + 2647) # 0.3399002
recall
precision <- results[1] / (results[1] + results[2])#1363 / (1363 + 642) # 0.6798005
precision

# Adjusting the threshold has very little impact on precision recall
pred <- rep(0, nrow(test))
pred[predict(tree.model4.train, test)[,1] <.3] <- 1
actual <- test$success
results <- table(actual, pred)

recall <- results[1] / (results[1] + results[1,2])
recall # 0.3581047
precision <- results[1] / (results[1] + results[2]) 
precision # 0.6459739

# Plot ROC curve
roc11 <- actual
roc12 <- pred
plot(roc(roc11, roc12, direction='<'), col='blue', lwd=3, main='ROC Curve')

# This model (tree.model4) seems a lot better, recall is a bit low though (if you consider 0s to be positive events).
# Model4 also generalizes a fairly well; the precision and recall scores remain about the same when using test/train data.

# Since we have a fairly large class imbalancy probelm. One possibility we could look into might be undersampling/oversampling.
# I.e. selecting less/more examples from a class to balance out the dataset

data.balanced <- ovun.sample(success~.,data=data.encoded, method="over")
table(data.balanced$data$success) # "Perfectly balanced, as all things should be"

# Create model 5
tree.model5 <- rpart(success~., data.balanced$data)
tree.model5
fancyRpartPlot(tree.model5)
summary(tree.model5)

# Check model 5
pred <- rep(0, nrow(data.balanced$data))
pred[predict(tree.model5)[,1] <.5] <- 1
actual <- data.balanced$data$success
results <- table(actual, pred)

recall <- results[1] / (results[1] + results[1,2])
recall
precision <- results[1] / (results[1] + results[2])
precision
# Vastly better results on training data

# Try it with a test train split
set.seed(42)
sample <- sample.split(data.encoded, SplitRatio=.80)
train <- subset(data.encoded, sample==TRUE)
test <- subset(data.encoded, sample==FALSE)

train.balanced <- ovun.sample(success~., data=train, method="over")
table(train.balanced$data$success)

tree.model5.train <- rpart(success~., train.balanced$data)

pred <- rep(1, nrow(test))
pred[predict(tree.model5.train, test)[,1] <.5] <- 0
actual <- test$success
results <- table(actual, pred)
results

recall <- results[1] / (results[1] + results[1,2])
recall # 0.8640898
precision <- results[1] / (results[1] + results[2])
precision # 0.2130341
# This model seems to have the opposite problem that model 4 had. It seems to to be a bit too aggressive in 
# predicting 0s.

# Again changing the threshold seems to have little effect.
roc21 <- actual
roc22 <- pred

# Plot ROC curves
plot(roc(roc11, roc12, direction='<'), col='blue', lwd=3, main='ROC Curves')
lines(roc(roc21, roc22, direction='<'), col='red', lwd=3)
# seems like the new model might have a little bit more area under the curve. Neither look that good.



## Let's try a completely different approach. Logistic Regression
logistic.model1 <- glm(success~., family="binomial",data.encoded)
summary(logistic.model1)

pred <- rep(0, nrow(data.encoded))
pred[predict(logistic.model1, type="response") >.5] <- 1
actual <- data.encoded$success
results <- table(actual, pred)
results

recall <- results[1] / (results[1] + results[1,2])
recall # 0.02058926
precision <- results[1] / (results[1] + results[2])
precision # 0.1019753

roc31 <- actual
roc32 <- pred
# Plot ROC curves
plot(roc(roc11, roc12, direction='<'), col='blue', lwd=3, main='ROC Curves')
lines(roc(roc21, roc22, direction='<'), col='red', lwd=3)
lines(roc(roc31, roc32, direction='<'), col='green', lwd=3)
# New model doesn't work at all

# Use backwards selection to find a better model

backselect <- regsubsets(success~., method='backward', data=data.encoded, nvmax=30)
plot(backselect)
summary(backselect)
selected <- c("property", "ishostkid", "weaptype1.6", "targtype1.17", "targtype1.15", "targtype1.14", "targtype1.4", "targtype1.3","targtype1.1",
              "attacktype1.3", "attacktype1.2", "success")
data.selected <- data.encoded[,selected]
logistic.model2 <- glm(success~., family = "binomial", data=data.selected)
summary(logistic.model2)

pred <- rep(0, nrow(data.encoded))
pred[predict(logistic.model2, type="response") >.5] <- 1
actual <- data.encoded$success
results <- table(actual, pred)
results

recall <- results[1] / (results[1] + results[1,2])
recall # 0.05748043
precision <- results[1] / (results[1] + results[2])
precision # 0.1206319
roc41 <- actual
roc42 <- pred

# Plot ROC curves
plot(roc(roc11, roc12, direction='<'), col='blue', lwd=1, main='ROC Curves')
lines(roc(roc21, roc22, direction='<'), col='red', lwd=1)
lines(roc(roc31, roc32, direction='<'), col='green', lwd=1)
lines(roc(roc41, roc42, direction='<'), col='yellow', lwd=1)
