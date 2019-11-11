# Add relevent libraries
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(corrplot)
library(caret)

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
