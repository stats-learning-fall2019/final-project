# Add relevent libraries
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(corrplot)

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

data.reduced <- data.frame(data.reduced,success=ifelse(data.reduced$success==0,"No","Yes"))
tree.model1=tree(success.1~.-success,data.reduced)
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
