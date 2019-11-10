# Add relevent libraries
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(corrplot)

# Load Terrorism Dataset
data <- read.csv('../data/gtdb_cleansed.csv')

colnames(data)
