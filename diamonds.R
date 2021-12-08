# Clear All Variables & Clear Screen
rm(list=ls())
cat("\014")

# Install packages 
library(caTools)
library(rpart)
library(rpart.plot)
library(tictoc)
library(randomForest)

# Setting the same random seed
set.seed(1234)

# Load the dataset 
Gem = read.csv("diamonds.csv")
str(Gem)

# Splitting the dataset into training and testing
split <- sample.split(Gem$price, SplitRatio = 0.7)
head (split)
Train  = subset(Gem, split==TRUE)
Test = subset(Gem, split==FALSE)

# Build a classification tree CART model on the training data
tic()
Gem.CT = rpart(price ~ carat + cut + color + clarity + depth + table, data = Train, method = "class", minbucket = )
Gem.CT = rpart()
toc()

# Plot the tree
prp(Gem.CT)

# Make prediction on the test set
Gem.CT.predTest = predict(Gem.CT, newdata = Test, type = "class")

# Create the confusion matrix
CT.predTable <- table(Test$price, Gem.CT.predTest)
CT.predTable

sum(diag(CT.predTable))/sum(CT.predTable)

# Random Forest
# The following commands convert the data type of the 0's and 1's from integer to categorical
# If this step is skipped, the randomForest command will use regression trees by default, not classification trees
Train$price = as.factor(Train$price)
Test$price = as.factor(Test$price)

GemForest = randomForest(price ~ carat + cut + color + clarity + depth + table, data = Train)
GemForest = randomForest(price ~ carat + cut + color + clarity + depth + table, data = Train, ntree=200, nodesize=15)

GemPredictForest = predict(GemForest, newdata=Test) # automatically assumes threshold = 0.5 and directly predicts 0 or 1
table(Test$price, GemPredictForest)

GemPredictForest = predict(GemForest, newdata = Test, type = "prob") # returns P(y=0) and P(y=1) for every observation in test data
table(Test$price, GemPredictForest[,2] > 0.75) # The second column of StevensPredictForest returns P(y=1), which we compare with the threshold 0.75


