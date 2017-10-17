# CS5800 Data Analysis
# Lab 2


### Carseats Data

## Load Packages

# install.packages("tree")
# install.packages("ISLR")
# 
# library(tree)
# library(ISLR)

## Load Data
data(Carseats)
names(Carseats)
attach(Carseats)

## Prep Data
High <- ifelse(Sales<=8,"No","Yes")
Carseats <- data.frame(Carseats, High)

tree.Carseats <- tree(High~.-Sales, Carseats) 
# Note "-Sales" removes sales from the list of predictors

# Note when R plots tress, if criteria on the node is satisfied, go down left branch.
# Otherwise go down right branch


## Create Training & Test Data
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test <- High[-train]

# Fit to training data & predict test responses
tree.Carseats <- tree(High~.-Sales, Carseats, subset=train) 
tree.pred <- predict(tree.Carseats, Carseats.test, type="class")

table(tree.pred, High.test)

# Correct prediction rate
sum(High.test==tree.pred)/length(High.test)


## Fit via cross-validation
set.seed(3)
cv.Carseats <- cv.tree(tree.Carseats, FUN=prune.misclass)

prune.Carseats <- prune.misclass(tree.Carseats, best=9)

tree.pred <- predict(prune.Carseats, Carseats.test, type="class")


#########

### Boston Data
library(MASS)
data(Boston)
names(Boston)

## Training & Test Data
set.seed(1)
train <- sample(1:nrow(Boston),nrow(Boston)/2)

## Fit tree
tree.Boston <- tree(medv~., data=Boston, subset=train)
cv.Boston <- cv.tree(tree.Boston)

## Prune Tree
prune.Boston <- prune.tree(tree.Boston, best=5)

## Predict using pruned tree
yhat <- predict(tree.Boston, newdata= Boston[-train,])
Boston.test <- Boston[-train,"medv"]

MSE <- mean((yhat - Boston.test)^2)

