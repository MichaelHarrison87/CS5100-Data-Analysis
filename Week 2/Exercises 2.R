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


## Create Training & Test Data
set.seed(10)
train <- sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]

tree.Carseats <- tree(Sales~., data=Carseats, subset=train)
plot(tree.Carseats)
text(tree.Carseats)

# Observations
# Tree has 20 terminal nodes
# root node is ShelveLoc
# Then after price - tree is small for Price<107.5, likel as obs run out



pred.Carseats <- predict(tree.Carseats, newdata= Carseats[-train,])

# Test MSE
TestMSE <- mean((pred.Carseats- Carseats.test$Sales)^2)

cv.Carseats <- cv.tree(tree.Carseats)
plot(cv.Carseats$size, cv.Carseats$dev, type="b")

# Plotting cv.Carseats deviance vs size, 20 nodes is apparently optimal
# So can't improve MSE by pruning the original tree.Carseats


