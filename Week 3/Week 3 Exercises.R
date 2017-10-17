# CS5100 Data Analysis - Week 3 Exercises

Auto <- read.table("Auto.data", header=TRUE, na.strings = "?")
Auto <- na.omit(Auto)
attach(Auto)

lm.mpg.horsepower <- lm(mpg~horsepower)

# Q1a - Yes, coefficient of horsepower is negative; can confirm by plotting mpg vs horsepower and seeing a downward slope.

# Q1b - Predicted mpg is: 39.9359 + (98 * -0.1578) = 24.47

plot(horsepower, mpg)
abline(lm.mpg.horsepower, col="red")

library(MASS)
data(Boston)
attach(Boston)

lm.crim.zn <- lm(crim~zn)
lm.crim.indus <- lm(crim~indus)
lm.crim.chas <- lm(crim~chas)
lm.crim.dis <- lm(crim~dis)


plot(zn, crim)
abline(lm.crim.zn, col="red")

plot(indus, crim)
abline(lm.crim.indus, col="red")

plot(chas, crim)
abline(lm.crim.chas, col="red")

plot(dis, crim)
abline(lm.crim.dis, col="red")


lm.crim.all <- lm(crim~., data=Boston)

coeffsIndiv <- double()
coeffsPolynomial <- data.frame(Variable = character()
                               ,beta0 = double()
                               ,beta1 = double()
                               ,beta2 = double()
                               ,beta3 = double()
                               , stringsAsFactors = FALSE)


varsToUse <- names(Boston)[-(names(Boston)=="crim")]

i=1
for (var in varsToUse) {
  
  colToUse <- Boston[,var]
  colToUse_sq <- colToUse^2
  colToUse_cb <- colToUse^3
  lm.var <- lm(crim~colToUse)
  
  poly.var <- lm(crim~colToUse + colToUse_sq + colToUse_cb)
  
  
  coeffsIndiv[i] = lm.var$coefficients[2]
  coeffsPolynomial[i,] = c(var
                          , poly.var$coefficients[1]
                          , poly.var$coefficients[2]
                          , poly.var$coefficients[3]
                          , poly.var$coefficients[4])
  
  sumr <- summary(poly.var)
  print(var)
  print(sumr)
  
  i=i+1

}

names(coeffsIndiv) <- varsToUse

coeffsAll <- lm.crim.all$coefficients[-1]
plot(coeffsIndiv, coeffsAll)

# Note, coeff of nox in single variable model is 31, suggesting very strong, positive correlation with crime.
# However, in the multivariate model, its param is -10 - suggesting a negative correlation!
# This is because nox has high correlations with other factors, e.g. dis,rad,tax etc - that better explain the positive correlation.


# Q2d - Testing Significance of Cubic Model 

# zn - square & cubic term not significant
# indus - all terms significant
# chas - NA's for square & cubic term;  chas is categorical?
# nox - all terms significant
# rm - no terms significant
# age - linear term not significant; square term signif @ 5% level; cubic term signif @ 1% level
# dis - all terms significant
# rad - no tems signifcant
# tax - no terms significant
# ptratio - all terms signif at 1% level
# black - no terms significant
# lstat - no terms significant
# medv - all terms significant

# Although - the point above re correlated/confounding factors within this data
# means "significance" in this univariate analysis should be 
# treated with caution. May simply be reflecting causation by some other undelying
# factor that is correlated.


# Q3
Auto <- read.table("Auto.data", header=TRUE, na.strings = "?")
attach(Auto)
mpg01 <- 1*(mpg>=median(mpg))
Auto.varsToUse <- names(Auto)[-(names(Auto)=="mpg")]

for (v in Auto.varsToUse) {
  
  plot(Auto[,v],mpg01, xlab=v, ylab="mpg01")
  
  
}

# Q3b
# Observations:
# Cylinders - categorical, both 0 & 1 in all categories
# Displacement - clusters to 1 (i.e. above median mpg) at the lower displacements
# Horsepower - similarly, cluster of high mpg at lower horsepowers.
# Weight - ditto for lower weight cars
# Acceleration - broad overlap in the middle; but in the tails
# lower accel cars have lower mpg; higher than higher.
# Year - categorical, both 0 & 1 in all categories
# Origin - same
# Name - partitions each data point (exactly?), so not useful for prediction.
# (which stands to reason!)

# Variables of interest:
# Displacement, Horsepower, Weight, Acceleration

set.seed(3)

Auto <- cbind(Auto,mpg01)

Num.Train <- ceiling(nrow(Auto)/2)
Index.Train <- sample(1:nrow(Auto), Num.Train)

Auto.Train <- Auto[Index.Train, ]
Auto.Test <- Auto[-Index.Train, ]

logistic.mpg01 <- glm(mpg01 ~ displacement + horsepower + weight + acceleration
                      , data=Auto.Train
                      , family=binomial)

Test.Probs <- predict(logistic.mpg01, type="response", newdata=Auto.Test)

Test.Pred <- rep(0,nrow(Auto.Test))
Test.Pred[Test.Probs>=0.5] <- 1
Test.Actual <- Auto.Test$mpg01

Test.Error <- mean(Test.Actual!=Test.Pred)

# Q3d
# Used model including Displacement, Horsepower, Weight, Acceleration
# Displacement was most significant at 1% level; the horsepower at 5%
# Weight & Acceleration were not significant
# Resulting Test Error was 13.6%.


library(class)

train.X <- as.matrix(Auto.Train$displacement)
train.Y <- as.matrix(Auto.Train$mpg01)
test.X <- as.matrix(Auto.Test$displacement)

for (k in c(1:196)) {
  
  model.knn <- knn(train.X
                   ,test.X
                   ,train.Y
                   ,k)
  
  model.knn.test.error <- mean(Test.Actual!=model.knn)
  to.print <- paste(k, ": ", model.knn.test.error)  
  print(to.print)
  
}

# Q4e
# Calcing knn model error for k=1 to 196 (i.e. size of training set)
# Shows model makes same predictions for large swathes of k - suggesting
# a reasonable level of clustering.
# "Best" test error appears to be ~11.2% for k=15
# (Note test & training were equal-sized, splittling original data into halves)


# Q5 - Explore crim in Boston data set

crim01 <- 1*(crim>=median(crim))
data(Boston)
attach(Boston)
Boston <- cbind(Boston, crim01)


# Split into Training & Test Sets
set.seed(6)
Boston.Train.Num <- ceiling((80*nrow(Boston))/100)
Boston.Train.Index <- sample(1:nrow(Boston), Boston.Train.Num)

Boston.Train <- Boston[Boston.Train.Index, ]
Boston.Test<- Boston[-Boston.Train.Index, ]


lm.crim.all <- lm(crim~., data=Boston)

# Most significant factors in multiple regression against the whole data set are:
# dis @ <0.1% level - distance to employment centers
# rad @ <0.1% level - accessibility of radial highways
# medv @ 1% level - median value of owner-occupied homes


# Logistic Regression

logistic.crim <- glm(crim01 ~ .-crim, data=Boston.Train, family=binomial)

Boston.Test.Pred <- rep(0,nrow(Boston.Test))
Boston.Test.Probs <- predict(logistic.crim.sig3, type="response", newdata=Boston.Test)
Boston.Test.Pred[Boston.Test.Probs>=0.5] <- 1

Boston.Test.Actual <- Boston.Test$crim01
Boston.Test.Error <- mean(Boston.Test.Actual!=Boston.Test.Pred)

# Fit & Plot Logistic Regression

varToPlot <- "rad"
xToPlot <- Boston[,varToPlot]
logistic.crim01.test <- glm(crim01 ~ xToPlot, data=Boston, family = binomial)
plot(crim01 ~ xToPlot, xlab=varToPlot, ylab="crim01")
curve(predict(logistic.crim01.test, data.frame(xToPlot=x), type="resp"), add=TRUE, col="red")


# Test Error for:
# (Training:Test 50:50) dis + rad + medv: 18.1% 
# (Training:Test 80:20) dis + rad + medv: 17.8%.
# (Training:Test 80:20) age: 16.8% - age better than dis+rad+medv, despite the latter being most significant in multiple regression
# (Training:Test 80:20) dis: 24.7%

# Fitting glm with all factors (except crim) gave test error 10.9%.
# Most significant factors here were:
# nox @ <0.1% level - Nitrogen Oxide concentration
# rad @ <0.1% level - accessibility of radial highways

# KNN

for (k in c(1:nrow(Boston.Train))) {

Boston.Train.X.knn <- as.matrix(subset(Boston.Train, select = -c(crim,crim01)))  
Boston.Test.X.knn <- as.matrix(subset(Boston.Test, select = -c(crim,crim01)))  
Boston.Train.Y.knn <- as.matrix(Boston.Train$crim01)
Boston.Test.Y.knn <- as.matrix(Boston.Test$crim01)



knn.crim01 <- knn(Boston.Train.X.knn,Boston.Test.X.knn, Boston.Train.Y.knn, k)
knn.crim01.error <- mean(Boston.Test.Y.knn != knn.crim01)
print(paste(k, ":", knn.crim01.error, sep=""))

}

# Lowest test error seems to be ~4.0% for k=5, at least when using all factors to classify