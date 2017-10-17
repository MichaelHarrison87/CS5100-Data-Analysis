library(MASS)
attach(Boston)

Boston_Sort <- Boston[order(age, decreasing = FALSE), ]
crim01 <- 1*(crim>=median(crim))
Boston.varsToUse <- names(Boston)[-(names(Boston)=="crim")]

# Fit & Plot Logistic Regression, with Logistic Curve

for (varToPlot in Boston.varsToUse) {
  xToPlot <- Boston[,varToPlot]
  logistic.crim01.test <- glm(crim01 ~ xToPlot, data=Boston, family = binomial)
  plot(crim01 ~ xToPlot, xlab=varToPlot, ylab="crim01")
  curve(predict(logistic.crim01.test, data.frame(xToPlot=x), type="resp"), add=TRUE, col="red")
}


