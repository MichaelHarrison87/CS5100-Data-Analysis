## Exercises - Week 1 Test


setwd('Y:\\01 MSc Machine Learning\\01 Term 1\\CS5100 Data Analysis\\Data')

Auto <- read.table("Auto.data", header=T, na.strings="?")
Auto <- na.omit(Auto)

# Q1
# Cylinders, Year, Orign &  Name are qualitative - i.e. take a fixed set of values.
# The remaining columns are quantitative


# Q2 & Q3
Ranges = data.frame(Name=character()
                    ,RangeMin=numeric()
                    ,RangeMax=numeric()
                    ,Mean = numeric()
                    ,StdDev = numeric()
                    ,stringsAsFactors = FALSE)


for (colNum in 1:(ncol(Auto)-1)) {
  
  colName = names(Auto)[colNum]
  colData = Auto[,colNum]
  
  colRange = range(colData)
  colMean = mean(colData)
  colStdDev = sd(colData)
  
  Ranges[colNum, ] = c(colName
                       , colRange[1]
                       , colRange[2]
                       , colMean
                       , colStdDev) 

}



# Q4

Auto_Partial <- Auto[-c(10:85),]

Ranges_Partial = data.frame(Name=character()
                    ,RangeMin=numeric()
                    ,RangeMax=numeric()
                    ,Mean = numeric()
                    , StdDev = numeric()
                    ,stringsAsFactors = FALSE)


for (colNum in 1:(ncol(Auto_Partial)-1)) {
  
  colName = names(Auto_Partial)[colNum]
  colData = Auto_Partial[,colNum]
  
  colRange = range(colData)
  colMean = mean(colData)
  colStdDev = sd(colData)
  
  Ranges_Partial[colNum, ] = c(colName
                       , colRange[1]
                       , colRange[2]
                       , colMean
                       , colStdDev) 
  
}


# Q5
# Inspected all 2-way scatterplots between pairs of variables of Auto, using pairs(Auto).
# Seems to be a negative qudaratic shape (i.e. a decelerating effect) of mpg vs each of horsepower, weight & displacement.
# Although these 3 variables are correlated since they relate to the size of the engine.
# The scatterplots of these 3 are roughly linear wrt to each, which confirms this.
# More cylinders is also associated with higher mpg, although there are overlaps among the different # of cylinders. Will depend on the exact model of car.

# Acceleration declines with more cylinders, or greater horsepower. Acceleration is time from 0-60mph, so a decline implies faster acceleration.
# This is again unsurprising, as more powerful engine designs will have faster acceleration.


# Q6
# Per the commentary for Q5, horsepower, displacement or weight correlate with mpg and so may be useful in predicting mpg.
# Of these, weight seems to be the perhaps the most obvious choice, since  the physical relation is intuitive - heavier vehicles require more gas.

