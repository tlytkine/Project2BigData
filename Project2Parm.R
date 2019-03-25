# loading possibly necessary libraries




library(data.table)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(rpart)
library(randomForest)
library(stepPlr)
library(C50)
library(plyr)
library(MASS)
library(caret)
library(caretEnsemble)
library(dplyr)

# loading data
BlackFriday <- read.csv("BlackFriday.csv",  stringsAsFactors = FALSE, header = TRUE, na.strings = c('NA',''))

# copying data to a variable
BlackFridayWorking <- BlackFriday

# removing missing data
BlackFridayClean <- na.omit(BlackFridayWorking)

# dividing data into training and testing sets
TrainSet <- sample(nrow(BlackFridayClean), 0.7*nrow(BlackFridayClean))

TrainSetClean <- BlackFridayClean[TrainSet,]
TrainSetClean[1:10,]

TestSetClean <- BlackFridayClean[-TrainSet,]
TestSetClean[1:10,]

# removing missing data
TrainSetClean <- na.omit(TrainSetClean)
TestSetClean <- na.omit(TestSetClean)

# structure of training dataset
str(TrainSetClean)

# checking for missing data
sapply(TrainSetClean, function(x) sum(is.na(x)))

# structure of testing dataset
str(TestSetClean)

# checking for missing data
sapply(TestSetClean, function(x) sum(is.na(x)))

#looking at data
TrainSetClean
TestSetClean

# describing and summarizing training dataset
describe(TrainSetClean) # 13 warnings
summary(TrainSetClean)

# describing and summarizing testing dataset
describe(TestSetClean) # 13 warnings
summary(TestSetClean)

# normalize data sets
normalize <- function(x) {((x-min(x))/(max(x)-min(x)))}

#
TrainSetClean$User_ID <- as.factor(TrainSetClean$User_ID)
TrainSetClean$Product_ID <- as.factor(TrainSetClean$Product_ID)
TrainSetClean$Marital_Status <- as.factor(ifelse(TrainSetClean$Marital_Status == 1, 'Married', 'Single'))
TrainSetClean$Age <- as.factor(TrainSetClean$Age)
TrainSetClean$Gender <- as.factor(ifelse(TrainSetClean$Gender=='M', 'Male', 'Female'))
TrainSetClean$Occupation <- as.factor(TrainSetClean$Occupation)
TrainSetClean$City_Category <- as.factor(TrainSetClean$City_Category)
TrainSetClean$Stay_In_Current_City_Years <- as.factor(TrainSetClean$Stay_In_Current_City_Years)

# structure of data set
str(TrainSetClean)
