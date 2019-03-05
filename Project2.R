# Install packages 
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")
# Load libraries 
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
BlackFriday <- read_excel("BlackFriday.xlsx")

BlackFriday.df<-BlackFriday[,1:8]
BlackFriday.df[1:8]
summary(BlackFriday.df) 
describe(BlackFriday.df)


# Summarize 
summary(BlackFriday)
# Describe 
describe(BlackFriday)
# Copy data to a working variable 
BlackFridayClean <- BlackFriday
# Removing missing data
BlackFridayClean <- na.omit(BlackFridayClean)
# View BlackFridayClean
BlackFridayClean
# Scale the data
# (Doesn't work) BlackFridayClean <- scale(BlackFridayClean)

# Euclidean Distance: sum of the squares of the pairs of observations

# Manhattan Distance: sum of the distance between pairs of observations

# Pearson correlation distance 

# Spearman correlation distance: computes correlation btwn rank of x & y variables
BlackFridayClean$User_ID
BlackFridayClean$Product_ID
BlackFridayClean$Gender
BlackFridayClean$Age
BlackFridayClean$Occupation
BlackFridayClean$City_Category
BlackFridayClean$Stay_In_Current_City_Years
BlackFridayClean$Marital_Status
BlackFridayClean$Product_Category_1
BlackFridayClean$Product_Category_2
BlackFridayClean$Product_Category_3
BlackFridayClean$Purchase

genderVsYears <- select(BlackFridayClean,Gender,Stay_In_Current_City_Years)
genderVsYears <- na.omit(genderVsYears)
genderVsYears

BlackFridayClean$Purchase <- as.integer(BlackFridayClean$Purchase)

genderVsPurchase <- select(BlackFridayClean,Gender,Purchase)
genderVsPurchase <- na.omit(genderVsPurchase)
genderVsPurchase

genderVsAge <- select(BlackFridayClean,Gender,Age)
genderVsAge <- na.omit(genderVsAge)
genderVsAge

count(genderVsAge, vars = "0-17")

# K means clustering: the most commonly used unsupervised "machine learning" algorithm for partitioning a given data set into a set of k groups (k clusters) where k represents
# the number of groups pre-specified by the analyst. In k means clustering each cluster is represented by its center (i.e. centroid) which corresponds to the 
# mean of points assigned to the cluster. Standard agorithm: Hartigan-Wong, defines  total within cluster variation as the sum of squared distances 
kc <- kmeans(BlackFridayClean$Purchase,centers=2,nstart=10)
kc

# Structure of BlackFridayClean
str(BlackFridayClean)

# psych::describe
psych::describe(BlackFridayClean,na.rm=TRUE)

BlackFridayClean.df <- BlackFridayClean
BlackFridayClean.df2 <- BlackFridayClean.df

BlackFridayClean.df2 <- na.omit(BlackFridayClean.df2)

BlackFridayClean.df2[is.na(BlackFridayClean.df2)<-0]
BlackFridayClean.df2 <- na.omit(BlackFridayClean.df2)


sum(sapply(BlackFridayClean.df2,is.na))
sum(sapply(BlackFridayClean.df2,is.infinite))
sum(sapply(BlackFridayClean.df2,is.nan))

BlackFridayClean.k3<-kmeans(BlackFridayClean.df2,3)

BlackFridayClean.df3 <- BlackFridayClean.df2[complete.cases(BlackFridayClean.df2),]
sum(sapply(BlackFridayClean.df3, is.infinite))
BlackFridayClean.df3[apply(sapply(BlackFridayClean,is.finite),1,all),]

BlackFridayClean.df3$User_ID <- as.numeric(BlackFridayClean.df3$User_ID)
BlackFridayClean.df3$Product_ID <- BlackFridayClean.df2$Product_ID 
BlackFridayClean.df3

as.integer(BlackFridayClean.df3$Stay_In_Current_City_Years)
BlackFridayClean.df3$User_ID <- as.integer(BlackFridayClean.df3$User_ID)
BlackFridayClean.df3$Occuptation <- as.integer(BlackFridayClean.df3$Occupation)
BlackFridayClean.df3$Stay_In_Current_City_Years <- as.integer(BlackFridayClean.df3$Stay_In_Current_City_Years)
BlackFridayClean.df3 <- na.omit(BlackFridayClean.df3)
BlackFridayClean.df3$Marital_Status <- as.integer(BlackFridayClean.df3$Marital_Status)
BlackFridayClean.df3



plot(idk,col=kc3$cluster)

ggplot(small,aes(x=Purchase,y=Stay_In_Current_City_Years)) + geom_point()






