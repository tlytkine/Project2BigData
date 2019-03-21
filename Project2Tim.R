# Install packages 
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")
# Load libraries 
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(base)
BlackFriday <- read_excel("BlackFriday.xlsx",col_names=TRUE)
# original data set 
BlackFriday

# Summary
summary(BlackFriday)
# Describe 
describe(BlackFriday)
# Copy data to a working variable 
BlackFridayClean <- BlackFriday
# Removing missing data
BlackFridayClean <- na.omit(BlackFridayClean)
# View BlackFridayClean
BlackFridayClean
colnames(BlackFridayClean)
bf <- BlackFridayClean
colnames(bf)

bf$City_Category[bf$City_Category=="A"] <- "DC"
bf$City_Category[bf$City_Category=="B"] <- "NYC"
bf$City_Category[bf$City_Category=="C"] <- "Chicago"
bf

cities <- split(bf,bf$City_Category)

cities
NYC <- cities$NYC
DC <- cities$DC
Chicago <- cities$Chicago

NYC
DC
Chicago

summary(NYC)
summary(DC)
summary(Chicago)


describe(NYC)
describe(DC)
describe(Chicago)

str(NYC)
psych::describe(NYC,na.rm=TRUE)
NYC.clean <- na.omit(NYC)
NYC.clean
NYC.clean$Marital_Status

NYC.data <- select(NYC,User_ID,Gender,Age,Occupation,Purchase,Marital_Status)
NYC.data$Gender

NYC.data.gender <- split(NYC.data,NYC.data$Gender)

NYC.data.male <- NYC.data.gender$M
NYC.data.female <- NYC.data.gender$F

NYC.data.male.maritalstatus <- split(NYC.data.male,NYC.data.male$Marital_Status)
NYC.data.female.maritalstatus <- split(NYC.data.female,NYC.data.female$Marital_Status)

NYC.data.male.single <- NYC.data.male.maritalstatus$`0`
NYC.data.male.married <- NYC.data.male.maritalstatus$`1`
NYC.data.male.married

NYC.data.female.single <- NYC.data.female.maritalstatus$`0`
NYC.data.female.married <- NYC.data.female.maritalstatus$`1`



