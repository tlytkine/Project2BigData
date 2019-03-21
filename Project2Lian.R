# attribute for marital status attribute
# Install packages 
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")
install.packages("ggplot2")

# Load libraries 
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
BlackFriday <- read_excel("BlackFriday.xlsx")
# original - printing the starting point
BlackFriday


# Summary
summary(BlackFriday)
# Describe 
describe(BlackFriday)
# Copy data to a working variable 
BlackFridayClean <- BlackFriday
# Removing missing data // clean the data even more
BlackFridayClean <- na.omit(BlackFridayClean)
# View BlackFridayClean
BlackFridayClean

# New summary
summary(BlackFridayClean)

str(BlackFridayClean)

# User_ID
BlackFridayClean$User_ID <- as.factor(BlackFridayClean$User_ID)
BlackFridayClean$User_ID

# Martial_Status
BlackFridayClean$Marital_Status <- as.factor(ifelse(BlackFridayClean$Marital_Status == 1, 'Married', 'Single'))
BlackFridayClean$Marital_Status

# Make the data distinct
BF_Distinct <- distinct(BlackFridayClean, User_ID, Age, Gender, Marital_Status, Occupation, City_Category, Stay_In_Current_City_Years)
BF_Distinct

# 1) Gender and marital status data
d1 <- table(BF_Distinct$Gender, BF_Distinct$Marital_Status)
d1

# Plot of this data
plot1 <- ggplot(BF_Distinct, aes(x=Gender, fill= Marital_Status)) + geom_bar(position = "dodge") + ggtitle("") +  labs(x="Gender",y="No. of distinct Sales") + annotate(geom = "text", x = 0.775, y = 619, label = "719")   + annotate(geom = "text", x = 1.225, y = 847, label = "947") + annotate(geom = "text", x = 1.775, y = 1655, label = "1755") + annotate(geom = "text", x = 2.225, y = 2370, label = "2470") + scale_fill_manual(values=c("blue","limegreen")) 
d1; plot1

# Age group, occupation and marital status
plot2 <- ggplot(BF_Distinct,aes(x=Occupation, fill=Age))+geom_bar()+facet_grid(Gender~Marital_Status)
plot2
