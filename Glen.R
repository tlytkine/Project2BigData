# Install the packages
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")
install.packages("ggplot2")

# Load the libraries
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)

BlackFriday <- read_excel("BlackFriday.xlsx")

#Original
BlackFriday

#Summary
summary(BlackFriday)

#Description
describe(BlackFriday)

#Copy data to a new variable
BlackFridayClean <- BlackFriday

#Remove missing information
BlackFridayClean <- na.omit(BlackFridayClean)

#View new BlackFiday information
BlackFridayClean

#New BlackFriday summary
summary(BlackFridayClean)
str(BlackFridayClean)

#User_ID
BlackFridayClean$User_ID <- as.factor(BlackFridayClean$User_ID)
BlackFridayClean$User_ID

#Gender
BlackFridayClean$Gender <- as.factor(BlackFridayClean$Gender)
BlackFridayClean$Gender

#Age
BlackFridayClean$Age <- as.factor(BlackFridayClean$Age)
BlackFridayClean$Age

#Occupation
BlackFridayClean$Occupation <- as.factor(BlackFridayClean$Occupation)
BlackFridayClean$Occupation

#Distinguish Part of Data
BFC_Distinct <- distinct(BlackFridayClean, User_ID, Age, Gender, Occupation, Marital_Status, City_Category, Purchase, Product_Category_1)
BFC_Distinct

#Occupation and Age
d1 <- table(BFC_Distinct$Occupation, BFC_Distinct$Age)
d1

#Occupation and Gender
d2 <- table(BFC_Distinct$Gender, BFC_Distinct$Occupation)
d2

#Plot1
p1 <- ggplot(BFC_Distinct, aes(x=Occupation, fill=Gender)) + geom_bar( col="black") + ggtitle("") +  labs(x="Occupation",y="No. of distinct people") + scale_fill_manual(values=c("hotpink", "royalblue"))
d1; p1

#Plot2
d3 <- table(BFC_Distinct$Marital_Status, BFC_Distinct$Occupation)
p2 <- ggplot(BFC_Distinct, aes(x=Occupation, fill=Marital_Status)) + geom_bar( col="black") + ggtitle("") +  labs(x="Occupation",y="No. of distinct people") + scale_fill_manual(values=c("tan4","limegreen"))
d3;p2

#Plot3
p3 <- ggplot(BFC_Distinct,aes(x=Occupation, fill=Age))+geom_bar()+facet_grid(Gender~Marital_Status)
d3; p3
