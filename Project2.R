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






