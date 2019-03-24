# Load libraries 
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(base)
library(data.table)
library(plyr)

# load data from csv
BlackFriday <- read.csv("BlackFriday.csv",stringsAsFactors = FALSE, header = TRUE, na.strings = c ('NA',''))

# Set names for data 
names(BlackFriday) <- c("User_ID","Product_ID","Gender","Age","Occupation","City_Category","Stay_In_Current_City_Years","Marital_Status","Product_Category_1","Product_Category_2","Product_Category_3","Purchase")

# Check out the data
BlackFriday

# Copy data to a working variable 
BlackFridayClean <- BlackFriday

# Removing missing data
BlackFridayClean <- na.omit(BlackFridayClean)

# View BlackFridayClean
BlackFridayClean
colnames(BlackFridayClean)
bf <- BlackFridayClean
colnames(bf)
# Summary
summary(bf)
# Describe 
describe(bf)

# Structure of bf 
str(bf)


# Cluster Analysis
#  Check which values are numeric 
sapply(bf,is.numeric) # User_ID, Occupation, Product Category 1, Product Category 2, Product Category 3, Purchase, Marital Status are numeric

# Make table of only numeric values 
bfn <- dplyr::select(bf,User_ID,Occupation,Product_Category_1,Product_Category_2,Product_Category_3,Purchase,Marital_Status)

# Take out na's 
bfn <- na.omit(bfn)

# Check out bfn 
bfn

# Scale the data 
bfn.scaled <- scale(bfn)

# Min-Max Normalization
normalize <- function(x) {(((x-min(x))/max(x)-min(x)))}

# Apply normalize function 
normalize(bfn.scaled)

#  Screen for Outliers 

#  Calculate Distances (Classical methods for distance measures are Euclidean and Manhattan Distances)
#   - Select a distance measure: Euclidean, Manhattan, Minkowski, and so on 
dist(bfn[1:100,])

#  Select a Clustering Algorithm - you will use K-Means and one other
#  Obtain one or more cluster solutions 

# k-means clustering of size 2 
k1 <- kmeans(bfn.scaled,centers=2,nstart = 25)
k1

# iClust 
iclust(bfn.scaled)

#  Determine # Clusters present 
#   - Tough problem, no generic solutions 
#   - Domain knowledge helpful
#   - Use the NbClust package which has 30 indices 


#  Obtain final cluster solution
#   - Perform a final clustering with the best set of parameters 
#   - There is NO optimal solution, usually 

#  Visualize the results 
#   - Hierarchical clustering usually displayed as a dendrodram
#   - Partitioning clustering usually displayed as a bivariate cluster plot
#   - You may have to select pairs of variables to plot (latter case)

#  Interpret the clusters 
#   - Examining the clusters, find what is common and what is not 
#   - Requires domain knowledge 
#   - Obtain summary statistics 
#   - If categorical variables, look at modes and/or category distributions

#  Validate the clusters 
#   - Are these groupings represented in the real world in some way?
#   - If a different clustering method was used, would the same clusters be obtained?
#   - Try the fpc, clv, and clValid packages 


# Divide into training and testing sets 
# 50-50
bf$id <- 1:nrow(bf)
train50 <- bf %>% dplyr::sample_frac(.50)
test50  <- dplyr::anti_join(bf, train50, by = 'id')
str(train50)
str(test50)

# 60-40
train60 <- bf %>% dplyr::sample_frac(.60)
test40  <- dplyr::anti_join(bf, train60, by = 'id')
str(train60)
str(test40)

# 70-30
train70 <- bf %>% dplyr::sample_frac(.70)
test30  <- dplyr::anti_join(bf, train70, by = 'id')
str(train70)
str(test30)





