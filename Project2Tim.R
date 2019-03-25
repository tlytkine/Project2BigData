install.packages("outliers")

# Load libraries 
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(base)
library(data.table)
library(plyr)
library(stringr)
library(cluster)
library(outliers)

# Preparing the data 
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

bf
# Make gender numeric 
# 0 = female, male = 1
bf[bf=="F"] <- 0
bf[bf=="M"] <- 1
bf

# Change gender to numeric 
bf$Gender <- as.numeric(as.character(bf$Gender))
bf$Age
str(bf)

# Make age numeric

# Copy bf to modify it 
bfCopy <- bf

# Check unique values of Age 
unique(bfCopy$Age)

# Get number of rows in data frame 
print(nrow(bfCopy))

# Set age range to random values 
for(row in 1:nrow(bfCopy)){
  # Assuming the youngest age people shop during BlackFriday is 1
  if(bfCopy[row,"Age"]=="0-17"){ # Generation Z 
    bfCopy[row,"Age"] <- floor(runif(1,min=13,max=17))
  } else if(bfCopy[row,"Age"]=="18-25"){ # Millenials
    bfCopy[row,"Age"] <- floor(runif(1,min=18,max=25))
  } else if(bfCopy[row,"Age"]=="26-35"){ # Millenials
    bfCopy[row,"Age"] <- floor(runif(1,min=26,max=35))
  } else if(bfCopy[row,"Age"]=="36-45"){ # Generation X 
    bfCopy[row,"Age"] <- floor(runif(1,min=36,max=45))
  } else if(bfCopy[row,"Age"]=="46-50"){ # Generation X 
    bfCopy[row,"Age"] <- floor(runif(1,min=46,max=50))
  } else if(bfCopy[row,"Age"]=="51-55"){ # Baby Boomers 
    bfCopy[row,"Age"] <- floor(runif(1,min=51,max=55))
  } else if(bfCopy[row,"Age"]=="55+"){ # Baby Boomers 
    bfCopy[row,"Age"] <- floor(runif(1,min=56,max=80))
  }
  print(row)
}

bf <- bfCopy

bf
# Check unique ages
unique(bf$Age)

# Structure of bf 
str(bf)

# Change age to numeric 
bf$Age <- as.numeric(as.character(bf$Age))
bf$Age


# Getting rid of first character of Product_ID
for(row in 1:nrow(bf)){
  bf[row,"Product_ID"] <- substring(bf[row,"Product_ID"],2,nchar(bf[row,"Product_ID"]))
  print(row)
}

bf$Product_ID
str(bf)

# Change Product_ID to numeric 
bf$Product_ID <- as.numeric(as.character(bf$Product_ID))
bf$Product_ID
str(bf)




# Change city_category to numeric, A = 1, B = 2, C = 3 
unique(bf$City_Category)

bf$City_Category

bf$City_Category[bf$City_Category=="A"] <- 1
bf$City_Category[bf$City_Category=="B"] <- 2
bf$City_Category[bf$City_Category=="C"] <- 3

bf$City_Category <- as.numeric(as.character(bf$City_Category))

str(bf)

# Change Stay_in_current_city_years to numeric
unique(bf$Stay_In_Current_City_Years)

str(bf)
 # Change 4+ to random number between 4 and 80 (oldest person is 80)
for(row in 1:nrow(bf)){
  if(bf[row,"Stay_In_Current_City_Years"]=="4+"){ 
    bf[row,"Stay_In_Current_City_Years"] <- floor(runif(1,min=4,max=80))
  } 
  print(row)
}
bf$Stay_In_Current_City_Years
unique(bf$Stay_In_Current_City_Years)
bf$Stay_In_Current_City_Years <- as.numeric(as.character(bf$Stay_In_Current_City_Years))
str(bf)



# Lecture 4 R Functions Experimentation
summary(bf)
psych::describe(bf,na.rm=TRUE)
str(bf)





# Cluster Analysis
#  Check which values are numeric 
sapply(bf,is.numeric) # User_ID, Occupation, Product Category 1, Product Category 2, Product Category 3, Purchase, Marital Status are numeric
# All values are now numeric 

# Find descriptive clusters of individuals in the data set 
# Each subset should contain UserID, gender, age 




bfOccupation <- dplyr::select(bf,User_ID,Gender,Age,Occupation,Purchase)

bfMaritalStatus <- dplyr::select(bf,User_ID,Gender,Age,Marital_Status,Purchase)

bfProductID <- dplyr::select(bf,User_ID,Gender,Age,Product_ID,Purchase)



# Try clustering on entire data set 
bfn <- bf


# Take out na's 
# Entire data set 
bfn <- na.omit(bfn)
# Occupation
bfOccupation <- na.omit(bfOccupation)
# Martial Status
bfMaritalStatus <- na.omit(bfMaritalStatus)
# Product ID 
bfProductID <- na.omit(bfProductID)


# Scale the data 
bfn.scaled <- scale(bfn)
bfOccupation.scaled <- scale(bfOccupation)
bfMaritalStatus.scaled <- scale(bfMaritalStatus)
bfProductID.scaled <- scale(bfProductID)

# Min-Max Normalization
normalize <- function(x) {(((x-min(x))/max(x)-min(x)))}
# Apply normalize function 
normalize(bfn.scaled)
normalize(bfOccupation.scaled)
normalize(bfMaritalStatus.scaled)
normalize(bfProductID.scaled)

#  Screen for Outliers 
# Use outliers package or mvoutlier for multivariate outliers
# Use clustering technique robust in presence of outliers 
# rm.outlier removes outliers with the biggest difference away from the mean value

# Stay_In_Current_City_Years
out1<-outlier(bfn.scaled[row, "Stay_In_Current_City_Years"], opposite = FALSE, logical=FALSE)
rm.outlier(bfn.scaled[row, "Stay_In_Current_City_Years"], fill = FALSE, median = FALSE, opposite = FALSE)
# prints what was removed
out1

# Purchase
out2<-outlier(bfn.scaled[row, "Purchase"], opposite = FALSE, logical=FALSE)
rm.outlier(bfn.scaled[row, "Purchase"], fill = FALSE, median = FALSE, opposite = FALSE)
# prints what was removed
out2


#  Calculate Distances 
dist(bfn.scaled[1:00,])
     
     
#  Select a Clustering Algorithm 
# Initial k should be sqrt(n) (n is number of points)
numRows <- nrow(bfn)
initialK <- sqrt(numRows)

# k-means clustering of size 3 
# Whole data set 
k3Whole <- kmeans(bfn.scaled,centers=3,nstart = 25)
# Occupation
k3Occupation <- kmeans(bfOccupation.scaled,centers=3,nstart = 25)
# Marital Status 
k3MaritalStatus <- kmeans(bfMaritalStatus.scaled,centers=3,nstart = 25)
# ProductID 
k3ProductID <- kmeans(bfProductID.scaled,centers=3,nstart = 25)



# k-means clustering of size 5
# Whole data set 
k5Whole <- kmeans(bfn.scaled,centers=5,nstart = 25)
# Occupation
k5Occupation <- kmeans(bfOccupation.scaled,centers=5,nstart = 25)
# Marital Status 
k5MaritalStatus <- kmeans(bfMaritalStatus.scaled,centers=5,nstart = 25)
# ProductID 
k5ProductID <- kmeans(bfProductID.scaled,centers=5,nstart = 25)



# k-means clustering of size 7 
# Whole data set 
k7Whole <- kmeans(bfn.scaled,centers=7,nstart = 25)
# Occupation
k7Occupation <- kmeans(bfOccupation.scaled,centers=7,nstart = 25)
# Marital Status 
k7MaritalStatus <- kmeans(bfMaritalStatus.scaled,centers=7,nstart = 25)
# ProductID 
k7ProductID <- kmeans(bfProductID.scaled,centers=7,nstart = 25)


# iClust 
# Whole data set 
iclust(bfn.scaled,nclusters=3)
iclust(bfn.scaled,nclusters=5)
iclust(bfn.scaled,nclusters=7)
# Occupation
iclust(bfOccupation.scaled,nclusters=3)
iclust(bfOccupation.scaled,nclusters=5)
iclust(bfOccupation.scaled,nclusters=7)
# Marital Status 
iclust(bfMaritalStatus.scaled,nclusters=3)
iclust(bfMaritalStatus.scaled,nclusters=5)
iclust(bfMaritalStatus.scaled,nclusters=7)
# ProductID 
iclust(bfProductID.scaled,nclusters=3)
iclust(bfProductID.scaled,nclusters=5)
iclust(bfProductID.scaled,nclusters=7)


k3Whole$totss
k3Whole$centers
k3Whole$iter

str(k3Whole)

# Try hclust 
hc <- hclust(dist(bfProductID[1:100,]),"ave")
plot(hc)

wssplot <- function(data,nc=15,seed=1234)
{
  wss <- (nrow(data)-2)*sum(apply(data,2,var))
  for(i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc, wss, type="b",
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}


#  Try wssplot 
# A bend in the graph can suggest the appropriate number of clusters 
# Bends 
wssplot(bfn,nc=3,seed=1234)
# nc = 3, 2 clusters 
wssplot(bfn,nc=5,seed=1234)
# nc = 5, 3 clusters 
wssplot(bfn,nc=7,seed=1234)
# nc = 7, 4 clusters 



#  Determine # Clusters present 



#  Obtain final cluster solution

# K-means clustering <- sum of squares distance should be minimized 
# total within-cluster sum of squares measures the compactness (i.e. goodness)
# of the clustering and we want it to be as small as possible 


#  Visualize the results 


#  Interpret the clusters 


#  Validate the clusters 




str(bf)

bf

# Scree plot: Gettting an idea of components 
bfScree <- scree(bfn,factors=TRUE)
bfScree


clusplot(pam(x=test30[1:100,],k=3,metric="euclidean",stand=FALSE))

# Principal function
bf.pca <- principal(bfn, nfactors = 2, rotate = "none")
bf.pca

bf.prcomp <- prcomp(bfn[,c(1:12)], center = T, scale. = T)
bf.prcomp
prop_var <- var(bf.pca$values)

prop_var_explained <- prop_var/sum(bf.pca$values)
# scree plot to access components or factors which explains the most of variability in the data
plot(prop_var_explained, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_var_explained), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# Root mean square = 0.08 (Measure of difference 
# between observed and expected values)

# dplyr::select(bf,Gender,Purchase)
# split(bf,bf$Gender)

# Total number of rows 
sumRows <- nrow(bf)

# Get sum of all purchases in table 
sum(bf$Purchase)
# Sum is 191564035
# Get average purchases 
mean(bf$Purchase)
# 11661 is average purchase 


# Get average age 
mean(bf$Age)
# Average age is 34.08769


str(bf)


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


str(train50)
str(test50)

train50$User_ID <- as.factor(train50$User_ID)
train50$Product_ID <-

str(train60)
str(test40)

str(train70)
str(test30)


# Plot Gender Vs. Purchase 

# Plot Age Vs. Purchase 

# Plot Occupation vs. Purchase 

# Plot City_Category vs. Purchase 

# Plot Stay_In_Current_City_Years vs. Purchase 

# Plot Marital_Status vs. Purchase 

# Plot Product_Category_1 vs. Purchase 

# Plot Product_Category_2 vs. Purchase 

# Plot Product_Category_3 vs. Purchase 




