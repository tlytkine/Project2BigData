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
library(class)
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

# Change Product_ID to numeric 
bf$Product_ID <- as.numeric(as.character(bf$Product_ID))
bf$Product_ID

# Change city_category to numeric, A = 1, B = 2, C = 3 
unique(bf$City_Category)

bf$City_Category

bf$City_Category[bf$City_Category=="A"] <- 1
bf$City_Category[bf$City_Category=="B"] <- 2
bf$City_Category[bf$City_Category=="C"] <- 3

bf$City_Category <- as.numeric(as.character(bf$City_Category))

# Change Stay_in_current_city_years to numeric
unique(bf$Stay_In_Current_City_Years)

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

# Reduce data set 
colnames(bf)
# Save bf
bfOriginal <- bf

# Add column for id to identify when creating training and testing sets 
bf$id <- 1:nrow(bf)
colnames(bf)
# Remove product id column 
bf <- within(bf,rm(Product_ID))
# Remove product categories 
bf <- within(bf,rm(Product_Category_1))
bf <- within(bf,rm(Product_Category_2))
bf <- within(bf,rm(Product_Category_3))
# Remove city 
bf <- within(bf,rm(City_Category))

colnames(bf)
# Divide into training and testing sets 
# 50-50
train50 <- bf %>% dplyr::sample_frac(.50)
test50  <- dplyr::anti_join(bf, train50, by = 'id')
train50 <- na.omit(train50)
test50 <- na.omit(test50)

train50 <- within(train50,rm(id))
test50 <- within(test50,rm(id))

# 60-40
train60 <- bf %>% dplyr::sample_frac(.60)
test40  <- dplyr::anti_join(bf, train60, by = 'id')
train60 <- na.omit(train60)
test40 <- na.omit(test40)

train60 <- within(train60,rm(id))
test40 <- within(test40,rm(id))

# 70-30
train70 <- bf %>% dplyr::sample_frac(.70)
test30  <- dplyr::anti_join(bf, train70, by = 'id')
train70 <- na.omit(train70)
test30 <- na.omit(test30)

train70 <- within(train70,rm(id))
test30 <- within(test30,rm(id))


# Testing out pairs function ??? What to do with this 
# pairs(Occupation ~ User_ID + Gender + Age, data = bf)
#  pairs(Marital_Status ~ User_ID + Gender + Age, data = bf)


# Cluster Analysis
#  Check which values are numeric 
sapply(bf,is.numeric) # User_ID, Occupation, Product Category 1, Product Category 2, Product Category 3, Purchase, Marital Status are numeric
# All values are now numeric 

# Find descriptive clusters of individuals in the data set 
# Each subset should contain UserID, gender, age 

# Try clustering on entire data set 
bfn <- bf
# Entire data set 
bfn.scaled <- scale(bfn)
# Scale the data 
train50.scaled <- scale(train50)
train60.scaled <- scale(train60)
train70.scaled <- scale(train70)

# Min-Max Normalization
normalize <- function(x) {(((x-min(x))/max(x)-min(x)))}
# Apply normalize function 
normalize(bfn.scaled)
normalize(train50.scaled)
normalize(train60.scaled)
normalize(train70.scaled)
#  Screen for Outliers 
# Use outliers package or mvoutlier for multivariate outliers
# Use clustering technique robust in presence of outliers 
# rm.outlier removes outliers with the biggest difference away from the mean value


# Stay_In_Current_City_Years
out_1 <- outlier(bfn.scaled[, "Stay_In_Current_City_Years"], opposite = FALSE, logical=FALSE)
out50_1 <- outlier(train50.scaled[, "Stay_In_Current_City_Years"], opposite = FALSE, logical=FALSE)
out60_1 <- outlier(train60.scaled[, "Stay_In_Current_City_Years"], opposite = FALSE, logical=FALSE)
out70_1 <- outlier(train60.scaled[, "Stay_In_Current_City_Years"], opposite = FALSE, logical=FALSE)
out50_1
out60_1
out70_1
rm.outlier(bfn.scaled[,"Stay_In_Current_City_Years"],fill=FALSE,median=FALSE,opposite=FALSE)
rm.outlier(train50.scaled[, "Stay_In_Current_City_Years"], fill = FALSE, median = FALSE, opposite = FALSE)
rm.outlier(train60.scaled[, "Stay_In_Current_City_Years"], fill = FALSE, median = FALSE, opposite = FALSE)
rm.outlier(train70.scaled[, "Stay_In_Current_City_Years"], fill = FALSE, median = FALSE, opposite = FALSE)
# prints what was removed


# Purchase
out_2 <- outlier(bfn.scaled[,"Purchase"],opposite=FALSE, logical = FALSE)
out50_2<-outlier(train50.scaled[, "Purchase"], opposite = FALSE, logical=FALSE)
out60_2<-outlier(train60.scaled[, "Purchase"], opposite = FALSE, logical=FALSE)
out70_2<-outlier(train70.scaled[, "Purchase"], opposite = FALSE, logical=FALSE)

rm.outlier(bfn.scaled[,"Purchase"],fill=FALSE,median=FALSE,opposite=FALSE)
rm.outlier(train50.scaled[, "Purchase"], fill = FALSE, median = FALSE, opposite = FALSE)
rm.outlier(train60.scaled[, "Purchase"], fill = FALSE, median = FALSE, opposite = FALSE)
rm.outlier(train70.scaled[, "Purchase"], fill = FALSE, median = FALSE, opposite = FALSE)
# prints what was removed
out_2
out50_2
out60_2
out70_2


#  Calculate Distances 
mean(dist(bfn.scaled[sample(nrow(bfn.scaled),100),]))
mean(dist(train50.scaled[sample(nrow(train50.scaled),100),]))
mean(dist(train60.scaled[sample(nrow(train60.scaled),100),]))
mean(dist(train70.scaled[sample(nrow(train70.scaled),100),]))
     
     
#  Select a Clustering Algorithm 
# K means on training set 
# Create classification labels for training 
# k-means clustering of size 3, 5 and 7 
bfn.scaled.k3 <- kmeans(bfn.scaled,3)
train50.scaled.k3 <- kmeans(train50.scaled,3)
train60.scaled.k3 <- kmeans(train60,3)
train70.scaled.k3 <- kmeans(train70,3)

bfn.scaled.k5 <- kmeans(bfn.scaled,5)
train50.scaled.k5<- kmeans(train50,5)
train60.scaled.k5<- kmeans(train60,5)
train70.scaled.k5<- kmeans(train70,5)

bfn.scaled.k7 <- kmeans(bfn.scaled,7)
train50.scaled.k7<- kmeans(train50,7)
train60.scaled.k7<- kmeans(train60,7)
train70.scaled.k7<- kmeans(train70,7)

bfn.scaled.k3
train50.scaled.k3
train60.scaled.k3
train70.scaled.k3

bfn.scaled.k5
train50.scaled.k5
train60.scaled.k5
train70.scaled.k5

bfn.scaled.k7
train50.scaled.k7
train60.scaled.k7
train70.scaled.k7

# totts gets bigger as it goes from 50-70
bfn.scaled.k3$totss
train50.scaled.k3$totss
train60.scaled.k3$totss
train70.scaled.k3$totss

bfn.scaled.k5$totss
train50.scaled.k5$totss
train60.scaled.k5$totss
train70.scaled.k5$totss

bfn.scaled.k7$totss
train50.scaled.k7totss
train60.scaled.k7$totss
train70.scaled.k7$totss
# tot.withinns varies, needs to be interpreted to find right number of clusters
bfn.scaled.k3$tot.withinss
train50.scaled.k3$tot.withinss
train60.scaled.k3$tot.withinss
train70.scaled.k3$tot.withinss

bfn.scaled.k5$tot.withinss
train50.scaled.k5$tot.withinss
train60.scaled.k5$tot.withinss
train70.scaled.k5$tot.withinss

bfn.scaled.k7$tot.withinss
train50.scaled.k7$tot.withinss
train60.scaled.k7$tot.withinss
train70.scaled.k7$tot.withinss



# iClust 
# Whole data set 
iclust(bfn.scaled,nclusters=3)
iclust(bfn.scaled,nclusters=5)
iclust(bfn.scaled,nclusters=7)

iclust(train50.scaled,nclusters=3)
iclust(train50.scaled,nclusters=5)
iclust(train50.scaled,nclusters=7)

iclust(train60.scaled,nclusters=3)
iclust(train60.scaled,nclusters=5)
iclust(train60.scaled,nclusters=7)

iclust(train70.scaled,nclusters=3)
iclust(train70.scaled,nclusters=5)
iclust(train70.scaled,nclusters=7)


# Try hclust 
hc <- hclust(dist(bfn.scaled[sample(nrow(bfn.scaled),100),]),"ave")
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
wssplot(bfn.scaled,nc=3,seed=1234)
wssplot(train50.scaled,nc=3,seed=1234)
wssplot(train60.scaled,nc=3,seed=1234)
wssplot(train70.scaled,nc=3,seed=1234)
# nc = 3, 2 clusters 

wssplot(bfn.scaled,nc=5,seed=1234)
wssplot(train50.scaled,nc=5,seed=1234)
wssplot(train60.scaled,nc=5,seed=1234)
wssplot(train70.scaled,nc=5,seed=1234)
# nc = 5, 3 clusters 

wssplot(bfn.scaled,nc=7,seed=1234)
wssplot(train50.scaled,nc=7,seed=1234)
wssplot(train60.scaled,nc=7,seed=1234)
wssplot(train70.scaled,nc=7,seed=1234)
# nc = 7, 4 clusters 


# TO DO:

# *** MOVE CODE BELOW TO PROPER SECTION ***


# Scree plot: Gettting an idea of components 
bfScree <- scree(bfn.scaled,factors=TRUE)
bfScree


clusplot(pam(x=test30[1:100,],k=3,metric="euclidean",stand=FALSE))

# Principal function
bf.pca <- principal(bfn.scaled, nfactors = 2, rotate = "none")
bf.pca

colnames(bfn.scaled)
bf.prcomp <- prcomp(bfn.scaled[,c(1:12)], center = T, scale. = T)
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

# *** MOVE CODE ABOVE TO PROPER SECTION ***


#Determine # Clusters present: 
#   -Tough probem, no generic solutions
#   -Domain knowledge helpful
#   -Use the NbClust package which has 30 indices


# Obtain final cluster solution
#   -Perform final clustering with best set of parameters
#   -There is no optimal solution usually 
#   -Might help: K-means clustering <- sum of squares distance should be minimized 
#   total within-cluster sum of squares measures the compactness (i.e. goodness)
#   of the clustering and we want it to be as small as possible 


#  Visualize the results 
#   -Hierarchical clustering usually displayed as a dendrogram
#   -Partitioning clustering usually displayed as a bivariate cluster plot 
#   -You may have to select pairs of variables to plot (latter case)

#  Interpret the clusters 
#   -Examining the clusters, find what is common and what is not
#   -Requires domain knowledge 
#   -Obtain summary statistics 
#   -If categorical variables, look at modes and/or category distributions 

#  Validate the clusters 
#   -Are these groupings represented in the real world
#   in some way?
#   -If a different clustering method was used, would
#    the same clusters be obtained?
#   -Try the fpc, clv and clValid packages 









