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
train50$Product_ID

str(train50)
str(test50)

str(train60)
str(test40)

str(train70)
str(test30)



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
# Occupation
bfOccupation <- na.omit(bfOccupation)
# Martial Status
bfMaritalStatus <- na.omit(bfMaritalStatus)
# Product ID 
bfProductID <- na.omit(bfProductID)


# Scale the data 
bfOccupation.scaled <- scale(bfOccupation)
bfMaritalStatus.scaled <- scale(bfMaritalStatus)
bfProductID.scaled <- scale(bfProductID)

# Min-Max Normalization
normalize <- function(x) {(((x-min(x))/max(x)-min(x)))}
# Apply normalize function 
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


clusplot(pam(x=test30[1:100,],k=3,metric="euclidean",stand=FALSE))

# Principal function
b2 <- principal(bf,nfactors=2,rotate="none")
b2
# Root mean square = 0.08 (Measure of difference 
# between observed and expected values)

# Scree plot: Gettting an idea of components 
bfScree <- scree(bf,factors=TRUE)
bfScree

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







# Plot Gender Vs. Purchase 

# Plot Age Vs. Purchase 

# Plot Occupation vs. Purchase 

# Plot City_Category vs. Purchase 

# Plot Stay_In_Current_City_Years vs. Purchase 

# Plot Marital_Status vs. Purchase 

# Plot Product_Category_1 vs. Purchase 

# Plot Product_Category_2 vs. Purchase 

# Plot Product_Category_3 vs. Purchase 



# Stuff from Exploratory Data Analysis Page 

train50$User_ID <- as.factor(train50$User_ID)
train50$Product_ID <- as.factor(train50$Product_ID)
train50$Marital_Status <- as.factor(ifelse(train50$Marital_Status == 1, 'Married', 'Single'))
train50$Age <- as.factor(train50$Age)
train50$Gender <- as.factor(ifelse(train50$Gender==0, 'Male', 'Female'))
train50$Occupation <- as.factor(train50$Occupation)
train50$City_Category <- as.factor(train50$City_Category)
train50$Stay_In_Current_City_Years <- as.factor(train50$Stay_In_Current_City_Years)

test50$User_ID <- as.factor(test50$User_ID)
test50$Product_ID <- as.factor(test50$Product_ID)
test50$Marital_Status <- as.factor(ifelse(test50$Marital_Status == 1, 'Married', 'Single'))
test50$Age <- as.factor(test50$Age)
test50$Gender <- as.factor(ifelse(test50$Gender==0, 'Male', 'Female'))
test50$Occupation <- as.factor(test50$Occupation)
test50$City_Category <- as.factor(test50$City_Category)
test50$Stay_In_Current_City_Years <- as.factor(test50$Stay_In_Current_City_Years)

#str(train50) 
#str(test50)

EDA_Distinct <- distinct(train50, User_ID, Age, Gender, Marital_Status, Occupation, City_Category, Stay_In_Current_City_Years)
#str(EDA_Distinct)
head(EDA_Distinct)
head(train50$User_ID,40)
head(test50$User_ID,40)

userIDCount <- as.data.frame(table(train50$User_ID))
names(userIDCount) <- c("User_ID","User_Purchase_Count")
head(userIDCount)

train50 <- merge(x = test50, y = userIDCount, by = "User_ID", all.x = TRUE)
str(train50)

test50 <- merge(x = test50, y = userIDCount, by = "User_ID", all.x = TRUE)
rm(userIDCount)
'%!in%' <- function(x,y)!('%in%'(x,y))
test50[is.na(test50$User_Purchase_Count), "User_Purchase_Count"] <- 1
class(test50$User_Purchase_Count)
str(test50)

test50$User_Purchase_Count <- as.integer(test50$User_Purchase_Count)
EDA_Distinct <- distinct(train50, User_ID, Age, Gender, Marital_Status, Occupation, City_Category, Stay_In_Current_City_Years, User_Purchase_Count)
d1 <- summary(EDA_Distinct$User_Purchase_Count)

p1 <- ggplot(EDA_Distinct, aes(x=User_Purchase_Count)) +geom_density(fill="red", col="black", alpha=0.80) + annotate(geom = "text", x = 6, y = 0.0125, label = "Min")  + annotate(geom = "text", x = 24, y = 0.013, label = "1st Qu.") + annotate(geom = "text", x = 50, y = 0.0125, label = "Median") + annotate(geom = "text", x = 90, y = 0.013, label = "Mean") + annotate(geom = "text", x = 112, y = 0.0125, label = "3rd Qu.") + annotate(geom = "text", x = 1015, y = 0.0125, label = "Max") + geom_vline(xintercept = c(6, 26, 54, 93.37, 117, 1026), size = 0.2, col = 'black') #+ lims(x = )

p1 

p2 <- ggplot(EDA_Distinct, aes(x=User_Purchase_Count)) +geom_histogram(fill="red", col="black", alpha=0.80) 

p2

p3 <- ggplot(EDA_Distinct,aes(x= Age,y=User_Purchase_Count, fill=Age)) + geom_boxplot() + facet_grid(Gender~Marital_Status) + labs(x="Age",y="Customer Purchase Count")

p3 

p4 <- ggplot(EDA_Distinct,aes(x= Occupation,y=User_Purchase_Count, fill=Occupation)) + geom_boxplot() + facet_grid(Gender~Marital_Status) + labs(x="Occupation",y="Customer Purchase Count")

p4

# p5 <- ggplot(EDA_Distinct,aes(x=Age,y=User_Purchase_Count,fill=Stay_In_Current_City_Years))+geom_boxplot()+facet_grid(City_Category~ Stay_In_Current_City_Years) + labs(x="Age",y="Customer Purchase Count")
# p5

# p5i <- ggplot(EDA_Distinct,aes(x=Age,y=User_Purchase_Count,fill=Stay_In_Current_City_Years))+geom_boxplot()+facet_grid( Stay_In_Current_City_Years ~ City_Category) + labs(x="Age",y="Customer Purchase Count")
# p5i 

p6 <- ggplot(EDA_Distinct,aes(x=Age,y=User_Purchase_Count,fill=Marital_Status))+geom_boxplot()+facet_grid(Gender~City_Category) + scale_fill_manual(values=c("tan4","limegreen"))  + labs(x="Age",y="Customer Purchase Count") + theme(text = element_text(size=5))
p6


train50$Product_ID




test50$Product_ID



head(train50$Product_ID,15)
ProductIDCount <- as.data.frame(table(train50$Product_ID))
names(ProductIDCount) <- c("Product_ID","Product_Sold_Count")
head(ProductIDCount)


train50$Product_ID
train50 <- merge(x = train50, y = ProductIDCount, by = "Product_ID", all.x = TRUE)
str(train50)
d2 <- summary(train50$Product_Sold_Count)

p7 <- ggplot(train50, aes(x="Product_Sold_Count")) +geom_density(fill="red", col="black", alpha=0.80) + annotate(geom = "text", x = 1, y = 0.004, label = "Min")  + annotate(geom = "text", x = 174, y = 0.00385, label = "1st Qu.") + annotate(geom = "text", x = 357, y = 0.004, label = "Median") + annotate(geom = "text", x = 450, y = 0.00385, label = "Mean") + annotate(geom = "text", x = 620, y = 0.004, label = "3rd Qu.") + annotate(geom = "text", x = 1880, y = 0.004, label = "Max") + geom_vline(xintercept = c(1,174,357,450.5,620,1880), size = 0.2, col = 'black') 
p7

head(ProductIDCount[order(-ProductIDCount$Product_Sold_Count),])
tail(ProductIDCount[order(-ProductIDCount$Product_Sold_Count),])

test50 <- merge(x = test50, y = ProductIDCount, by = "Product_ID", all.x = TRUE)
rm(ProductIDCount)
test50[is.na(test50$User_Purchase_Count), "User_Purchase_Count"] <- 1
test50$User_Purchase_Count <- as.integer(test50$User_Purchase_Count)
head(train50$Gender); head(train50$Marital_Status)

d3 <- table(EDA_Distinct$Gender, EDA_Distinct$Marital_Status)
d3


p8 <- ggplot(EDA_Distinct, aes(x=Gender, fill= Marital_Status)) + geom_bar(position = "dodge") + ggtitle("") +  labs(x="Gender",y="No. of distinct Sales") + annotate(geom = "text", x = 0.775, y = 619, label = "719")   + annotate(geom = "text", x = 1.225, y = 847, label = "947") + annotate(geom = "text", x = 1.775, y = 1655, label = "1755") + annotate(geom = "text", x = 2.225, y = 2370, label = "2470") + scale_fill_manual(values=c("tan4","limegreen")) 
p8
d3; p8

head(train50, 10)
d4 <- table(EDA_Distinct$Age)
d4

d5 <- table(EDA_Distinct$Marital_Status, EDA_Distinct$Gender, EDA_Distinct$Age)
d5

p10 <- ggplot(EDA_Distinct, aes(x= Age,fill= Gender, col= Marital_Status)) + geom_bar(position = "dodge", size=1.25) +  labs(x="Age Group",y="No. of distinct buyer") + scale_fill_manual(values=c("hotpink", "royalblue")) + scale_color_manual(values=c("tan4","limegreen")) + ggtitle("") 
p10


p11 <- ggplot(EDA_Distinct,aes(x=Age,fill=Marital_Status))+geom_bar(position = "dodge")+facet_grid(Gender~.) + scale_fill_manual(values=c("tan4","limegreen"))
p11


head(train50$Occupation, 10)
d6 <- table(EDA_Distinct$Occupation)
d6
d7 <- table(EDA_Distinct$Gender, EDA_Distinct$Occupation)
d7
p12 <- ggplot(EDA_Distinct, aes(x=Occupation, fill=Gender)) + geom_bar( col="black") + ggtitle("") +  labs(x="Occupation",y="No. of distinct people") + scale_fill_manual(values=c("hotpink", "royalblue"))
p12

d8 <- table(EDA_Distinct$Marital_Status, EDA_Distinct$Occupation)

p13 <- ggplot(EDA_Distinct, aes(x=Occupation, fill=Marital_Status)) + geom_bar( col="black") + ggtitle("") +  labs(x="Occupation",y="No. of distinct people") + scale_fill_manual(values=c("tan4","limegreen"))
p13

p14 <- ggplot(EDA_Distinct,aes(x=Occupation, fill=Age))+geom_bar()+facet_grid(Gender~Marital_Status)
p14

head(train50$Stay_In_Current_City_Years, 10); head(train50$City_Category, 10)

d9 <- table(EDA_Distinct$City_Category, EDA_Distinct$Stay_In_Current_City_Years)
d9

p15 <- ggplot(EDA_Distinct, aes(x=Stay_In_Current_City_Years, fill=City_Category)) + geom_bar( col="black") + ggtitle("") +  labs(x="Stay in Current City (Years)",y="No. of distinct people")
p15

p16 <- ggplot(EDA_Distinct,aes(City_Category,fill=Age))+geom_bar()
p16

# p17 <- ggplot(EDA_Distinct,aes(x=Age,fill=Stay_In_Current_City_Years))+geom_bar()+facet_grid(City_Category~ Stay_In_Current_City_Years)
# p17

p18 <- ggplot(EDA_Distinct,aes(x=Age,fill=Marital_Status))+geom_bar()+facet_grid(Gender~City_Category) + scale_fill_manual(values=c("tan4","limegreen"))
p18

head(as.factor(train50$Product_Category_1))
head(as.factor(train50$Product_Category_2))
head(as.factor(train50$Product_Category_3))


train50$Product_Category_1 <- as.factor(train50$Product_Category_1)
train50$Product_Category_2 <- as.factor(train50$Product_Category_2)
train50$Product_Category_3 <- as.factor(train50$Product_Category_3)


train50$Product_Category_2 <- factor(train50$Product_Category_2, levels=c(levels(train50$Product_Category_2), "0"))
train50[is.na(train50$Product_Category_2), "Product_Category_2"] <-"0"

train50$Product_Category_3 <- factor(train50$Product_Category_3, levels=c(levels(train50$Product_Category_3), "0"))
train50[is.na(train50$Product_Category_3), "Product_Category_3"] <-"0"



train50$Cat_1 <- as.factor(ifelse((train50$Product_Category_1=='1' | train50$Product_Category_2=='1' | train50$Product_Category_3=='1'), 1,0))
for(i in 2:20)
{
  assign(paste("Cat_", as.character(i), sep=""),as.factor(ifelse((train50$Product_Category_1==i | train50$Product_Category_2==i | train50$Product_Category_3==i), 1,0)))
}
train50 <- cbind(train50, Cat_2, Cat_3, Cat_4, Cat_5, Cat_6, Cat_7, Cat_8, Cat_9, Cat_10, Cat_11, Cat_12, Cat_13, Cat_14, Cat_15, Cat_16, Cat_17, Cat_18, Cat_19, Cat_20)


to_drop <- c("Product_Category_1", "Product_Category_2", "Product_Category_3")
train50 <- train50[,!names(train50)%in% to_drop]
rm(Cat_2, Cat_3, Cat_4, Cat_5, Cat_6, Cat_7, Cat_8, Cat_9, Cat_10, Cat_11, Cat_12, Cat_13, Cat_14, Cat_15, Cat_16, Cat_17, Cat_18, Cat_19, Cat_20)


dim(train50)

as.matrix(sapply(train50, function(x) class(x)))


test50$Product_Category_1 <- as.factor(test50$Product_Category_1)
test50$Product_Category_2 <- as.factor(test50$Product_Category_2)
test50$Product_Category_3 <- as.factor(test50$Product_Category_3)


test50$Product_Category_2 <- factor(test50$Product_Category_2, levels=c(levels(test50$Product_Category_2), "0"))
test50[is.na(test50$Product_Category_2), "Product_Category_2"] <-"0"

test50$Product_Category_3 <- factor(test50$Product_Category_3, levels=c(levels(test50$Product_Category_3), "0"))
test50[is.na(test50$Product_Category_3), "Product_Category_3"] <-"0"


for(i in 1:20)
{
  assign(paste("Cat_", as.character(i), sep=""),as.factor(ifelse((test50$Product_Category_1==i | test50$Product_Category_2==i | test50$Product_Category_3==i), 1,0)))
}
test50 <- cbind(test50, Cat_1, Cat_2, Cat_3, Cat_4, Cat_5, Cat_6, Cat_7, Cat_8, Cat_9, Cat_10, Cat_11, Cat_12, Cat_13, Cat_14, Cat_15, Cat_16, Cat_17, Cat_18, Cat_19, Cat_20)


to_drop <- c("Product_Category_1", "Product_Category_2", "Product_Category_3")
test50 <- test50[,!names(test50)%in% to_drop]
rm(Cat_1, Cat_2, Cat_3, Cat_4, Cat_5, Cat_6, Cat_7, Cat_8, Cat_9, Cat_10, Cat_11, Cat_12, Cat_13, Cat_14, Cat_15, Cat_16, Cat_17, Cat_18, Cat_19, Cat_20)

dim(test50)
as.matrix(sapply(test50, function(x) class(x)))
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,i,to_drop)



str(train50); str(test50)
train50

sapply(train50, function(x) sum(is.na(x))); sapply(test50, function(x) sum(is.na(x)))
