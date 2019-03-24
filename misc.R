
colnames(bf)
bf
# User_ID
# Which user made the biggest purchase and how much was it? 
uidVsPurchase <- dplyr::select(bf,"User_ID","Purchase")
uidVsPurchase

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colSort <- function(data, ...) sapply(data, sort, ...)
colMax(uidVsPurchase)
# Max purchase was 23959 by user 1006040

# Product_ID 
# Which product ID had the greatest revenue and how much revenue did it generate?
pidVsRevenue <- dplyr::select(bf,"Product_ID","Purchase")
pidVsRevenue
colMax(pidVsRevenue)
# Max purchases was 23959 for product ID P0099942

# Gender 
# Which gender contributed more to the revenue and how much did they contribute?
genderVsRevenue <- dplyr::select(bf,"Gender","Purchase")
genderVsRevenue <- split(genderVsRevenue,genderVsRevenue$Gender)
genderVsRevenue 
sum(genderVsRevenue$M$Purchase)
# 1,506,076,260
# Males contributed more 
sum(genderVsRevenue$F$Purchase)
# 409,569,775

# Age 
# Which age group contributed the most to the revenue and how much?
ageVsRevenue <- dplyr::select(bf,"Age","Purchase")
ageVsRevenue <- split(ageVsRevenue,ageVsRevenue$Age)
summary(ageVsRevenue)

ageVsRevenue <- c(sum(ageVsRevenue$`0-17`$Purchase),sum(ageVsRevenue$`18-25`$Purchase),sum(ageVsRevenue$`26-35`$Purchase),sum(ageVsRevenue$`36-45`$Purchase),sum(ageVsRevenue$`46-50`$Purchase),sum(ageVsRevenue$`51-55`$Purchase),sum(ageVsRevenue$`55+`$Purchase))
names(ageVsRevenue) <- c("0-17","18-25","26-35","36-45","46-50","51-55","55+")
max(ageVsRevenue)
# 26-35 spent the most: 765778036

# Occupation
# Which occupation contributed the most to the revenue and how much?
# City_Category 
# Which city contributed the most to the revenue and how much? 
# Stay_In_Current_City_Years
# Is there a correlation between revenue and the number of years a person stays in a city?
# Marital_Status 
# Do married or single people spend more?
# Product_Category_1  
# Product_Category_2 
# Product_Category_3 
# ^ Which product category generated the most revenue?
# Purchase 

# Predictive Model examples 

# Will * insert group * purchase * insert product id *? 

# Decision Model examples 

# Do single people who just moved to city A get married within 3 years? 

# Association Model examples 

# Is there a correlation between revenue and the number of years a person stays in a city?

uids <- bf[,1]
pids <- bf[,2]
gender <- bf[,3]
age <- bf[,4]
occupations <- bf[,5]
cities <- bf[,6]
yearsStayed <- bf[,7]
maritalStatus <- bf[,8]
prod1 <- bf[,9]
prod2 <- bf[,10]
prod3 <- bf[,11]
purchases <- bf[,12]

mean(purchases)

purchases <- na.omit(purchases)
purchases
summary(purchases)

summary(bf)


# Convert NAs to 0s 
bf <- na.omit(bf)
bf
bf$Marital_Status[is.na(bf$Marital_Status)<-0]

# Z Score normalization
zscore <- function(x) {(x-mean(x))/sd(x)}
zscore(c(10,20,30,40,50))

# K means purchases 
purchases <- bf$Purchase
purchases.k3 <- kmeans(purchases,3)
purchases.k3

purchases.k3$totss
purchases.k3$centers
purchases.k3$iter


purchases.k4 <- kmeans(purchases,4)
purchases.k4

purchases.k4$totss
purchases.k4$centers
purchases.k4$iter

purchases.k5 <- kmeans(purchases,5)
purchases.k5

purchases.k5$totss
purchases.k5$centers
purchases.k5$iter




# Scale the data (Scaled purchases) 
bf.data.scaled <- scale(bf.data$Purchase)


userIDvsPurchase.scaled <- scale(userIDvsPurchase)
uidvsp <- userIDvsPurchase.scaled

str(bf.data)
str(uidvsp)

uidvsp <- na.omit(uidvsp)

bf$Gender <- as.numeric(bf$Gender)
bf$Gender <- na.omit(bf$Gender)
bf$Gender

# Make table with UserID, gender, age, city category, purchase 
bf.data <- dplyr::select(bf,User_ID,Gender,Age,City_Category,Purchase)
bf.data <- na.omit(bf.data)
bf.data
sapply()

iclust(bf.data)

userIDvsPurchase <- dplyr::select(bf,User_ID,Purchase)

psych::describe(bf.data,na.rm=TRUE)
summary(bf.data)
str(bf.data)





# Clusters for UserID
userids <- bf.data$User_ID
userids <- na.omit(userids)
userids
uidClust <- kmeans(userids,centers=2,nstart=25)
uidClust
uidvspClust <- kmeans(uidvsp,centers=2,nstart=25)
uidvspClust


# Clusters for Gender 
genders <- bf.data$Gender
genders <- na.omit(genders)
genders
genderClust <- kmeans(userids,centers=2,nstart=25)
genderClust
# Clusters for Age 
ages <- bf.data$Age
ages <- na.omit(ages)
ages


# Below doesn't work
# agesClust <- kmeans(ages,center=2,nstart=1)
iclust(bf.data,nclusters=4)


# Statistical Machine Learning: refers to analytics models that learn from the data as the 
# data changes 

# Training data: data used to build the models by associating predictors (or rules) with
# outcomes: e.g. spam filtering 

# Test data: data used to test the models by evaluating if the models predicted new data 
# outcomes correctly 

# Unsupervised learning: no target group specified: ex: clustering, co-occurrence grouping,
# profiling, density estimates, pattern discovery, customer categorization 

# Supervised learning: specific target specified - ex: which loan clients are more likely 
# to default on their loan? regressionl; predictions 

# Training / Testing Sets 


# hclust 
hc <- hclust(dist(bfn[1:100,]), "ave")
plot(hc)
plot(hc, hang = -1)




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




NYC



USArrests

summary(USArrests)

describe(USArrests)

# Data must be standardized (i.e. scaled) to make
# variables comparable.

# Why? Some values too large compared to others will
# skew the analysis and the results.

# Standardization consists of transforming the variables
# such that they have mean zero and standard deviation one 

df <- USArrests 
df <- na.omit(df)
df[1:10,]
df <- scale(df)
df[1:10,]

k2 <- kmeans(df,centers=2,nstart=25)
str(k2)

k2


k3 <- kmeans(df,centers=3,nstart=25)
k3


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

wssplot(df,nc=4,seed=1234)


# k-means: issues
# Very sensitive to the initial selection of centroids:
#   Do many runs of k-Means, each with different initial centroids.
# Seed the centroids using a better method than random.  (e.g. Farthest-first sampling)
# Must manually choose k
# Pick an initial k: Suggested is sqrt(n) where n is # points
# But, for very large n, leads to very large k, which is/maybe absurd!
#   Real-life data is almost always messy.
# Lots of noise obscuring the signal, large dispersion.

# Psycch:: Cluster Analysis
iclust(mtcars,nclusters=2)
mtcars

iclust(mtcars,nclusters=4)

plot(USArrests)

install.packages("cluster")
library(cluster)

clusplot(pam(x=mtcars,k=3,metric="euclidean",stand=FALSE))

b2 <- principal(df,nfactors=2,rotate="none")
b2
b3 <- principal(df,nfactors=2,rotate="none")
b3

# Scree Plot: Getting an idea of components 
usascree<-scree(USArrests,factors=TRUE)
usascree


describe(USArrests)
psych::describe(USArrests,na.rm=TRUE)




# Lecture 5 Predictive Analytics 
# Predictive Models: identify possible outcomes by analyzing data

# Decision Models: what are the possible decisions that will yield a net benefit?
# -Need to have good domain knowledge 

# Association Models: based on underlying relationships and associations in the data

BlackFriday <- read_excel("BlackFriday.xlsx")
black_friday

