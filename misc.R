
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



# Lecture 4 R Functions Experimentation
summary(bf)
psych::describe(bf,na.rm=TRUE)
str(bf)

