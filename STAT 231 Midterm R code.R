set.seed(20901291) 
obs <-sample(1:10999, 500, replace = FALSE) #uniquely sample observations
dataset <-shipping_data[obs,] #create new dataset with 500 observations

library(tidyverse)
library("moments")
glimpse(dataset) #check summary of variables in dataset
dataset %>% is.na() %>% colSums()  #check for missing data


##Question 1
## (a)
#create and store frequency table to create barplot
freq<-table(dataset$Customer_rating) 
freq
#solve for % relative frequency and round to 2 decimal points
RFrequency<-round(100*freq/sum(freq),2)
RFrequency #printing out to see the values
barplot(RFrequency,ylim=c(0, 50),
        main="Bar Chart for the Customer Rating",
        ylab="% Relative Frequency", col="blue" )

## (b)
#label any blank entries NA
dataset$Gender[dataset$Gender==""]<-NA 
#remove all rows that have values of NA in them
dataset$GenderPart<-na.omit(dataset$Gender) 
counts_gender <- table(dataset$Gender, dataset$Customer_rating) 
counts_gender

## (c)
#side by side barplot where y-axis is frequency
Bar<-barplot(counts_gender, main="Customer Rating by Male vs. Female",
             xlab="Customer Rating",ylab="Frequency", 
             col=c("lightblue","red"),ylim=c(0, 100),
             legend = rownames(counts_gender), beside=TRUE)
text(x = Bar , y = counts_gender, label = counts_gender, pos = 3, 
     cex = 1.0, col = "black")

## (d)
dataset$Customer_binary_rating<-ifelse(dataset$Customer_rating > 3,1,0)
table(dataset$Customer_binary_rating)

## (e)
counts_reach_on_time <- table(dataset$Reached_on_Time, 
                              dataset$Customer_binary_rating) 
counts_reach_on_time
#solve for relative frequency and round to 2 decimal points
RFrequency1<-round(counts_reach_on_time/sum(counts_reach_on_time),2)
RFrequency1 #printing out to see the values

## (g)
n <- (500-5)
thetahat <- 191/(500-5)
y <- 191
#standard error of estimator
s<-(thetahat*(1-thetahat)/n)^0.5
# interval of values for plotting relative likelihood function
th<-seq(max(0,thetahat-4*s), min(1,thetahat+4*s),0.001)
# create function to calculate Binomial relative likelihood function
BinRLF <- function(x,y,n,thetahat) 
{(x/thetahat)^y*(1-x)^(n-y)/(1-thetahat)^(n-y)}

# plot relative likelihood function
plot(th, BinRLF(th,y,n,thetahat),
     xlab=expression(theta),
     ylab=expression(paste("R(",theta,")")),type="l",lwd=2)
title(main="Binomial Relative Likelihood Function")
abline(a=0.15,b=0,col="red",lwd=2)

#find 15% likelihood interval using uniroot
uniroot(function(x) BinRLF(x,y,n,thetahat)-0.15,lower = 0.30, upper = 0.35)$root
uniroot(function(x) BinRLF(x,y,n,thetahat)-0.15,lower = 0.40, upper = 0.45)$root


##Question 2
## (b)
fivenum(dataset$Discount_offered)
mean(dataset$Discount_offered)
sd(dataset$Discount_offered)
skewness(dataset$Discount_offered)

## (c)
n <- 500
thetahat <- mean(dataset$Discount_offered)
s<-(thetahat/n^0.5) # estimate for std error of estimator 
#determine an interval of values for plotting relative likelihood function 
th<-seq(max(0,thetahat-4*s), thetahat+4*s,0.001) 

ExponentialRLF <- function(x,n,thetahat) 
{(thetahat/x)^n*exp(n*(1-(thetahat/x)))}

plot(th,ExponentialRLF(th,n,thetahat),
     type='l', 
     lwd='3', 
     col="darkblue",
     ylab=expression(paste("R(",theta,")")),
     xlab=expression(theta))
title(main="Exponential Relative Likelihood Function")
#plot the 15% likelihood line
abline(a=0.15,b=0,col="red",lwd=2)

#find 15% likelihood interval using uniroot
uniroot(function(x) 
  ExponentialRLF(x,n,thetahat)-0.15,lower = 10.5, upper = 11.0)$root
uniroot(function(x) 
  ExponentialRLF(x,n,thetahat)-0.15,lower = 12.5, upper = 13.0)$root

## (d)
a <-qnorm((1+0.90)/2)
lower_bound <- thetahat-a*thetahat/sqrt(500)
upper_bound <- thetahat+a*thetahat/sqrt(500)
lower_bound 
upper_bound


##Question 3
## (b)
fivenum(dataset$Customer_care_calls)
mean(dataset$Customer_care_calls)
sd(dataset$Customer_care_calls)
skewness(dataset$Customer_care_calls)

## (c)
freq<-table(dataset$Customer_care_calls) 
freq
thetahat_calls <- mean(dataset$Customer_care_calls)
expected_0 <- dpois(0,thetahat_calls)*500
expected_0
expected_1 <-dpois(1,thetahat_calls)*500
expected_1
expected_2 <-dpois(2,thetahat_calls)*500
expected_2
expected_3 <-dpois(3,thetahat_calls)*500
expected_3
expected_4 <-dpois(4,thetahat_calls)*500
expected_4
expected_5 <- 500-expected_0-expected_1-expected_2-expected_3-expected_4
expected_5
total <- expected_0+expected_1+expected_2+expected_3+expected_4+expected_5
total


##Question 4
## (b)
Ship_dataset<-subset(dataset,Mode_of_Shipment=="Ship")
Ship_weight <- Ship_dataset$Weight_in_gms

fivenum(Ship_weight)
mean(Ship_weight)
sd(Ship_weight)
skewness(Ship_weight)
kurtosis(Ship_weight)

##(c)
qqnorm(Ship_weight,xlab = "G(0,1) Theoretical Quantiles",
       main = "qqplot for the Weight in gms of product shipped by Ship")
qqline(Ship_weight,col = "blue",lwd = 2)


## (e)
b <- qt((1+0.95)/2,n-1)
n <- 500
lower_bound_ship_u <- mean(Ship_weight) - (b*sd(Ship_weight))/sqrt(500)
upper_bound_ship_u <- mean(Ship_weight) + (b*sd(Ship_weight))/sqrt(500)
lower_bound_ship_u
upper_bound_ship_u

c <- qchisq((1-0.90)/2, n-1)
d <- qchisq((1+0.90)/2, n-1)

lower_bound_ship_sd <- sqrt(((n-1)*sd(Ship_weight)^2)/d)
upper_bound_ship_sd <- sqrt(((n-1)*sd(Ship_weight)^2)/c)
lower_bound_ship_sd
upper_bound_ship_sd 

## (f)
Road_dataset<-subset(dataset,Mode_of_Shipment=="Road")
Road_weight <- Road_dataset$Weight_in_gms

fivenum(Road_weight)
mean(Road_weight)
sd(Road_weight)
skewness(Road_weight)
kurtosis(Road_weight)

## (g)
qqnorm(Road_weight,xlab = "G(0,1) Theoretical Quantiles",
       main = "qqplot for the Weight in gms of product shipped by Road")
qqline(Road_weight,col = "blue",lwd = 2)

## (i)

b_r <- qt((1+0.95)/2,n-1)
n <- 500
lower_bound_road_u <- mean(Road_weight) - (b_r*sd(Road_weight))/sqrt(500)
upper_bound_road_u <- mean(Road_weight) + (b_r*sd(Road_weight))/sqrt(500)
lower_bound_road_u
upper_bound_road_u

c_r <- qchisq((1-0.90)/2, n-1)
d_r <- qchisq((1+0.90)/2, n-1)

lower_bound_road_sd <- sqrt(((n-1)*sd(Road_weight)^2)/d_r)
upper_bound_road_sd <- sqrt(((n-1)*sd(Road_weight)^2)/c_r)
lower_bound_road_sd
upper_bound_road_sd 

## (j)
Flight_dataset<-subset(dataset,Mode_of_Shipment=="Flight")
Flight_weight <- Flight_dataset$Weight_in_gms

boxplot(Ship_weight,Flight_weight, Road_weight,
        names=c("Ship","Flight", "Road"),
        main = "Weight of Package by Shipping Method",
        ylab = "Weight in grams",
        col="cyan")

##Question 5
## (b)
table(dataset$Reached_on_Time)

## (c)
thetahat_reach <- 295/500
a <- qnorm((1+0.95)/2)
lower_bound_5 <- thetahat_reach -
                 a*sqrt(((thetahat_reach)*(1-thetahat_reach))/500)
upper_bound_5 <- thetahat_reach +
                 a*sqrt(((thetahat_reach)*(1-thetahat_reach))/500)

lower_bound_5
upper_bound_5 

## (d)
probability <- 1-pbinom(22,50,295/500)
probability

## (e)
Importance_dataset<-subset(dataset,Product_importance=="high")
Importance_Reach_on_time <- Importance_dataset$Reached_on_Time
table(Importance_Reach_on_time)

##(f)
counts_importance <- table(dataset$Reached_on_Time, dataset$Product_importance) 
importance_RFrequency1<-round(counts_importance/sum(counts_importance),2)
importance_RFrequency1 #printing out to see the values

##(g)

counts_Reached_importance <- table(dataset$Reached_on_Time, 
                                   dataset$Product_importance)
counts_Reached_importance
refcounts<-prop.table(counts_Reached_importance, 2) #solving for column proportions
refcounts_round<-round(refcounts,2)
Bar<-barplot(refcounts, 
             main="Product_importance by Reached_on_Time vs. Not Reached_on_Time",
             xlab="Product_importance",ylab="Relative Frequency", 
             col=c("lightblue","red"),ylim=c(0, 1.0),
             legend = rownames(refcounts_round), beside=TRUE)
text(x =Bar , y = refcounts_round, label = refcounts_round, pos = 3, 
     cex = 1.0, col = "black")


