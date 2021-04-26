#################################
#------- Homework 2 Script ------
# Created by: Manasa Challa
# Date: 02/26/2021
# Edited : 03/03/2021
#################################

rm(list = ls())
#install.packages("ISLR")
#install.packages("arules")
#install.packages("rattle")
#install.packages('MASS')

library(ISLR)
library(rpart)
library(gbm)
library('rpart.plot')
library('rattle')
library(MASS)  
library(arules)
setwd("~/Desktop/spring 2021/STA/Homework/HW2")


#################################
# QUESTION 1
#################################

load("marketing.RData")
View(marketing)
summary(marketing)
head(marketing)

set.seed(234)
N = dim(marketing)[1]
reference <- c()
for (i in 1:5){
  x <- marketing[ ,i]
  uni <- na.omit(unique(x))
  temp <- sample(uni, N, replace = TRUE)
  reference <- cbind(reference, temp)
}
colnames(reference) <- colnames(marketing)[1:5]

combinedData <- rbind(marketing[,1:5], reference)
rm(referenceSample, trainSample)
str(combinedData)

unique(marketing$Sex)
male <- which(marketing$Sex == 1)
marketing$Sex[male] <- "Male"
marketing$Sex[-male] <- "Female"
marketing$Sex <- as.factor(marketing$Sex)

marketing$Marital <- as.factor(marketing$Marital)

dats <- rep(1, N)
ref <- rep(0, N)
Y <- c(dats, ref)

combinedData$final<-Y 

#classification tree
Tree<- rpart.control(minbucket = 8, minsplit = 100, xval=10, cp=0.02, maxdepth=4)
fit <- rpart(final~., data=combinedData, method='class', control=Tree)
rules<-rpart.rules(fit)
rules
summary(fit)
rpart.plot(fit)

prediction<-predict(fit,data=combinedData)

summary(prediction)
prediction
 
#################################
# QUESTION 2
################################# 
  

data(Boston) 
head(Boston)
summary(Boston)

boston_dataset <- Boston

  
# A) Visualizing data and transforming the data into a binary incidence matrix  

hist(Boston$medv, xlab="MEDV" ,col="blue")
hist(Boston$crim ,xlab="CRIM" ,col="blue")
hist(Boston$zn, xlab="ZN" ,col="blue")
hist(Boston$indus, xlab="INDUS" ,col="blue")
hist(Boston$chas, xlab="CHAS" ,col="blue")
hist(Boston$nox, xlab="NOX" ,col="blue")
hist(Boston$rm, xlab="RM" ,col="blue")
hist(Boston$age, xlab="AGE" ,col="blue")
hist(Boston$dis, xlab="DIS" ,col="blue")
hist(Boston$rad, xlab="RAD" ,col="blue")
hist(Boston$tax, xlab="TAX" ,col="blue")
hist(Boston$ptratio, xlab="PTRATIO" ,col="blue")
hist(Boston$black, xlab="BLACK" ,col="blue")
hist(Boston$lstat, xlab="LSTAT" ,col="blue")

?Boston

Boston[["crim"]] <- ordered(cut(Boston[["crim"]], c(0,0.1,3.61,5,90)), labels = c('VeryLowCrimeRate','LowCrimeRate','ModerateCrimeRate','HighCrimeRate'))
Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(0,20,50,100)), labels = c('SmallResidence','ModerateResidence','LargeResidence'))
Boston[["indus"]] <- ordered(cut(Boston[["indus"]], c(0,5,9,20,30)), labels = c(' VeryLowProportion','LowProportion','ModerateProportion','High Proportion'))
Boston[["chas"]] <- ordered(cut(Boston[["chas"]], c(0, 0.5, 1), labels=c('Unbound', 'Bounds')))
Boston[["nox"]] <- ordered(cut(Boston[["nox"]], c(0, 0.4490, 0.6240, 0.8710), labels=c('LowConcentration', 'MediumConcentration', 'HighConcentration')))
Boston[["rm"]] <- ordered(cut(Boston[["rm"]], c(1,5,7,10)), labels = c('Few','Moderate','Many'))
Boston[["age"]] <- ordered(cut(Boston[["age"]], c(0,20,49,100)), labels = c('Few','Moderate','Many'))
Boston[["dis"]] <- ordered(cut(Boston[["dis"]], c(0,2.1,5.1,15)), labels = c('NearBy','AverageDistance','farAway'))
Boston[["rad"]] <- ordered(cut(Boston[["rad"]], c(0,5,10,25)), labels = c('EasilyAccessible','ModeratelyAccessible','NotAccessible'))
Boston[["tax"]] <- ordered(cut(Boston[["tax"]], c(0,250,450,1000)), labels = c('LowTax','AverageTax','HighTax'))
Boston[["ptratio"]] <- ordered(cut(Boston[["ptratio"]], c(10,15,17,25)), labels = c('SmallRatio','ModerateRatio','HighRatio'))
Boston[["black"]] <- ordered(cut(Boston[["black"]], c(0, 375.36, 396.23, 400)), labels=c('FewBlacks', 'ModerateBlacks', 'ManyBlacks'))
Boston[["lstat"]] <- ordered(cut(Boston[["lstat"]], c(0, 6.95, 16.95,40), labels=c('LowPercent', 'AveragePercentage', 'HighPercentage')))
Boston[["medv"]] <- ordered(cut(Boston[["medv"]], c(1,20,35,60)), labels = c('Cheap','Moderate','Expensive'))

summary(Boston)

bmatrix <- as(Boston, 'transactions')
summary(bmatrix)

# B) Visualize the data using the itemFrequencyPlot in the “arules” package. Apply the apriori algorithm.

itemFrequencyPlot(bmatrix, support=0.05, cex.names=0.8)

rules <- apriori(bmatrix, parameter = list(support = 0.02, confidence = 0.8))
summary(rules)

# C) A student is interested is a low crime area, but wants to be as close to the city as possible (as measured by “dis”). What can you advise on this matter through the mining of association rules?

CloseToCity <- subset(rules, subset = rhs %in% "dis=NearBy" & lift >1.5)
LowCrime <- subset(rules, subset = rhs %in% "crim=VeryLowCrimeRate" & lift >1.5)
summary(CloseToCity)
summary(LowCrime)

LowCrimeNearCity <- subset(rules, subset = rhs %in% "crim=LowCrimeRate" & lhs %in% "dis=NearBy" & lift >1.5)
summary(LowCrimeNearCity)

# D) Schools with low pupil-teacher ratios

LowPupilTeacherRatio <- subset(rules, subset = rhs %in% "ptratio=SmallRatio" & lift >1.2)

summary(LowPupilTeacherRatio)
inspect(head(sort(LowPupilTeacherRatio, by ='lift'),n = 6))

################################################ EXTRA CREDIT ###############################################

# Regression model to solve part D
regression <- lm(ptratio~., data=boston_dataset)
summary(regression)
