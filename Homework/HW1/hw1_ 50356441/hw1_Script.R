#################################
#------- Homework 1 Script ------
# Created by: Manasa Challa
# Date: 02/05/2021
# Edited : 02/06/2021
#################################

#install.packages("ISLR")
library(ISLR)
setwd("~/Desktop/spring 2021/STA/Homework/HW1")

########COLLEGE DATASET##########

data("College")
head(College)
summary(College) #numerical summary of the variables in the dataset
pairs(College[ ,2:18],main="Scatter plots of all pairs of continuous variables") #scatterplot of the continuous variables in the data set
College$Elite <- College$Top10perc > 50 #creating a qualitative variable
table(College$Elite) #To figure out how many Elite schools there are
table(College[College$Private=="Yes",]$Elite) #To figure out how many of the Elite schools are private
#Do elite schools tend to have higher graduation rates? 
#Answer :Yes
plot(College$Elite,College$Grad.Rate, xlab ="Elite and Non-Elite schools", ylab="Graduation Rate") 
aggregate(College$Grad.Rate,list(College$Elite),FUN=mean)

###########AUTO DATASET##########

data("Auto")
head(Auto)
summary(Auto)
sum(is.na(Auto)) #To check number of missing values 

#What variables are numerical (continuous) or factors (categorical)?
class(Auto$mpg) #numeric
class(Auto$cylinders) #numeric
class(Auto$displacement) #numeric
class(Auto$horsepower) #numeric
class(Auto$weight) #numeric
class(Auto$acceleration) #numeric
class(Auto$year) #numeric
class(Auto$origin) #numeric but can be taken as factor as it has only 3 values
class(Auto$name) #factor
sapply(Auto, class) #can be written in this way as well

#mean and standard deviation for each continuous variable in the data
sapply(Auto[1:7],mean) #I have taken Auto$origin as factor
sapply(Auto[1:7],sd)

#Removing 5th through 55th observations and calculating the range, mean and standard deviation
subset <- Auto[-c(5:55),]
sapply(subset[1:7],range)
sapply(subset[1:7],mean)
sapply(subset[1:7],sd)

#In the full Auto dataset, are there any variables you would consider removing, or representing differently? Why?
# Ans : Like mentioned above, the variable origin has only three values associated with it so it can be considered as a factor. Therefore I considered it as a factor for this assignment. Name can be removed as its a factor.

#In the full Auto dataset, graphically explore the relationships between the variables in the data set.
graphics.off()
pairs(Auto[1:7], main="Scatter plots of all pairs of continuous variables")
plot(Auto$mpg,Auto$weight,main = "MPG Vz Weight of the car",xlab = "mpg", ylab = "weight")
plot(factor(Auto$origin),Auto$mpg,names=(c("American","European","Japanese")),main = "Manufacturer and the mpg of the car",xlab = "Origin", ylab = "mpg") 
plot(as.factor(Auto$cylinders),Auto$mpg,main = "mpg Vz. cylinders",xlab = "Cylinders", ylab = "mpg")
plot(Auto$cylinders,Auto$displacement,main = "Cylinders Vz Displacement", xlab="cylinders", ylab="displacement")

#In the full Auto dataset, consider the variable mpg. You are going to create a new categorical variable for MPG, which has the categories: {low, med, high}. Call this variable “my_mpg”, and create a new_Auto dataset, which contains all of the Auto variables, and your new variable “my_mpg”. Save the dataset as an *.RData file and submit it with your assignment.
new_Auto <- Auto
new_Auto$my_mpg[new_Auto$mpg >= 9 & new_Auto$mpg <= 21] <- "low"
new_Auto$my_mpg[new_Auto$mpg >= 22 & new_Auto$mpg <= 34] <- "mid"
new_Auto$my_mpg[new_Auto$mpg >= 35 & new_Auto$mpg <= 47] <- "high"

save(new_Auto, file="newAuto.RData")

