#################################
#------- Demo Script ------
# Created by: Manasa Challa
# Date: 02/05/2021
#################################
rm(list = ls()) # clear the memory
setwd("~/Desktop/spring 2021/STA/Homework")




#Load Libraries
#install.packages("ISLR")
#install.packages("plotly")
library("geneplotter")
library("ISLR")
library("plotly")




# what is in the ISLR package
#ls("package:ISLR")




# focus on the OJ dataset
?OJ
head(OJ) #first five rows
class(OJ) #this is a dataframe




# playing with the dataframe
dim(OJ) #dimensions of matrix(rows x cols)
OJ[1:10,1:5] #grab rows and cols of interest
OJ[1:10,c(1:5,8,9,13)] #grab rows and cols of interest
class(OJ[ ,1]) #first row is purchase and it is a factor 
class(OJ$Purchase) #first row is purchase and it is a factor 
class(OJ[ ,2]) #second row is WeekofPurchase and it is a factor 
class(OJ$WeekofPurchase) #second row is WeekofPurchase and it is a factor 
colnames(OJ) #column names
OJ$Purchase #values of purchase
mean(OJ$WeekofPurchase) 



########################################
# how to manipulate and change a dataset
########################################
graphics.off()
quartz()
hist(OJ$WeekofPurchase)


x11()
hist(OJ$PriceMM, main = "My histogram of price", xlab = "Price", ylab = "count")
savepdf("my_histogram")

x11()
hist(log(OJ$WeekofPurchase), main = "My histogram of price", xlab = "Price", ylab = "count")

clean_OJ <- OJ
head(clean_OJ)

# add new variables
clean_OJ <- data.frame(clean_OJ, log(OJ$WeekofPurchase))
colnames(clean_OJ)
colnames(clean_OJ)[19] #19th column
colnames(clean_OJ)[19] <- "logpurch" #assigning new col name for 19th column


# turn this into a categorical variable
clean_OJ$PriceCH
med <- median(clean_OJ$PriceCH) # turn this into a categorical variable high/low
id <- which(clean_OJ$PriceCH>med) #high priced items
dim(clean_OJ[id,])
price_HL <- clean_OJ$PriceCH
price_HL[id] <- 1
price_HL[-id] <- 0

# convert to a factor (categorical variable)
as.factor(price_HL) #convert a numeric to factor
as.numeric(clean_OJ$Purchase) #convert variable to factor

head(clean_OJ)
clean_OJ[ ,4] <- price_HL

# save your dataset
write.table(clean_OJ, file = "cleanOJ.txt", sep = "\t", col.names = colnames(clean_OJ), row.names=FALSE)

# read in a dataset
dats <- read.delim("cleanOJ.txt", sep="\t", header = TRUE)

#save data in R object
save(OJ, file="mydata.RData")
load("mydata.RData")

save(OJ,clean_OJ, file="mydata.RData")
load("mydata.RData")



# how to look at tables
table(clean_OJ$Purchase)
table(clean_OJ$Purchase, clean_OJ$StoreID)


# lists structures
set.seed(123)
N <- length(clean_OJ[ ,1])
train_indis <- sample(1:N, round(2/3*N), replace = FALSE)

train <- clean_OJ[train_indis, ]
test <- clean_OJ[-train_indis, ]
dim(clean_OJ)
dim(test)
dim(train)


OJ_datasets <- list()
OJ_datasets[[1]] <- train
OJ_datasets[[2]] <- test
OJ_datasets[[3]] <- clean_OJ

names(OJ_datasets)
names(OJ_datasets) <- c("train","test","clean")

OJ_datasets[[1]][1:5, ]
OJ_datasets$train[1:5, ]







