#################################
# QUESTION 3
#################################
rm(list = ls())
library(class)
library(Metrics)
library(plotly)
library(rpart.plot)
library(MASS)
library(rpart)
library(gbm)
library(proxy)
#install.packages("vegan")
library(fossil)
library(foreign)
library (cluster)
library (vegan)
set.seed(1234)



#a.) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total) and 50 variables.

#Class A
dataA = data.frame()
for(i in c(1:20))
{
  dataA = rbind(dataA[,],c(rnorm(50,5,2)))
}
colnames(dataA) = paste0(rep("a",50),c(1:50))
dataA$Class = "A"

#Class B
dataB = data.frame()
for(i in c(1:20))
{
  dataB = rbind(dataB[,],c(rnorm(50,15,2)))
}
colnames(dataB) = paste0(rep("a",50),c(1:50))
dataB$Class = "B"

#Class C
dataC = data.frame()
for(i in c(1:20))
{
  dataC = rbind(dataC[,],c(rnorm(50,25,2)))
}
colnames(dataC) = paste0(rep("a",50),c(1:50))
dataC$Class = "C"

dataset<- rbind(dataA,dataB,dataC)

#b.) Perform k-means clustering of the observations with K=3. Using the rand index and adjusted rand index, assess how well do the clusters that you obtained in K-means clustering compare to the true labels?

km = kmeans(dataset[,1:50],centers = 3,nstart = 20)

table(km$cluster, dataset$Class)

rand.index(km$cluster, as.numeric(as.factor(dataset$Class)))
adj.rand.index(km$cluster, as.numeric(dataset$Class))

s <- silhouette(km$cluster, dist(dataset[,1:50]))
plot(s)


#c.) Using silhouette plots, select the optimal number of clusters.

avg = c()
for (k in c(2:10)) 
{
  km = kmeans(dataset[,1:50],centers = k,nstart = 20)
  s <- silhouette(km$cluster, dist(dataset[,1:50]))
  avg=c(avg,mean(s[,3]))
}
plot(c(2:10),avg,type = 'b')

#optimal number of clusters can be identified by the highest value in the plot.

#d.) Using the gap statistics, select the optimal number of clusters.

gap <- clusGap(dataset[,1:50], kmeans, nstart = 20, K.max = 10, B = 100)
plot(c(1:10),gap$Tab[,1],type="b")

#optimal number of clusters can be identified by sudden elbow shape in the plot
