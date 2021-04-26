#################################
#------- Homework 3 Script ------
# Created by: Manasa Challa
# Date: 03/21/2021
# Edited : 03/29/2021
#################################

#################################
# QUESTION 1
#################################


rm(list = ls())
install.packages('lsa')
library(lsa)
library(proxy)

a= c(5,NA,3)
b= c(4,3,NA)
c= c(NA,4,1)
d= c(5,4,4)
e= c(2,2,NA)
f= c(NA,2,4)
g= c(3,1,5)
h= c(2,NA,3)


Data = data.frame(a,b,c,d,e,f,g,h)
Data

#boolean utility matrix
boolean = Data
boolean[!is.na(boolean)] = 1
boolean[is.na(boolean)] = 0
boolean

#a.)Treating the utility matrix as Boolean, compute the Jaccard distance between each pair of users.

jaccardDistance<-dist(boolean, method = "Jaccard")
jaccardDistance

#b.) Repeat Part A, but use the cosine distance.

cosineDistance<-cosine(t(as.matrix(boolean)))
cosineDistance

#c) Treat ratings of 3, 4, and 5 as 1, and ratings 1, 2, and blank as zero. Compute the Jaccard distance between each pair of users.

a= c(1,0,1)
b= c(1,1,0)
c= c(0,1,0)
d= c(1,1,1)
e= c(0,0,0)
f= c(0,0,1)
g= c(1,0,1)
h= c(0,0,1)
TableData = data.frame(a,b,c,d,e,f,g,h)
TableData

dist(TableData, method='Jaccard')

#d.) Repeat Part C, but use the cosine distance.
cosine(t(as.matrix(TableData)))

DataNorm =Data

#e) Normalize the matrix by subtracting from each nonblank entry the average value for its user.
#tableOld<-data.frame(a=factor(c(5,NA,3), levels=level),b=factor(c(4,3,NA), levels=level),c=factor(c(NA,4,1), levels=level),d=factor(c(5,4,4), levels=level),e=factor(c(2,2,NA), levels=level),f=factor(c(NA,2,4), levels=level),g=factor(c(3,1,5), levels=level),h=factor(c(2,NA,3), levels=level))

mean = rowMeans(DataNorm, na.rm = TRUE)
mean

DataNorm = DataNorm - mean
DataNorm

#f) Using the normalized matrix from Part E, compute the cosine distance between each pair of users.
DataNorm[is.na(DataNorm)] = 0
cosine(t(as.matrix(DataNorm)))

#################################
# QUESTION 2
#################################


#a.) On the basis of this dissimilarity matrix, sketch the dendrogram that results from hierarchically clustering these four observations using complete linkage.
data = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow = 4))
plot(hclust(data, method = "complete"))

#b.) Repeat (a), this time using simple linkage clustering.
plot(hclust(data, method = "single"))

#c.) Suppose that we cut the dendrogram obtained in (a) such that two clusters result. Which observations are in each cluster ?
#Ans : We obtain clusters (1,2) and (3,4).

#d.) Suppose that we cut the dendrogram obtained in (b) such that two clusters result. Which observations are in each cluster ?
#Ans : We obtain clusters (1,2,3) and (4).

#e.) Draw a dendrogram that is equivalent to the dendrogram in (a), for which two or more of the leaves are repositioned, but for which the meaning of the dendrogram is the same.
plot(hclust(data, method = "complete"), labels = c(2,1,4,3))


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

dataset<- rbind(data_A,dataB,dataC)

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
  avg=c(avg,mean(ss[,3]))
}
plot(c(2:10),avg,type = 'b')

#optimal number of clusters can be identified by the highest value in the plot.

#d.) Using the gap statistics, select the optimal number of clusters.

gap <- clusGap(dataset[,1:50], kmeans, nstart = 20, K.max = 10, B = 100)
plot(c(1:10),gap$Tab[,1],type="b")

#optimal number of clusters can be identified by sudden elbow shape in the plot


















