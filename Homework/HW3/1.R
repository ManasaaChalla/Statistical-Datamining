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

cosineDistance<-1-cosine(t(as.matrix(boolean)))
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
1-cosine(t(as.matrix(TableData)))

DataNorm =Data

#e) Normalize the matrix by subtracting from each nonblank entry the average value for its user.
#tableOld<-data.frame(a=factor(c(5,NA,3), levels=level),b=factor(c(4,3,NA), levels=level),c=factor(c(NA,4,1), levels=level),d=factor(c(5,4,4), levels=level),e=factor(c(2,2,NA), levels=level),f=factor(c(NA,2,4), levels=level),g=factor(c(3,1,5), levels=level),h=factor(c(2,NA,3), levels=level))

mean = rowMeans(DataNorm, na.rm = TRUE)
mean

DataNorm = DataNorm - mean
DataNorm

#f) Using the normalized matrix from Part E, compute the cosine distance between each pair of users.
DataNorm[is.na(DataNorm)] = 0
1-cosine(t(as.matrix(DataNorm)))

