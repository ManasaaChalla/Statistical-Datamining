#################################
#------- Concept Quiz 3 ------
# Created by: Manasa Challa
# Date: 03/09/2021
# Edited : 03/10/2021
#################################

#install.packages("ISLR")
#install.packages("NbClust")
#install.packages("factoextra")
library(ISLR)
library(NbClust)
library(factoextra)
setwd("~/Desktop/spring 2021/STA/Quizzes/Quiz 3")

#################################
# QUESTION 1
#################################

data("iris")
head(iris)
df <- scale(iris[, -5])

#ELBOW METHOD
fviz_nbclust(df, kmeans, method = "wss")+geom_vline(xintercept = 3, linetype = 2)

#SILHOUETTE METHOD
fviz_nbclust(df, kmeans, method = "silhouette")

#################################
# QUESTION 2
#################################

#a) Plotting the observations
obs <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
plot(obs[,1], obs[,2])

#b) Randomly assigning a cluster label to each observation
set.seed(1)
ClusterLabel <- sample(2, nrow(obs), replace = T)
ClusterLabel

plot(obs[, 1], obs[, 2], col = (ClusterLabel + 1), pch = 20, cex = 2)

#c) Computing the centroid for each cluster
c1 <- c(mean(obs[ClusterLabel == 1, 1]), mean(obs[ClusterLabel == 1, 2]))
c2 <- c(mean(obs[ClusterLabel == 2, 1]), mean(obs[ClusterLabel == 2, 2]))
plot(obs[,1], obs[,2], col=(ClusterLabel + 1), pch = 20, cex = 2)
points(c1[1], c1[2], col = 2, pch = 4)
points(c2[1], c2[2], col = 3, pch = 4)

#d) Assign each observation to the centroid to which it is closest, in terms of Euclidean distance.Report the cluster labels for each observation.
ClusterLabel <- c(1, 1, 1, 2, 2, 2)
plot(x=obs[, 1], obs[, 2], col = (ClusterLabel + 1), pch = 20, cex = 2)
points(c1[1], c1[2], col = 2, pch = 4)
points(c2[1], c2[2], col = 3, pch = 4)

#e) Repeat (c) and (d) until the answers obtained stop changing.
c1 <- c(mean(obs[ClusterLabel == 1, 1]), mean(obs[ClusterLabel == 1, 2]))
c2 <- c(mean(obs[ClusterLabel == 2, 1]), mean(obs[ClusterLabel == 2, 2]))
plot(obs[,1], obs[,2], col=(ClusterLabel + 1), pch = 20, cex = 2)
points(c1[1], c1[2], col = 2, pch = 4)
points(c2[1], c2[2], col = 3, pch = 4)

#f) In your plot from (a), color the observations according to the clusters labels obtained.
plot(obs[, 1], obs[, 2], col=(ClusterLabel + 1), pch = 20, cex = 2)




