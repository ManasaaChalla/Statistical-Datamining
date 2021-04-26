#################################
# QUESTION 2
#################################


#a.) On the basis of this dissimilarity matrix, sketch the dendrogram that results from hierarchically clustering these four observations using complete linkage.
data = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                        0.3, 0, 0.5, 0.8,
                        0.4, 0.5, 0.0, 0.45,
                        0.7, 0.8, 0.45, 0.0), nrow = 4))
data
plot(hclust(data, method = "complete"))

#b.) Repeat (a), this time using simple linkage clustering.
plot(hclust(data, method = "single"))

#c.) Suppose that we cut the dendrogram obtained in (a) such that two clusters result. Which observations are in each cluster ?
#Ans : We obtain clusters (1,2) and (3,4).

#d.) Suppose that we cut the dendrogram obtained in (b) such that two clusters result. Which observations are in each cluster ?
#Ans : We obtain clusters (1,2,3) and (4).

#e.) Draw a dendrogram that is equivalent to the dendrogram in (a), for which two or more of the leaves are repositioned, but for which the meaning of the dendrogram is the same.
plot(hclust(data, method = "complete"), labels = c(2,1,4,3))

