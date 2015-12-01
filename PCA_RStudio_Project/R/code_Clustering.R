#k means clustering
data(iris) #this IRIS dataset contains measurements in centi meters
irisSAS <- iris[,1:4]*10 #the SAS IRIS dataset example of PROC FASTCLUS seemed to use
                         #the dataset containing measurements in mm and hence I am 
                         #multiplying by 10

#Running K-Means with maximum iterations amounting to 100 and 100 random initializations
set.seed(12345)
output <- kmeans(irisSAS, 3, iter.max = 100, nstart = 100)

table(output$cluster,iris$Species) #comparing original species to the kmeans clusters
plot(iris[,1], iris[,2], col = output$cluster)

output$centers #cluster centers
output$size #the number of points in each cluster - This is identical output to SAS
output$withinss #total within cluster sum of squares (WCSS)
output$tot.withinss # sum of all WCSS. After 100 random initializations,
                    #the cluster set with the lowest total WCSS is chosen as output

clust1 <- irisSAS[output$cluster==1,]
clust2 <- irisSAS[output$cluster==2,]
clust3 <- irisSAS[output$cluster==3,]
sdData <- data.frame(rbind(sapply(clust3,sd),sapply(clust1,sd),
                           sapply(clust2,sd))) #for comparison with SAS output
meanData <- data.frame(rbind(sapply(clust3,mean),sapply(clust1,mean),
                             sapply(clust2,mean))) #for comparison with SAS output


library(roxygen2)
library(devtools)
document()
