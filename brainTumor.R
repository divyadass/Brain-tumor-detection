#### K -Means Clustering ####### 
healthy = read.csv("healthy.csv", header = FALSE)
head(healthy)
healthyMatrix = as.matrix(healthy)
healthyVector = as.vector(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))
str(healthyVector)

### no. of distances that will be counted in Hierchichal clustering
(365636*365635)/2
### Therefore, we will go with k-means clustering

Kmeans = kmeans(healthyVector, iter.max = 1000, centers = 5)
str(Kmeans)
healthyCluster = Kmeans$cluster
dim(healthyCluster) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyCluster, axes=FALSE, col = rainbow(5))

tumor = read.csv("tumor.csv", header = FALSE)
tumorMatrix = as.matrix(tumor)
str(tumorMatrix)
tumorVector = as.vector(tumorMatrix)
###### Applying K-means clustering image on this image #### 

#install.packages("flexclust")
## install this package if you don't have it

### flexclust for k-centroid cluster analysis
# We need to convert the information from the clustering
# algorithm to an object of the class KCCA.
# And this conversion is needed before we
# can use the predict function on the test set tumorVector. 

library(flexclust) 
KMC.kcca = as.kcca(Kmeans, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE , col = rainbow(5))

## We can see the differnt region in the MRI image
## This region contains the tumor part