install.packages("dummies")
install.packages("dendextend")
install.packages("dendextendRcpp")
install.packages("gridExtra")
install.packages("cluster")
install.packages("factoextra")
install.packages("MASS")
install.packages("fpc")
## Libraries
library("dummies")
library("dendextend")
library("dendextendRcpp")
library("gridExtra")
library("cluster")
library("factoextra")
library("MASS")
library("fpc")

#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
Eastwestairlines <- read.csv(choose.files())
View(Eastwestairlines)
#removing the firstcolumn
dummy.data.frame(Eastwestairlines1, names = "cc1_miles", omit.constants=FALSE)
Eastwestairlines1 <- Eastwestairlines[,2:12]
#Creating Dummy variables for categorical data
Eastwestairlines2 <-dummy.data.frame(Eastwestairlines1, names = "cc1_miles", omit.constants=FALSE )
Eastwestairlines2 <-dummy.data.frame(Eastwestairlines1, names = "cc2_miles", omit.constants=FALSE )
Eastwestairlines2 <-dummy.data.frame(Eastwestairlines1, names = "cc3_miles", omit.constants=FALSE )
# Standardize Data
Eastwestairlines3   <- scale(Eastwestairlines2)
View(Eastwestairlines3)
# 2. Compute dissimilarity matrix
d <- dist(Eastwestairlines3, method = "euclidean")
d
# Hierarchical clustering using Ward's method
res.hc    <- hclust(d, method = "ward.D2" )
plot(res.hc)
Dend1   <- as.dendrogram(res.hc)
Dend1
#Random Sample1 with 95% of data
input2=Eastwestairlines[sample(nrow(Eastwestairlines3),replace=F,size=0.95*nrow(Eastwestairlines)),]
d1 <- dist(input2, method = "euclidean")
res2.hc <- hclust(d1, method = "ward.D2" )
Dend2   <- as.dendrogram(res2.hc)
Dend2
#Random Sample2 with 95% of data
input3=Eastwestairlines[sample(nrow(Eastwestairlines3),replace=F,size=0.95*nrow(Eastwestairlines)),]

d2 <- dist(input3, method = "euclidean")
res3.hc <- hclust(d2, method = "ward.D2" )
Dend3   <- as.dendrogram(res3.hc)
Dend1
Dend2
Dend3
# Comparison of Population, Random sample 1 and Random sample 2 dendogarms.
all.equal(Dend3, Dend2, Dend1, use.edge.length = TRUE)
plot(Dend1)
plot(Dend2)
plot(Dend3)

#Conclusion
#the dendogarams are not same for all the Dend1,Dend2,Dend3.For every instance of sample, new insights are emerged.
#Cluser 1- metrics depicts that the level of spending is average to cluster 2 and 3.
#Cluster 2 metrics are leading in progressive way except non-flight bonus transactions.
        #These are the segment of customers, who are associated with EastWest Airlines since long time.
#cluster3-Cluster 3 metrics depicts that the flight miles and flight transactions in last 12 months is zero and their
     #non-flight bonus transactions are leading than the other clusters.


#Kmean clustering
## K-means clustering
set.seed(123)
fit <- kmeans(Eastwestairlines3, 3) # 3 cluster solution
fit
#Aggregation of k-means

mydatak     <- data.frame(Eastwestairlines2, fit$cluster) # append cluster membership
temp        <- aggregate(mydatak, by=list(fit$cluster), FUN=mean)
temp
#to find the size of clusters 
ClusterCo   <- aggregate(mydatak, by=list(fit$cluster), FUN=sum) 
#to find the cluster size
d <- transform(ClusterCo, clusterSize = fit.cluster / Group.1)
d <- transform(d, fit.cluster= fit.cluster/ clusterSize)
temp$clusterSize   <- d$clusterSize
temp$clusterPCT    <- (d$clusterSize*100)/3999
# transpose to change from horizontal to vertical
temp2       <- t(temp)
temp2
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

temp3       <- round_df(temp2, 2)
temp3
#Hierarchical Aggregate calculations

# Hierarchical clustering using Ward's method

#set.seed(123)
groups      <- cutree(res.hc, k=3) # cut tree into 3 clusters
membership  <-as.matrix(groups)
membership  <- data.frame(membership)
names(membership) <- c("cluster")
mydatao     <- data.frame(Eastwestairlines2, membership$cluster) # append cluster membership

temp        <- aggregate(mydatao, by=list(membership$cluster), FUN=mean)

temp4       <- t(temp)
temp4
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

temp5       <- round_df(temp4, 2)
temp5

#cluster2
#62.64% of customers fall into this cluster. There is potential business growth 
opportunity, if the passengers are turned into regular travellers.
#1. If frequent flyer credit card is used then the reward points per travel 
percentage can increased. 
#2. If the number of travel checkins are more than 5 in an year, Then extra
bonus points can be awarded to the customer. 
