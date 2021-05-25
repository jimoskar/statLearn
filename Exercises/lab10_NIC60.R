#======================#
# Lab Chapter 6: NCI60 #
#======================#

library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

## PCA
pr.out <- prcomp(nci.data, scale = T)

# function for creating colors
Cols <- function(vec){
   cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch=19, cex = 0.5,
     xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, cex = 0.5,
     xlab="Z1",ylab="Z3")
summary(pr.out)

# Plotting PVE
plot(pr.out)
# More informative:
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
       col =" blue ")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")

## CLUSTERING
# Hierarchichal 

# scale
sd.data <- scale(nci.data)

par(mfrow=c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab="", sub="",ylab="")

# Continue with complete linkage and euclidian distance
hc.out <- hclust(data.dist, method = "complete")
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs) > abline(h=139, col="red")

# K-means
set.seed(1)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

# Performing hc on the first PC rather than on whole dataset:
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First
Five Score Vectors ")
table(cutree(hc.out,4), nci.labs)


