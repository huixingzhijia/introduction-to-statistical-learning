#7
library(ISLR)
set.seed(2)
#each obs. has been centered to have mean zero and 
#standard deviation one. 
scal<-scale(USArrests)
r_ij<-cor(t(scal))
a<-dist(scal)^2
b<-as.dist(1-r_ij)
summary(a/b)

#8
##a
set.seed(1)
pr.out<-prcomp(USArrests,scale=T,center=T)
#or scale(USArrest)
pr.var<-pr.out$sdev^2
pve1<-pr.var/sum(pr.var)


##b
#loading
loading<-pr.out$rotation
data<-scale(USArrests)
sumvar<-sum(apply(as.matrix(data)^2,2,sum))
#same as sum(apply(as.matrix(data)^2,1,sum))
pve2<-apply((as.matrix(data) %*% loading)^2,2,sum)/sumvar

#9

#a

hc.complete<-hclust(dist(USArrests),method = "complete")
summary(hc.complete)

plot(hc.complete, main = "Complete Linkage with Euclidean Distance", xlab = "", sub = "")
tree.0<-cutree(hc.complete,3)
table(tree.0)


##c
data.usarrest<-scale(USArrests)
hc.complete.scale<-hclust(dist(data.usarrest),method = "complete")
summary(hc.complete.scale)

plot(hc.complete.scale, main = "Complete Linkage with Euclidean Distance", xlab = "", sub = "")
tree.1<-cutree(hc.complete.scale,3)
table(tree.1)

#d
table(tree.0,tree.1)
#Scaling the variables effects the max height of the dendogram obtained 
#from hierarchical clustering. It does affect the 
#clusters obtained from cutting the dendogram into 3 clusters. The data should be standardized
#and center.Because different variables meansured on different scale. If we didn't standardize,
#some variables may dominate the effect.

#10
#a
set.seed(10)
data<-matrix(rnorm(20*3*50,mean=0,sd=0.001),ncol = 50)

data[1:20, 2] <- -10
data[21:40, 1] <- 2
data[21:40, 2] <- 2
data[41:60, 1] <- 10
true.labels<-c(rep(1, 20), rep(2, 20), rep(3, 20))
#to check the separation
plot(data)

#b
pc.out<-prcomp(data,scale=T)
summary(pc.out)
plot(pc.out$x[,1:2],col=2:4,pch=19,xlab = "Z1",ylab = "Z2")

#c
km.out<-kmeans(data,3,nstart=20)
table(true.labels,km.out$cluster)
#The observation was cluster correctly.

#d
km.out<-kmeans(data,2,nstart=20)
table(true.labels,km.out$cluster)
#The first class contain two cluster

#e
km.out<-kmeans(data,4,nstart=20)
table(true.labels,km.out$cluster)
#The k-means method cluster 1 and 3 was all belong to one true cluster.

#f
km.out<-kmeans(pc.out$x[,1:2],3,nstart=20)
table(true.labels,km.out$cluster)
#The results were worse than other method.


#g
data.scale<-scale(data)
km.out<-kmeans(data,3,nstart=20)
table(true.labels,km.out$cluster)
#The data was separated correctly.There is no bad effect

#11

#a
ch10 <- read.csv('D:/courses/BSTcourse/machine learning and predictive modeling/week11/Ch10Ex11.csv', header=F)

#b
ch10_scale<-scale(ch10)
dd <- as.dist(1 - cor(t(ch10)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab = "", sub = "")

plot(hclust(dd, method = "single"), main = "Single Linkage with Correlation-Based Distance", xlab = "", sub = "")

plot(hclust(dd, method = "average"), main = "Average Linkage with Correlation-Based Distance", xlab = "", sub = "")

#Yes, The results were depend on the method we used. 

#c

#Principle Component Aanalysis was used to see which genes differ the most. 
#We will examine the absolute values of the total loadings for each gene as 
#it characterizes the weight of each gene.

pr.out<-prcomp(t(ch10))
head(pr.out$rotation)
total.load <- apply(pr.out$rotation, 1, sum)
index <- order(abs(total.load), decreasing = TRUE)
index[1:10]
#There are two most different genes across the two groups.


