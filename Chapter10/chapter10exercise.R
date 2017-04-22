#2

##a-e
d = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow=4))
plot(hclust(d, method="complete"))
plot(hclust(d, method="single"))
plot(hclust(d, method="complete"), labels=c(2,1,4,3))

#If the results in (a) have two cluster. We can see that (1,2) and (3,4)
#The two clusters are (4), (1,2,3)

##3

##a
set.seed(1)
x1<-c(1,1,0,5,6,4)
x2<-c(4,3,4,1,2,0)
data<-cbind(x1,x2)
plot(data[,1],data[,2])
plot(data)

##b
s<-sample(2,nrow(data),replace = T)
plot(data[,1],data[,2],col=s+1,pch=20,cex=2)

##c

centroid1<-c(mean(data[s==1,1]),mean(data[s==1,2]))
centroid2<-c(mean(data[s==2,1]),mean(data[s==2,2]))
plot(data[,1],data[,2],col=s+1,pch=20,cex=2)
points(centroid1[1],centroid1[2],col=1,pch=4)
points(centroid2[1],centroid2[2],col=2,pch=4)

##d
euclid = function(x1, x2) {
  return(sqrt((x1[1] - x2[1])^2 + (x1[2]-x2[2])^2))
}
assign_labels = function(x, centroid1, centroid2) {
  s = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], centroid1) < euclid(x[i,], centroid2)) {
      s[i] = 1
    } else {
      s[i] = 2
    }
  }
  return(s)
}
labels = assign_labels(data, centroid1, centroid2)
labels

##e

centroid1<-c(mean(data[s==1,1]),mean(data[s==1,2]))
centroid2<-c(mean(data[s==2,1]),mean(data[s==2,2]))
plot(data[,1],data[,2],col=s+1,pch=20,cex=2)
points(centroid1[1],centroid1[2],col=1,pch=4)
points(centroid2[1],centroid2[2],col=2,pch=4)

#In here each observation was assigned to the closest centroid

#f
plot(data[,1],data[,2],col=s+1,pch=20,cex=2)
#Point 1,2,3 was in one cluster and point 4,5,6 in another

#5

##a

socks<-c(10,9,8,7,6,5,10)
computers<-c(0,0,0,1,1,1,1)
shop<-cbind(socks,computers)
labels<-c(1,1,1,2,2,2,1)
plot(shop[,1],shop[,2],col=labels+1,pch=20,cex=2)

##b

socks<-scale(socks,center = F)
computers<-scale(computers,center = F)
shop<-cbind(socks,computers)
labels<-c(1,1,1,2,2,2,1)
plot(shop[,1],shop[,2],col=labels+1,pch=20,cex=2)

#6

#c

set.seed(10)
control<-matrix(rnorm(50*1000),ncol = 50)
treatment<-matrix(rnorm(50*1000),ncol = 50)
data<-cbind(control,treatment)
data[1,]<-seq(-20,20-0.4,.4)#Linear trend in one dimension

prc<-prcomp(scale(data))
summary(prc)$importance[,1]
#0.11541 explained by the first principle

#Now addint A vs B via 10 vs 0 coding

data<-rbind(data,c(rep(10,50),rep(0,50)))
prc<-prcomp(data,scale=T)
summary(prc)$importance[,1]
#0.147900  of the variables was explained.It was showing that addint the A vs B
#improving the variation been explained. 


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


