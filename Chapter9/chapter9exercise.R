#1

x1<--10:10
x2=3*x1-1
plot(x1,x2,type="l",col="red")
text(c(-1),c(15), labels = c("Greater than 0"), col = "blue")
text(c(1),c(-15), labels = c("Less than 0"), col = "blue")
#Add Connected Line Segments to a Plot
lines(x1,-1/2*x1+1)
text(c(0),c(-8), labels = c("Less than 0"))
text(c(3),c(20), labels = c("Greater than 0"))

#2
radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", 
     ylab = "X2")
#if add is TRUE, the symbols are added to an existing plot, otherwise a new plot is created.
#inches is FALSE, the units are taken to be those of the appropriate axes. 
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)
text(c(-1),c(2), labels = c("< 4"))
text(c(-3),c(4), labels = c("> 4"))

#c
#(-1,1) belong to red
#(2,2) belong to blue
#(0,0) belong to blue
#(3,8) belong to blue

#3
#a
x1<-c(3,2,4,1,2,4,4)
x2<-c(4,2,4,4,1,3,1)
cols<-c("red","red","red","red","Blue","Blue","Blue")
plot(x1,x2,col=cols,xlim = c(-1,5),ylim =c(-1,5) )
abline(-0.5,1)
#b

#e

plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

#g
x1<-c(3,2,4,1,2,4,4)
x2<-c(4,2,4,4,1,3,1)
cols<-c("red","red","red","red","Blue","Blue","Blue")
plot(x1,x2,col=cols,xlim = c(-1,5),ylim =c(-1,5) )
abline(-0.6, 1.1)

#h
x1<-c(3,2,4,1,2,4,4,4)
x2<-c(4,2,4,4,1,3,1,0)
cols<-c("red","red","red","red","Blue","Blue","Blue","red")
plot(x1,x2,col=cols,xlim = c(-1,5),ylim =c(-1,5) )
abline(-0.6, 1.1)

#2
##a

d = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow=4))
plot(hclust(d, method="complete"))

##b

plot(hclust(d, method="single"))

##c

#(1,2), (3,4)

##d

#(1, 2, 3), (4)

#e
par(mfrow=c(1,2))
plot(hclust(d, method="complete"))
plot(hclust(d, method="complete"), labels=c(2,1,4,3))


#3
##a
par(mfrow=c(1,1))
x1<-c(1,1,0,5,6,4)
x2<-c(4,3,4,1,2,0)

plot(x1,x2)

##b
set.seed(11)
#sample(x,n,replace=T)if x is numerican and greater than 1, sampling via sample takes place from 1:x. 
l <- sample(2, length(x1), replace=T)
l

##c
cd1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
centroid1

#4
library(e1071)

set.seed(1)
x<-rnorm(100)
y<-3*x^4+rnorm(100)+1.5
cut<-sample(100,50)
y[cut]<-y[cut]+10
y[-cut]<-y[-cut]-10
plot(x[cut], y[cut], col = "red", xlab = "X", ylab = "Y",ylim=c(-35,35))
points(x[-cut], y[-cut], col = "blue")

z<-rep(1,100)
z[cut]<--1
data<-data.frame(x=x,y=y,z=as.factor(z))
plot(data,col=(z+3))

train<-sample(100,70)
tune.out<-tune(svm,z~.,data=data[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#we see that cost=1 and gamma 0.5 results in the lowest coss-validation error
#rate. the tune() function stores the best model obtained
bestmod<-tune.out$best.model
summary(bestmod)

svmfit.k<-svm(z~.,data=data[train,],kernel="radial",gamma=0.5,cost=1)
plot(svmfit.k,data[train,])

#training error
ypred<-predict(svmfit.k,data[train,])
table(predict=ypred,truth=data$z[train])
#The training error was 2/(70)
#test error
ypred<-predict(svmfit.k,data[-train,])
table(predict=ypred,truth=data$z[-train])

#The test error was 0 and the training error was 2/70 using kernal method

# Using polynominal method
svmfit.p<-svm(z~.,data=data[train,],kernel="polynomial",cost=1)

ypred<-predict(svmfit.p,data[train,])
table(predict=ypred,truth=data$z[train])
#The training error was 15/(70)
#test error
ypred<-predict(svmfit.p,data[-train,])
table(predict=ypred,truth=data$z[-train])
#The test error was 7/30

#Using suppor classifer, which is the linear method or polynominal that degree=1
svmfit.l<-svm(z~.,data=data[train,],kernel="linear",cost=1)

ypred<-predict(svmfit.l,data[train,])
table(predict=ypred,truth=data$z[train])
#The training error was 4/(70)
#test error
ypred<-predict(svmfit.l,data[-train,])
table(predict=ypred,truth=data$z[-train])
#The test error was 2/30

#In this situation, the kernel out perform the rest method, gives test error of 0.
#and training error of 2

#5

##a
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2>0)
data<-data.frame(X1=x1,X2=x2,y=as.factor(y))

##b
#col=(y+3) indicates that we colored according to their class labels.
plot(x1,x2,col=(y+3),xlab="x1",ylab="x2")

##c

glm.fit<-glm(y~.,data=data,family = "binomial")
summary(glm.fit)

##d
pred<-predict(glm.fit,data,type="response")
preds<-rep(0,500)
preds[pred>0.5]<-1

#They ask the plot using X1 and X2 as the two axis

plot(data[preds == 1, ]$X1, data[preds == 1, ]$X2, col = "blue", pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$X1, data[preds == 0, ]$X2, col = "red", pch = (3 - 0))

#e

glm.fit.1<-glm(y~poly(X1,2)+poly(X2,2)+I(X1*X2),data=data,family = "binomial")
summary(glm.fit.1)

pred<-predict(glm.fit.1,data,type="response")
preds<-rep(0,500)
preds[pred>0.5]<-1


#They ask the plot using X1 and X2 as the two axis

plot(data[preds == 1, ]$X1, data[preds == 1, ]$X2, col = "blue", pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$X1, data[preds == 0, ]$X2, col = "red", pch = (3 - 0))

#The logistic regression with non-linear function will be better

#g
tune.out<-tune(svm,y~.,data=data,kernel="linear",
               ranges=list(cost=c(0.1,1,10,100,1000)))

summary(tune.out)
#we see that cost=10 results in the lowest coss-validation error
#rate. the tune() function stores the best model obtained

svmfit.l<-svm(y~X1+X2,data=data,kernel="linear",cost=0.1)

preds<-predict(svmfit.l,data)
pred
plot(data[preds == 1, ]$X1, data[preds == 1, ]$X2, col = "blue", pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$X1, data[preds == 0, ]$X2, col = "red", pch = (3 - 0))

#h

tune.out<-tune(svm,y~.,data=data,kernel="radial",
               ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))

summary(tune.out)
#we see that cost=10 and gamma 4 results in the lowest coss-validation error
#rate. the tune() function stores the best model obtained
bestmod<-tune.out$best.model
summary(bestmod)

svmfit.r<-svm(y~X1+X2,data=data,kernel="radial",gamma=4,cost=10)

preds<-predict(svmfit.r,data)

plot(data[preds == 1, ]$X1, data[preds == 1, ]$X2, col = "blue", pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$X1, data[preds == 0, ]$X2, col = "red", pch = (3 - 0))

#From the results, we can see that the non-linear kernel and non-linear logistic regression
#gives the best predict boundary. The linear one gives bad prediction

#6

set.seed(1)
x.one <- runif(400, 0, 90)
y.one <- runif(400, x.one + 10, 100)
x.one.noise <- runif(50, 20, 80)
y.one.noise <- 5/4 * (x.one.noise - 10) + 0.1

x.zero <- runif(400, 10, 100)
y.zero <- runif(400, 0, x.zero - 10)
x.zero.noise <- runif(50, 20, 80)
y.zero.noise <- 5/4 * (x.zero.noise - 10) - 0.1

class.one <- seq(1, 450)
x <- c(x.one, x.one.noise, x.zero, x.zero.noise)
y <- c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)

#b
z<-rep(0,900)
z[class.one]<-1
data<-data.frame(x=x,y=y,z=as.factor(z))

tune.out<-tune(svm,z~.,data=data,kernel="linear",ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)

a<-data.frame(cost = tune.out$performance$cost, misclass = tune.out$performance$error * 1100)
plot(tune.out$performance$cost, tune.out$performance$error)

#The cost of 1000 gives best model

#c
set.seed(1)
x.one <- runif(500, 0, 90)
y.one <- runif(500, x.one + 10, 100)

x.zero <- runif(500, 10, 100)
y.zero <- runif(500, 0, x.zero - 10)


class.one <- seq(1, 500)
x <- c(x.one, x.zero)
y <- c(y.one,y.zero)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)

z<-rep(0,1000)
z[class.one]<-1
data.test<-data.frame(x=x,y=y,z=as.factor(z))

cost<-c(0.1,0.5,1,5,10,100,1000)
test.err<-rep(NA,length(cost))
for (i in 1:length(cost)){
  svmfit.l<-svm(z~.,data=data.test,kernel="linear",cost=cost[i])
  pred<-predict(svmfit.l,data.test)
  test.err[i]=sum(pred!=data.test$z)
}

data.frame(cost = cost, misclass = test.err)
#It seems like the linear was correctly classify all the test error no matter what the cost is
#so the small cost may be better

#d
#We again see an overfitting phenomenon for linear kernel.
#A large cost tries to correctly classify noisy-points and hence overfits the 
#train data. A small cost, however, makes a few errors on the noisy test points 
#and performs better on test data.


#7

library(ISLR)
names(Auto)
#a
var<-ifelse(Auto$mpg>median(Auto$mpg),1,0)
Auto$gasm<-as.factor(var)

#b

tune.out<-tune(svm,gasm~.-mpg,data=Auto,kernel="linear",
               ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)

a<-data.frame(cost = tune.out$performance$cost, misclass = tune.out$performance$error)
a
#The cost of 0.1 and 1 gives the same results, as the cost increase the error
#first increase and then decrease

#c
set.seed(1)
tune.out.r<-tune(svm,gasm~.-mpg,data=Auto,kernel="radial",
               ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out.r)

tune.out.r$best.parameters
#best model cost is 1 and gamma is 1

tune.out.p<-tune(svm,gasm~.-mpg,data=Auto,kernel="polynomial",ranges=list(cost=c(0.1,1,10,100,1000),degree=c(2,3,4)))

summary(tune.out.p)
tune.out.p$best.parameters
#best model cost is 1000 and degree is 3

#d
svmfit.l<-svm(gasm~.,data=Auto,kernel="linear",cost=1)
svmfit.r<-svm(gasm~.,data=Auto,kernel="radial",cost=1, gamma=1)

svmfit.p<-svm(gasm~.,data=Auto,kernel="polynomial",cost=1000, degree=3)


plotsvm = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg","gasm", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotsvm(svmfit.l)
plotsvm(svmfit.p)
plotsvm(svmfit.r)

#8

##a
set.seed(1)
train<-sample(nrow(OJ),800)

##b
svmfit.l<-svm(Purchase~.,data=OJ[train,],kernel="linear",cost=0.1)
summary(svmfit.l)
#The number of supper vectors was 345, 173 was on one side and 172 the other side

##c
##training error
preds<-predict(svmfit.l,OJ[train,])
table(preds,OJ$Purchase[train])
mean(preds!=OJ$Purchase[train])
#The training error is 0.16125

#Testing error
preds<-predict(svmfit.l,OJ[-train,])
table(preds,OJ$Purchase[-train])
mean(preds!=OJ$Purchase[-train])
#The testing error is 0.1814815

##d
set.seed(1)
tune.out.l<-tune(svm,Purchase~.,data=OJ[train,],kernel="linear",
                 ranges=list(cost=10^seq(-2, 1, by = 0.25)))
summary(tune.out.l)

best.cost<-tune.out.l$best.parameters
#the cost is 5.623413 is the best model

##e

svmfit.l<-svm(Purchase~.,data=OJ[train,],kernel="linear",cost= tune.out.l$best.parameter$cost)
summary(svmfit.l)
#The number of supper vectors was 345, 173 was on one side and 172 the other side

##training error
preds<-predict(svmfit.l,OJ[train,])
table(preds,OJ$Purchase[train])
mean(preds!=OJ$Purchase[train])
#The training error is  0.16375

#Testing error
preds<-predict(svmfit.l,OJ[-train,])
table(preds,OJ$Purchase[-train])
mean(preds!=OJ$Purchase[-train])
#The test error is 0.185

##f

set.seed(1)
tune.out.r<-tune(svm,Purchase~.,data=OJ[train,],kernel="radial",
                 ranges=list(cost=10^seq(-2, 1, by = 0.25)))
summary(tune.out.r)

best.cost<-tune.out.r$best.parameters

svmfit.r<-svm(Purchase~.,data=OJ[train,],kernel="radial",cost= tune.out.r$best.parameter$cost)
summary(svmfit.r)

preds<-predict(svmfit.r,OJ[train,])
table(preds,OJ$Purchase[train])
mean(preds!=OJ$Purchase[train])
#The training error is   0.145

#Testing error
preds<-predict(svmfit.r,OJ[-train,])
table(preds,OJ$Purchase[-train])
mean(preds!=OJ$Purchase[-train])
#The testing error is   0.19


#g
set.seed(1)
tune.out.p<-tune(svm,Purchase~.,data=OJ[train,],kernel="polynomial",
                 ranges=list(cost=c(0.1,1,10,100,1000),degree=2))
summary(tune.out.p)

best.cost<-tune.out.p$best.parameters

svmfit.p<-svm(Purchase~.,data=OJ[train,],kernel="polynomial",cost= tune.out.p$best.parameter$cost)
summary(svmfit.p)

preds<-predict(svmfit.p,OJ[train,])
table(preds,OJ$Purchase[train])
mean(preds!=OJ$Purchase[train])
#The training error is  0.13875

#Testing error
preds<-predict(svmfit.p,OJ[-train,])
table(preds,OJ$Purchase[-train])
mean(preds!=OJ$Purchase[-train])
#The testing error is  0.2

#h
##The linear kernel gives the best predict and test error is 0.181










