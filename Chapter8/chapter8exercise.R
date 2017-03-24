#Chapter 8 

##7
library(tree)
library(ISLR)
library(randomForest)
library(MASS)
library(gbm)

set.seed(1)
train<-sample(1:nrow(Boston),nrow(Boston)/2)
x.train<-Boston[train,-14]
x.test<-Boston[-train,-14]
y.test<-Boston$medv[-train]
y.train<-Boston$medv[train]

#The random forest has two forms, 1 is using the randomForest(formular,data=Null,subset=)
#The other one is the following one, the following one is easy to calculate the test error and calculate the result

rf.boston.25.1<-randomForest(x=x.train,y=y.train,xtest=x.test,ytest = y.test,ntree=25,mtry=(ncol(Boston)-1)/2)
rf.boston.25.2<-randomForest(x=x.train,y=y.train,xtest=x.test,ytest = y.test,ntree=25,mtry=ncol(Boston)-1)
rf.boston.25.3<-randomForest(x=x.train,y=y.train,xtest=x.test,ytest = y.test,ntree=25,mtry=sqrt(ncol(Boston-1)))
rf.boston.500.1<-randomForest(x=x.train,y=y.train,xtest=x.test,ytest = y.test,ntree=500,mtry=(ncol(Boston)-1)/2)
rf.boston.500.2<-randomForest(x=x.train,y=y.train,xtest=x.test,ytest = y.test,ntree=500,mtry=ncol(Boston)-1)
rf.boston.500.3<-randomForest(x=x.train,y=y.train,xtest=x.test,ytest = y.test,ntree=500,mtry=sqrt(ncol(Boston-1)))


plot(1:25, rf.boston.25.1$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE")
lines(1:25, rf.boston.25.2$test$mse, col = "red", type = "l")
lines(1:25, rf.boston.25.3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("green", "red", "blue"), lty = 1)

par(mfrow=c(1,1))
plot(1:500, rf.boston.500.1$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE")
lines(1:500, rf.boston.500.2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.500.3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("green", "red", "blue"), lty = 1,cex=1, pch=1, pt.cex = 1)

#For both plots, the test MSE was decreasing as the number of trees increased. For the growing trees upto25, 
#there is not too much difference between using different number of variables.
#However, when we growing trees upto 500, we can see that the bagging gives the smallest test MSE. after around 
#140 trees, the bagging and random forest are almost same.

#8

##a
set.seed(1)
train<-sample(1:nrow(Carseats),nrow(Carseats)/2)
x.train<-Carseats[train,]
x.test<-Carseats[-train,]
y.train<-Carseats$Sales[train]
y.test<-Carseats$Sales[-train]

##b

tree.car<-tree(Sales~.,data = Carseats,subset = train)

plot(tree.car)
text(tree.car,pretty=0)
##Describe the trees:The unit sales was high for the location,where the
#average age is greater than 62,the quality of the shelving is good and the
#the charges for car seats was less than 113. The average sale price was $8,600 dollars


yhat<-predict(tree.car,newdata=x.test)
boston.test=Boston$medv[-train]
plot(yhat,y.test)
test.mse<-mean((yhat-y.test)^2)
#The test MSE is 4.148897

#c
#cross-validation select the alpha and terminal node
#The default is  FUN = prune.tree
set.seed(1)
cv.car<-cv.tree(tree.car,FUN = prune.tree)
par(mfrow=c(1,2))
plot(cv.car$size,cv.car$dev,type="b")
plot(cv.car$k,cv.car$dev,type="b")
#The lowest MSE has 8 terminal nodes

prune.car<-prune.tree(tree.car,best=8)
plot(prune.car)
text(prune.car,pretty=0)

yhat<-predict(prune.car,newdata=x.test)
plot(yhat,y.test)
test.mse<-mean((yhat-y.test)^2)

#The test error is 5.09. The test error was increased. There is no improvement of the test error.

#d
set.seed(1)
#mtry=13 indicates that all 13 predictors should be considered for each split of the tree. that bagging should be done
p<-ncol(Carseats)-1
bag.car<-randomForest(Sales~.,data=Carseats,subset=train,mtry=p,importance=T)
importance(bag.car)

#how well does this bagged model perform on the test set
yhat.bag<-predict(bag.car,newdata=x.test)
plot(yhat.bag,y.test)
abline(0,1)
test.mse<-mean((yhat.bag-y.test)^2)

#The test MSE was 2.57295 and we can see that the most important three variables were price, shelveloc, and age
#The test error was decrease

#d
rf.car<-randomForest(Sales~.,data=Carseats,subset=train,mtry=sqrt(p),importance=T)
importance(rf.car)

#how well does this bagged model perform on the test set
yhat.bag<-predict(rf.car,newdata=x.test)
plot(yhat.bag,y.test)
abline(0,1)
test.mse<-mean((yhat.bag-y.test)^2)

#The test MSE was 3.326674 and we again see that the most important three variables were price, 
#shelveloc, and age The test error was increased from bagging, but smaller than single tree. 


#9

##a
train<-sample(1:nrow(OJ),800)
#names(OJ)
x.train<-OJ[train,]
x.test<-OJ[-train,]
y.test<-OJ$Purchase[-train]
y.train<-OJ$Purchase[train]

##b
oj.tree<-tree(Purchase~.,data=x.train)
summary(oj.tree)
#The training error is 125/800=0.15625, the number of terminal nodes is 7.

##c
oj.tree
#Let's pick terminal node labeled 27. The splitting variable at this node is ListpriceDiff, $\tt{ListpriceDiff}$. 
#The splitting value of this node is $0.135$. There are $157$ points in the subtree below this node.
#The deviance for all points contained in region below this node is $134.30$. A * in the line denotes that 
#this is in fact a terminal node. The prediction at this node is $\tt{Sales}$ = $\tt{CH}$. About $84.713$% 
#points in this node have $\tt{CH}$ as value of $\tt{Sales}$. Remaining $15.287$% points have $\tt{MM}$ as 
#value of $\tt{Sales}$.

##d
par(mfrow=c(1,1))
plot(oj.tree)
text(oj.tree,pretty=0)
#$\tt{LoyalCH}$ is the most important variable of the tree, in fact top 3 nodes contain $\tt{LoyalCH}$. 
#If $\tt{LoyalCH} < 0.27$, the tree predicts $\tt{MM}$. If $\tt{LoyalCH} > 0.76$, the tree predicts
#$\tt{CH}$. For intermediate values of $\tt{LoyalCH}$, the decision also depends on the value of 
#$\tt{PriceDiff}$.

#e
tree.pred<-predict(oj.tree,x.test,type="class")
table(tree.pred, y.test)
test.error<-mean(tree.pred!=y.test)
#The test MSE is 0.2148148

#f-h
cv.oj<-cv.tree(oj.tree,FUN = prune.tree)
par(mfrow=c(1,2))
plot(cv.oj$size,cv.oj$dev,type="b")
plot(cv.oj$k,cv.oj$dev,type="b")
#it seems like 5 node gives a good prediction. 

#i-j
prune.oj<-prune.tree(oj.tree,best=5)
summary(prune.oj)
#The training error is 130/800, there is 5 terminal node. The training error was increased. 

#k
tree.pred<-predict(prune.oj,x.test,type="class")
table(tree.pred, y.test)
test.error<-mean(tree.pred!=y.test)
#The test error was 19.259. It was decrease.

#10























