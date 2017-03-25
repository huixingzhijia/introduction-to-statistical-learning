#Chapter 8 

##7
library(tree)
library(ISLR)
library(randomForest)
library(MASS)
library(gbm)
library(glmnet)
library(foreign)#read.xport

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

#10 boosting

##a

Hitters<-na.omit(Hitters)
Hitters$Salary<-log(Hitters$Salary)
#names(Hitters)

##b

train<-sample(nrow(Hitters),200)
x.train<-Hitters[train,]
x.test<-Hitters[-train,]
y.train<-Hitters$Salary[train]
y.test<-Hitters$Salary[-train]

##c
set.seed(1)
lr<-seq(-5,-0.1,by=0.1)
lambda<-10^lr
train.error<-rep(NA,length(lambda))
for (i in 1: length(lambda)){
  boost.Hitter<-gbm(Salary~.,data=Hitters[train,],distribution="gaussian",n.trees=1000,
                    shrinkage = lambda[i], verbose=F)
  pred.train<-predict(boost.Hitter,x.train,n.trees = 1000)
  train.error[i]=mean((pred.train-y.train)^2)
}

plot(lambda,train.error,type = "b", xlab = "Shrinkage values", ylab = "Training MSE")
min.train<-min(train.error)
number<-which.min(train.error)
#The 12 lambda which is 0.7943282 gives a minmum training error about 0.003212434.
lambda<-lambda[which.min(train.error)]

##d
lr<-seq(-5,-0.1,by=0.1)
lambda<-10^lr
test.error<-rep(NA,length(lambda))
for (i in 1: length(lambda)){
  boost.Hitter<-gbm(Salary~.,data=Hitters[train,],distribution="gaussian",n.trees=1000,
                    shrinkage = lambda[i], verbose=F)
  pred.test<-predict(boost.Hitter,x.test,n.trees = 1000)
  test.error[i]=mean((pred.test-y.test)^2)
}

plot(lambda,test.error,type = "b", xlab = "Shrinkage values", ylab = "Testing MSE")
min.test<-min(test.error)
number<-which.min(test.error)

#The lambda, 0.01995262 gives a minmum test error about 0.1429364

lambda<-lambda[which.min(test.error)]

#it seems like the training error was decreasing as the lambda or shrinkage value was increase, however,
#The test error was decrease and then increase.

##e

###Linear regression 
lm.fit<-lm(Salary~.,data=Hitters,subset = train)
lm.pred<-predict(lm.fit,x.test)
test.error<-mean((lm.pred-y.test)^2)
#The test error is about 0.3722868

##Lasso
names(Hitters)
x.train.matrix<-model.matrix(Salary~.,Hitters)[train,-19]
x.test.matrix<-model.matrix(Salary~.,Hitters)[-train,-19]
set.seed(1)
lasso.fit<-glmnet(x.train.matrix,y.train,alpha=1)

lass.pred<-predict(lasso.fit,x.test.matrix)
test.error<-mean((lass.pred-y.test)^2)
#The test errror is 0.403, which is higher than bagging and random forest

##f
boost.Hitter<-gbm(Salary~.,data=Hitters[train,],distribution="gaussian",n.trees=1000,
                  shrinkage = lambda[which.min(test.error)], verbose=F)

summary(boost.Hitter)
#CRuns was the most important variable so far


##g

bag.hitter<-randomForest(Salary~.,data=Hitters,subset=train,mtry=ncol(Hitters)-1,importance=T)

#how well does this bagged model perform on the test set
yhat.bag<-predict(bag.hitter,newdata=Hitters[-train,])
plot(yhat.bag,y.test)
abline(0,1)
mean((yhat.bag-y.test)^2)

#The test error is 0.1747


#11
train<-1:1000
set.seed(1)
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
y.test<-Caravan$Purchase[-train]
boost.caravan<-gbm(Purchase~.,data=Caravan[train,],distribution="gaussian",n.trees=1000,
                  shrinkage = 0.01, verbose=F)
summary(boost.caravan)

#The variable PPERSAUT and mkoopkla are the two most important

#c

boost.pred<-predict(boost.caravan,Caravan[-train,],n.trees=1000)
boost.prob = ifelse(boost.pred >0.2, 1, 0)
table(boost.prob,y.test)
test.error<-mean(boost.prob!=y.test)

#The fraction of people that predicted to make a purchase in fact make a purchase, is 11/58. Which means the prediction 
#correct probability is 11/58

#logistic regression
glm.fit<-glm(Purchase~.,data=Caravan,subset = train, family = binomial)
glm.pred<-predict(glm.fit,Caravan[-train,],type = "response")
pred.test2 <- ifelse(glm.pred > 0.2, 1, 0)
table(pred.test2,y.test)

#The fraction of people that predicted to make a purchase in fact make a purchase, is 58/(350+58). Which means the prediction 
#correct probability is 58/(350+58). which is lower than the boosting

#12

#Try to use the BRFSS 2014 as the training data and BRFSS 2015 as the testing data

llcp2014 <- read.xport(file = "D:/courses/BSTcourse/machine learning and predictive modeling/midterm/LLCP2014XPT/LLCP2014.XPT")
LLCP2014<-llcp2014[ ,c("X_STATE","MSCODE","X_RACE","X_AGE_G","SEX","INCOME2","EMPLOY1","HLTHPLN1","AVEDRNK2","X_EDUCAG","HAVARTH3","MENTHLTH")]

llcp2015 <- read.xport(file = "D:/courses/BSTcourse/machine learning and predictive modeling/midterm/LLCP2015XPT/LLCP2015.XPT")
LLCP2015<-llcp2015[ ,c("X_STATE","MSCODE",
                       "X_RACE","X_AGE_G","SEX","INCOME2","EMPLOY1","HLTHPLN1","AVEDRNK2","X_EDUCAG","HAVARTH3","MENTHLTH")]


LLCP2014$Region[LLCP2014$X_STATE %in% c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47, 48, 51, 54, 66, 72)] <- "South"
LLCP2014$Region[LLCP2014$X_STATE %in% c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56)] <- "West"
LLCP2014$Region[LLCP2014$X_STATE %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50)] <- "Northeast"
LLCP2014$Region[LLCP2014$X_STATE %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)] <- "Midwest"
LLCP2014$Region <- as.factor(LLCP2014$Region)

#MSCODE (Urbanity)
#Have lots of missing values

LLCP2014$Urbanity[LLCP2014$MSCODE == 1] <- "Center of metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 2] <- "Outside metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 3] <- "Suburb of metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 5] <- "Non-metropolitan statistical area"
LLCP2014$Urbanity<- as.factor(LLCP2014$Urbanity)

#X_RACE(Race group)

LLCP2014$Race[LLCP2014$X_RACE==1] <-"Non-Hispanic White"
LLCP2014$Race[LLCP2014$X_RACE == 2] <- "Non-Hispanic Black"
LLCP2014$Race[LLCP2014$X_RACE %in% c(3,4,5,6,7)] <- "Non-Hispanic Others"
LLCP2014$Race[LLCP2014$X_RACE == 8] <- "Hispanic"
LLCP2014$Race[LLCP2014$X_RACE == 9] <- NA
LLCP2014$Race<- as.factor(LLCP2014$Race)


#X_AGE_G (Age)

LLCP2014$Age[LLCP2014$X_AGE_G==1] <-"18-24"
LLCP2014$Age[LLCP2014$X_AGE_G == 2] <- "25-34"
LLCP2014$Age[LLCP2014$X_AGE_G ==3] <- "35-44"
LLCP2014$Age[LLCP2014$X_AGE_G == 4] <- "45-54"
LLCP2014$Age[LLCP2014$X_AGE_G == 5] <- "55-64"
LLCP2014$Age[LLCP2014$X_AGE_G == 6] <- "65+"
LLCP2014$Age<- as.factor(LLCP2014$Age)


#SEX (gender)
LLCP2014$gender[LLCP2014$SEX==1] <-"Male"
LLCP2014$gender[LLCP2014$SEX==2] <-"Female"
LLCP2014$gender<- as.factor(LLCP2014$gender)

#INCOME2 (income)

LLCP2014$income[LLCP2014$INCOME2 %in% c(1,2)] <-"<=$15,000"
LLCP2014$income[LLCP2014$INCOME2 %in% c(3,4)] <-"$15,000-$25,000"
LLCP2014$income[LLCP2014$INCOME2 ==5] <-"$25,000-$35,000"
LLCP2014$income[LLCP2014$INCOME2 ==6] <-"$35,000-$50,000"
LLCP2014$income[LLCP2014$INCOME2 ==7] <-"$50,000-$75,000"
LLCP2014$income[LLCP2014$INCOME2 ==8] <-"$75,000+"
LLCP2014$income<- as.factor(LLCP2014$income)

#EMPLOY1 (employment status)

LLCP2014$employ[LLCP2014$EMPLOY1 %in% c(1,2)] <-"employed"
LLCP2014$employ[LLCP2014$EMPLOY1 %in% c(3,4,5,6,7,8)] <-"unemployed"
LLCP2014$employ[LLCP2014$EMPLOY1 ==9] <-NA
LLCP2014$employ<- as.factor(LLCP2014$employ)

#HLTHPLN1 (health insurance)
LLCP2014$Hinsurance[LLCP2014$HLTHPLN1 ==1] <-"Yes"
LLCP2014$Hinsurance[LLCP2014$HLTHPLN1 ==2] <-"No"
LLCP2014$Hinsurance[LLCP2014$HLTHPLN1 %in% c(7,9)] <-NA
LLCP2014$Hinsurance<- as.factor(LLCP2014$Hinsurance)

#AVEDRNK2(about how many drinks did you drink on the average?)
LLCP2014$AVEDRNK2[LLCP2014$AVEDRNK2 %in% c(77,99)] <-NA
LLCP2014$AVEDRNK2<-as.numeric(LLCP2014$AVEDRNK2)


#X_EDUCAG (education)

LLCP2014$Education[LLCP2014$X_EDUCAG ==1] <-"Did not graduate High School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==2] <-"Graduated from High School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==3] <-"Attended College/Technical School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==4] <-"Graduated from College/Technical School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==9] <-NA
LLCP2014$Education<- as.factor(LLCP2014$Education)

#Outcome numeric variable
# poor mental health days: MENTHLTH (1-30, 88 = None, 77 = don't know, 99 = refused)

LLCP2014$MENTHLTH[LLCP2014$MENTHLTH %in% c(88,77,99)] <-NA


#The binary health outcome variable 
#HAVARTH3 (If it have skin cancer)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

LLCP2014$arthritis[LLCP2014$HAVARTH3 ==1] <-"Yes"
LLCP2014$arthritis[LLCP2014$HAVARTH3 ==2] <-"No"
LLCP2014$arthritis[LLCP2014$HAVARTH3 %in% c(7,9)] <-NA
LLCP2014$arthritis<- as.factor(LLCP2014$arthritis)

LLCP2014<-na.omit(LLCP2014)

#X_STATE (Region)
LLCP2015$Region[LLCP2015$X_STATE %in% c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47, 48, 51, 54, 66, 72)] <- "South"
LLCP2015$Region[LLCP2015$X_STATE %in% c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56)] <- "West"
LLCP2015$Region[LLCP2015$X_STATE %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50)] <- "Northeast"
LLCP2015$Region[LLCP2015$X_STATE %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)] <- "Midwest"
LLCP2015$Region <- as.factor(LLCP2015$Region)

#MSCODE (Urbanity)
#Have lots of missing values

LLCP2015$Urbanity[LLCP2015$MSCODE == 1] <- "Center of metropolitan statistical area"
LLCP2015$Urbanity[LLCP2015$MSCODE == 2] <- "Outside metropolitan statistical area"
LLCP2015$Urbanity[LLCP2015$MSCODE == 3] <- "Suburb of metropolitan statistical area"
LLCP2015$Urbanity[LLCP2015$MSCODE == 5] <- "Non-metropolitan statistical area"
LLCP2015$Urbanity<- as.factor(LLCP2015$Urbanity)

#X_RACE(Race group)

LLCP2015$Race[LLCP2015$X_RACE==1] <-"Non-Hispanic White"
LLCP2015$Race[LLCP2015$X_RACE == 2] <- "Non-Hispanic Black"
LLCP2015$Race[LLCP2015$X_RACE %in% c(3,4,5,6,7)] <- "Non-Hispanic Others"
LLCP2015$Race[LLCP2015$X_RACE == 8] <- "Hispanic"
LLCP2015$Race[LLCP2015$X_RACE == 9] <- NA
LLCP2015$Race<- as.factor(LLCP2015$Race)


#X_AGE_G (Age)

LLCP2015$Age[LLCP2015$X_AGE_G==1] <-"18-24"
LLCP2015$Age[LLCP2015$X_AGE_G == 2] <- "25-34"
LLCP2015$Age[LLCP2015$X_AGE_G ==3] <- "35-44"
LLCP2015$Age[LLCP2015$X_AGE_G == 4] <- "45-54"
LLCP2015$Age[LLCP2015$X_AGE_G == 5] <- "55-64"
LLCP2015$Age[LLCP2015$X_AGE_G == 6] <- "65+"
LLCP2015$Age<- as.factor(LLCP2015$Age)


#SEX (gender)
LLCP2015$gender[LLCP2015$SEX==1] <-"Male"
LLCP2015$gender[LLCP2015$SEX==2] <-"Female"
LLCP2015$gender<- as.factor(LLCP2015$gender)

#INCOME2 (income)

LLCP2015$income[LLCP2015$INCOME2 %in% c(1,2)] <-"<=$15,000"
LLCP2015$income[LLCP2015$INCOME2 %in% c(3,4)] <-"$15,000-$25,000"
LLCP2015$income[LLCP2015$INCOME2 ==5] <-"$25,000-$35,000"
LLCP2015$income[LLCP2015$INCOME2==6] <-"$35,000-$50,000"
LLCP2015$income[LLCP2015$INCOME2 ==7] <-"$50,000-$75,000"
LLCP2015$income[LLCP2015$INCOME2 ==8] <-"$75,000+"
LLCP2015$income<- as.factor(LLCP2015$income)

#EMPLOY1 (employment status)

LLCP2015$employ[LLCP2015$EMPLOY1 %in% c(1,2)] <-"employed"
LLCP2015$employ[LLCP2015$EMPLOY1 %in% c(3,4,5,6,7,8)] <-"unemployed"
LLCP2015$employ[LLCP2015$EMPLOY1 ==9] <-NA
LLCP2015$employ<- as.factor(LLCP2015$employ)

#HLTHPLN1 (health insurance)
LLCP2015$Hinsurance[LLCP2015$HLTHPLN1 ==1] <-"Yes"
LLCP2015$Hinsurance[LLCP2015$HLTHPLN1 ==2] <-"No"
LLCP2015$Hinsurance[LLCP2015$HLTHPLN1 %in% c(7,9)] <-NA
LLCP2015$Hinsurance<- as.factor(LLCP2015$Hinsurance)

#AVEDRNK2(about how many drinks did you drink on the average?)
LLCP2015$AVEDRNK2[LLCP2015$AVEDRNK2 %in% c(77,99)] <-NA
LLCP2015$AVEDRNK2<-as.numeric(LLCP2015$AVEDRNK2)

#X_EDUCAG (education)

LLCP2015$Education[LLCP2015$X_EDUCAG ==1] <-"Did not graduate High School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==2] <-"Graduated from High School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==3] <-"Attended College/Technical School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==4] <-"Graduated from College/Technical School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==9] <-NA
LLCP2015$Education<- as.factor(LLCP2015$Education)

#Outcome numeric variable
# poor mental health days: MENTHLTH (1-30, 88 = None, 77 = don't know, 99 = refused)

LLCP2015$MENTHLTH[LLCP2015$MENTHLTH %in% c(88,77,99)] <-NA
LLCP2015$MENTHLTH<-as.numeric(LLCP2015$MENTHLTH)

#The binary health outcome variable 
#HAVARTH3 (Ever told) you have some form of arthritis, rheumatoid arthritis, gout,
#lupus, or fibromyalgia? (Arthritis diagnoses include: rheumatism, polymyalgia rheumatica; 
#osteoarthritis (not osteporosis); tendonitis, bursitis, bunion, tennis elbow; carpal tunnel 
#syndrome, tarsal tunnel syndrome; joint infection, etc.)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

LLCP2015$arthritis[LLCP2015$HAVARTH3 ==1] <-"Yes"
LLCP2015$arthritis[LLCP2015$HAVARTH3 ==2] <-"No"
LLCP2015$arthritis[LLCP2015$HAVARTH3 %in% c(7,9)] <-NA
LLCP2015$arthritis<- as.factor(LLCP2015$arthritis)

#get rid of missing values
LLCP2015<-na.omit(LLCP2015)

brfss<-rbind(LLCP2014,LLCP2015)

BRFSS<-brfss[,c("Hinsurance","employ","gender","income","Age","Race","Urbanity","Region",
                "AVEDRNK2","Education","MENTHLTH","arthritis")]

sum(is.na(BRFSS))#there is no missing value

# Recode variables as indicated by the codebook

###########
#CREATE TRAIN and TEST SET
##########

#

dim(LLCP2014)[1]
dim(LLCP2015)[1]
dim(BRFSS)[1]

brfss.train<-BRFSS[1:dim(LLCP2014)[1],]
brfss.test<-BRFSS[(dim(LLCP2014)[1]+1):dim(BRFSS)[1],]
m.train<-BRFSS$MENTHLTH[1:dim(LLCP2014)[1]]
m.test<-BRFSS$MENTHLTH[(dim(LLCP2014)[1]+1):dim(BRFSS)[1]]
sc.train<-BRFSS$arthritis[1:dim(LLCP2014)[1]]
sc.test<-BRFSS$arthritis[(dim(LLCP2014)[1]+1):dim(BRFSS)[1]]


##linear regression 

lm.fit<-glm(MENTHLTH~.-arthritis-gender-Urbanity,data=brfss.train)
summary(lm.fit)
lm.pred<-predict(lm.fit,brfss.test)
#the mean square error of the test data was not change, but we tend to select the simple model without the gender and Urbanity
#we can see gender, urbanity are both not significant different!
lm.MSE<-mean((m.test-lm.pred)^2)




#logistic regression

lg.fit<-glm(arthritis~.-MENTHLTH-AVEDRNK2,family = binomial,data=brfss.train)
# Get fitted probabilities from test set error:
lg.pred <- predict(lg.fit,newdata=brfss.test, type = "response")
glm.pred <- rep("No", length(lg.pred))
glm.pred[lg.pred>0.5] <- "Yes"
# get vector of predicted classifications:
(class.table<-table(glm.pred,sc.test))
# and our crosstabulation of the predicted vs the actual classification
# Finally, our TEST ERROR RATE
b<-1-sum(diag(class.table))/sum(class.table)



















