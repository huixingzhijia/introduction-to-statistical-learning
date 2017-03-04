#8 
#a
library(leaps)
library(glmnet)
library(pls)
library(ISLR)
library(MASS)

set.seed(1)
x=rnorm(100)
eps=rnorm(100)

#b
beta_0=1
beta_1=2
beta_2=4
beta_3=5
y=beta_0+beta_1*x+beta_2*x^2+beta_3*x^3+eps

#c
data<-data.frame(x,y)
bestsubset<-regsubsets(y~poly(x,10,raw=T),data=data,nvmax=10)
bs.summary<-summary(bestsubset)
names(bs.summary)
#create a function:
plot.bestsummary <- function(reg.summary) {
  par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
  plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(reg.summary$adjr2, xlab = "Number of Variables", 
       ylab = expression(paste("Adjusted ", R^2)), type = "l")
  points(which.max(reg.summary$adjr2), 
         reg.summary$adjr2[which.max(reg.summary$adjr2)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = expression(C[p]), 
       type = "l")
  points(which.min(reg.summary$cp), 
         reg.summary$cp[which.min(reg.summary$cp)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(reg.summary$bic), 
         reg.summary$bic[which.min(reg.summary$bic)], 
         col = "red", cex = 2, pch = 20)
}

plot.bestsummary(bs.summary)

#using adjusted-Rsquare
plot(bs.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(bs.summary$adjr2,xlab="Number of variables",ylab="Adjusted RSQ",type="l")
which.max(bs.summary$adjr2)
points(4,bs.summary$adjr2[4],col="red",cex=2,pch=20)
#number of variable is 4 gives the maximum adjusted R square, but we can see there is no much difference between
#3 and 4

#Using AIC method
plot(bs.summary$cp,xlab="Number of Variables",ylab="AIC",type="l")
which.min(bs.summary$cp)
points(3,bs.summary$cp[3],col="red",cex=2,pch=20)
#number of variable is 3 gives the minimum AIC 

#Using BIC method
plot(bs.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
which.min(bs.summary$bic)
points(3,bs.summary$bic[3],col="red",cex=2,pch=20)
#number of variable is 3 gives the minimum BIC

#So the 3 variable model will give the best model 
coef(bestsubset,3)
#the best three variables are x, x^2, and x^3



#d use the method="forward"
regfit.fwd<-regsubsets(y~poly(x,10,raw=T),data=data,nvmax=10,method="forward")
fwd.summary<-summary(regfit.fwd)

#foward adjust R square

plot.bestsummary(fwd.summary)

#So the 3 variable model will give the best model 
coef(regfit.fwd,3)


#backward
regfit.bwd<-regsubsets(y~poly(x,10,raw=T),data=data,nvmax=10,method="backward")
bwd.summary<-summary(regfit.bwd)

#Making the plot
plot.bestsummary(bwd.summary)


#So the 4 variable model will give the best model 
coef(regfit.bwd,4)


#The backward method pick 4 variables, they are X^1,x^2,x^3,x^9. the forward pick 4 variables, 
#they are X^1,x^2,x^3,x^5. The best subset pick 3 variables, they are X^1,x^2,x^3.

#e
#because the glmnet only take care of matrix, don't take care of the data.frame, so we have to 
#build matrix.
x.matrix<-model.matrix(y~poly(x,10,raw=T),data)[,-1]
set.seed(1)
cv.lasso.mod<-cv.glmnet(x.matrix,y,alpha=1)
#alpha controls the model lasso or ridge, plot the mean square error to log
#coefficient plot that depending on the choice of tuning parameter, some of the coefficients will be exactly equal to zero
plot(cv.lasso.mod)

best.lam<-cv.lasso.mod$lambda.min
best.lam
best.model<-glmnet(x.matrix,y,alpha=1)

#using the type=coefficients to run on the whole data set, But if you substitute type=coefficients using 
#newx=testdataset, you test the model on the test dataset

lasso.pred<-predict(best.model,s=best.lam,type="coefficient")

lasso.pred
#lasso picks X,x^2,x^3 with negligible coefficients

#f
beta_7=3.5
y=beta_0+beta_7*x^7+eps

data<-data.frame(x,y)

best.subset<-regsubsets(y~poly(x,10),data=data,nvmax=10)
b.summary<-summary(best.subset)

plot.regsummary <- function(reg.summary) {
  par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
  plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(reg.summary$adjr2, xlab = "Number of Variables", 
       ylab = expression(paste("Adjusted ", R^2)), type = "l")
  points(which.max(reg.summary$adjr2), 
         reg.summary$adjr2[which.max(reg.summary$adjr2)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = expression(C[p]), 
       type = "l")
  points(which.min(reg.summary$cp), 
         reg.summary$cp[which.min(reg.summary$cp)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(reg.summary$bic), 
         reg.summary$bic[which.min(reg.summary$bic)], 
         col = "red", cex = 2, pch = 20)
}
plot.regsummary(b.summary)

#So the 7 variable model will give the best model 
coef(best.subset,7)
#poly(x, 10)1 poly(x, 10)2 poly(x, 10)3 poly(x, 10)4 poly(x, 10)5 poly(x, 10)6 poly(x, 10)7 


#f lasso
x.matrix<-model.matrix(y~poly(x,10,raw=T),data)[,-1]
set.seed(1)
cv.lasso<-cv.glmnet(x.matrix,y,alpha=1)

#coefficient plot that depending on the choice of tuning parameter, some of the coefficients will be exactly equal to zero

best.lam<-cv.lasso$lambda.min
best.lam
best.model<-glmnet(x.matrix,y,alpha=1)

#using the type=coefficients to run on the whole data set, But if you substitute type=coefficients using 
#newx=testdataset, you test the model on the test dataset

lasso.pred<-predict(best.model,s=best.lam,type="coefficient")
lasso.pred
#lasso picks x^7 as the only predictors. Best subset picks 
#poly(x, 10)7 

#9


#a

sum(is.na(College))
#there is no missing variables

set.seed(1)
train<-sample(1:dim(College)[1],dim(College)[1]/2)
test<-(-train)
train<-College[train,]
test<-College[test,]
y.test<-test$Apps
y.train<-train$Apps

#b
lm.college<-lm(Apps~.,data=train)
lm.pred<-predict(lm.college,test)
mean((y.test-lm.pred)^2)
#The test error is 1480089

#c

#in general we could choose lambda using the built-in cross-validation function,cv.glmnet()
#by default, the function performs ten-fold cross-validation,(cv.glmnet) though this can be changed using the argument folds
set.seed(1)
train.mat<-model.matrix(Apps~.,data=train)
test.mat<-model.matrix(Apps~.,data=test)

cv.out<-cv.glmnet(train.mat,y.train,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam

#therefore the value of lambda that results in the smallest cross-validation error is 212. 
#what is MSE associated with this value of lambda
ridge.model<-glmnet(train.mat,y.train,alpha=0)
ridge.pred<-predict(ridge.model,s=bestlam,newx=test.mat)
mean((ridge.pred-y.test)^2)
#the test error is 1028694, there is improvement from least square 

#d
set.seed(1)
x.mat<-model.matrix(Apps~.,data=College)
y<-College$Apps
train.mat<-model.matrix(Apps~.,data=train)
test.mat<-model.matrix(Apps~.,data=test)

cv.out<-cv.glmnet(train.mat,y.train,alpha=1)
bestlam<-cv.out$lambda.min
bestlam#24.62086
lasso.model<-glmnet(train.mat,y.train,alpha=1)
lasso.pred<-predict(lasso.model,s=bestlam,newx=test.mat)
mean((lasso.pred-y.test)^2)

#using the full data
model<-glmnet(x.mat,y,alpha=1,lambda=grid)
lasso.coef<-predict(model,type="coefficients",s=bestlam)
lasso.coef
#the test error is 1030941, there is improvement from least square and also simple than ridge regression


#e
set.seed(1)
#scale=TRUE has the effect of standardizing each predictor,so the scale on which each variable
#is measured will ot have an effect, validation="CV" cause pcr() to compute the ten-fold cross-validation error for each
#possible value of M,the number of principal components used
#perform the PCR on the training data and test the performance on test data

pcr.fit<-pcr(Apps~.,data=train,scale=TRUE,valiation="CV")
validationplot(pcr.fit,val.type="MSEP")
summary(pcr.fit)

#from the plot we can see that M=10 explained good variability

#lowest cross-validation error occus when M=7, compute the test MSE
pcr.pred<-predict(pcr.fit,test,ncomp=10)
mean((pcr.pred-y.test)^2)
#the test error about 1505718,M is 10

#f
#Partial Least Squares, plsr()
set.seed(1)
pls.fit<-plsr(Apps~.,data=train,scale=TRUE,valiation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
#the lowest cross-validation error occurs when only M=2 partial least squares directions are used.
pls.pred<-predict(pls.fit,test,ncomp=5)
mean((pls.pred-y.test)^2)
#the test error is 1158597 and the M is 5

#g
lm.test.error <- mean((y.test-lm.pred)^2)
ridge.test.error <- mean((ridge.pred-y.test)^2)
lasso.test.error <- mean((lasso.pred-y.test)^2)
pcr.test.error<- mean((pcr.pred-y.test)^2)
pls.test.error <- mean((pls.pred-y.test)^2)
barplot(c(lm.test.error, ridge.test.error, lasso.test.error, pcr.test.error, pls.test.error), col="red", 
        names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test error")

#The plot shows that PCR gives the higest test error, Lasso gives the lowest test error.OLS, and PLS are almost same
#Expected PCR, there are not too much difference.


#10 
#a
set.seed(1)
p <- 20
n <- 1000
x <- matrix(rnorm(n * p), n, p)
B <- rnorm(p)
B[3] <- 0
B[4] <- 0
B[9] <- 0
B[19] <- 0
B[10] <- 0
eps <- rnorm(p)
y <- x %*% B + eps

#b
train<-sample(seq(1000),100,replace = F)

data<-data.frame(x=x,y=y)
data.train<-data[train,]
data.test<-data[-train,]
y.train<-data$y[train]
y.test<-data$y[-train]
#c

# perform best subsets selection on training subset
ref.fit.best <- regsubsets(y ~ ., data = data.train, nvmax = p)
# now create model matrix (matrix of x variables) from test data
test.mat <- model.matrix(y ~.,data = data.train)
# create empty vector to hold Test MSE for each model
val.errors <- rep(NA, p)
# loop through each model (1:19), calculating validation error for each:
for (i in 1:p){
  coefi <- coef(ref.fit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((y.train-pred)^2)
}

plot(val.errors, ylab="Training MSE", pch=19, type="b")

# Which of those has smallest testing error?
which.min(val.errors)
coef(ref.fit.best, 10)

#It seems like after 10 variable there is not too much change


#d
# create empty vector to hold Test MSE for each model
val.errors <- rep(NA, p)
test.mat <- model.matrix(y ~.,data = data.test)
# loop through each model (1:20), calculating validation error for each:
for (i in 1:p){
  coefi <- coef(ref.fit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((y.test-pred)^2)
}

plot(val.errors, ylab="Testing MSE", pch=19, type="b")


#e
which.min(val.errors)
#15 parameter model has the smallest test MSE.


#f

coef(ref.fit.best, id=15)
#It is different with the training MSE, because
#As it stated, as more predictors in the model, the mean square error of training dataset
#is increasing. However the test dataset is not.
#it is consistant with my equation in a, because I put beta_3,beta_4,beta_9,beta_10,
#beta-19 are equal to 0.The best subset result agrees with that.

#g
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
val.errors = rep(NA, p)
c = rep(NA, p)
d = rep(NA, p)
for (i in 1:p) {
  coefi = coef(ref.fit.best, id = i)
  c[i] = length(coefi) - 1
  d[i] = sqrt(sum((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + 
                sum(B[!(x_cols %in% names(coefi))])^2)
}
plot(x = c, y = d, xlab = "No. of coefficients", ylab = "Error between estimated and true coefficients")

which.min(d)

#Model with 9 coefficients (10 with intercept) minimizes the error between the estimated and true 
#coefficients. Test error is minimized with 16 parameter model. A better fit of true coefficients
#as measured here doesn't mean the model will have a lower test MSE.

#11

#best subset selection

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(Boston) - 1
folds = sample(rep(1:k, length = nrow(Boston)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i, ], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")
#we can see that 9 parameter gives the lowest RMSE

#Lasso
x<-model.matrix(crim~.-1,data=Boston)
y<-Boston$crim
cv.lasso<-cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
coef(cv.lasso)
cv.lasso$lambda.min

#test error 7.428573
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

#Ridge
cv.ridge<-cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)
cv.lasso$lambda.min
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])
#test error 7.893898


#PCR
pcr.fit<-pcr(crim~.-1,data=Boston,scale=TRUE,validation="CV")
summary(pcr.fit)
#13 component pcr fit has lowest CV/adjCV RMSEP.

#d-c
#I would choose the 9 parameter best subset model because it had the best cross-validated RMSE, also it was simpler.



















