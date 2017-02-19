#6
library(ISLR)
library(boot)
par(mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
set.seed(14)
#since if the polynominal is greater than 4 or 5 tends to overfit, so we just test upto 5-degree
cross.error=rep(0,5)
for (i in 1:5) {
  lm.poly<- glm(wage~poly(age,i),data=Wage)
  cross.error[i]<-cv.glm(Wage,lm.poly,K=5)$delta[1]
}

which.min(cross.error)

#the minimum is degree of 5, but we can see the test error didn't change much  from degree 3 to 5. So I will choose degree of 3 as the optimal one

plot(1:5,cross.error,type = "l",ylab = "cross validation error",xlab = "degree of freedom")

#using ANOVA
lm<-glm(wage~age,data=Wage)
lm.fit.2<-glm(wage~poly(age,2),data = Wage)
lm.fit.3<-glm(wage~poly(age,3),data = Wage)
lm.fit.4<-glm(wage~poly(age,4),data = Wage)
lm.fit.5<-glm(wage~poly(age,5),data = Wage)

#method 1-ANOVA
anova(lm,lm.fit.2,lm.fit.3,lm.fit.4,lm.fit.5,test = "Chisq")
#The anova results indicate that the degree of 3 is sufficient. The degree of 4 is kind of at the critical point


#make a plot of the resulting polynominal fit to the data

agelimits<-range(Wage$age)

#the starting and (maximal) end values of the sequence. Of length 1 unless specify
age.range<-seq(from=agelimits[1],to=agelimits[2])
preds<-predict(lm.fit.3,newdata=list(age=age.range),se=T)
band<-cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

#A numerical vector of the form c(bottom, left, top, right)

par(mfrow=c(1,1))
with(Wage,plot(age,wage,xlim=agelimits,cex=.5,col="darkgrey"))

title("Degree-3 Polynomial",outer = T)

with(Wage,lines(age.range,preds$fit,lwd=2,col="blue"))

matlines(age.range,band,lwd=1,col="blue")

#b
#fit a step function to predict wage using age and perform cross-validation to choose the optimal number of cuts
#i is the interval cut into i interval, there is no 1 interval
#corss-validation only can be used glm object, even it was linear use glm without specify family

#METHOD 1
step.cv<-rep(NA,10)
for (i in 2:10) {
  Wage$age.cut<-cut(Wage$age,i)
  fit.step<-glm(wage~age.cut,data = Wage)
  step.cv[i]<-cv.glm(Wage,fit.step,K=10)$delta[1]
}


which.min(step.cv)

#Method 2
step.cv<-rep(NA,10)
for (i in 2:10) {
  
  fit.step<-glm(wage~cut(age,i,labels = FALSE),data = Wage)
  step.cv[i]<-cv.glm(Wage,fit.step,K=10)$delta[1]
}

which.min(step.cv)


#the minimum is degree of 5, but we can see the test error didn't change much  from degree 3 to 5. So I will choose degree of 3 as the optimal one

plot(1:10,step.cv,type = "l",ylab = "cross-validation errors",xlab = "degree of freedom")

#we can see that 8 cuts will give best fit 

fit.step.op<-glm(wage~cut(age,8),data = Wage)

agelimits<-range(Wage$age)
age.range<-seq(from=agelimits[1],to=agelimits[2])
pred.step<-predict(fit.step.op,data.frame(age=age.range))

#Create plot for step function

par(mfrow=c(1,1))
with(Wage,plot(age,wage,xlim=agelimits,cex=.5,col="darkgrey"))
title("Degree-8 step function",outer = T)
lines(age.range, pred.step, col="red", lwd=2)

#7
#investigate the wage to jobclass and race are categorical variables

summary(Wage$race)
summary(Wage$jobclass)

par(mfrow=c(1,2))
with(Wage,plot(Wage$maritl,wage))
with(Wage,plot(Wage$jobclass,wage))

#Unable to fit splines on categorical variables.

library(gam)
#usig genearlized additive model
gam.0<-gam(wage~lo(age,span=0.7)+s(year,df=4),data = Wage)
gam.1<-gam(wage~lo(age,span=0.7)+s(year,df=4)+race,data = Wage)
gam.2<-gam(wage~lo(age,span=0.7)+s(year,df=4)+jobclass,data = Wage)
#Model 4
gam.3<-gam(wage~lo(age,span=0.7)+s(year,df=4)+race+jobclass,data = Wage)

anova(gam.0,gam.1,gam.2,gam.3,test="F")

#Based on the ANOVA results, M4 is perfered. using linear fit of race and jobclass. 
par(mfrow=c(2,2))
plot(gam.3,se=T,col="blue")

#8
pairs(Auto)
gam.1<-glm(mpg~displacement+weight+horsepower,data = Auto)
gam.2<-gam(mpg~poly(displacement,2)+lo(weight,span = 0.2)+s(horsepower,df=2),data = Auto)
gam.3<-gam(mpg~poly(displacement,3)+lo(weight,span = 0.2)+s(horsepower,df=2),data = Auto)
gam.4<-gam(mpg~poly(displacement,2)+lo(weight,span = 0.2)+s(horsepower,df=4),data = Auto)

anova(gam.1,gam.2,gam.3,gam.4,test = "F")
#The test shows that there is no necessary for displacement go higher degree polynominal, but the horsepower is necessary for higher degree
#there is nonlinear relationship between mpg to displacement, horsepower and weight
par(mfrow=c(1,3))
plot(gam.4,se=T,col="blue")

#9

##a
library(MASS)
glm.boston<-glm(nox~poly(dis,3),data=Boston)
coef(summary(glm.boston))

dislimit<-range(Boston$dis)
par(mfrow=c(1,1))
dis.grid<-seq(from=dislimit[1],to=dislimit[2],by=0.1)
preds<-predict(glm.boston,newdata=list(dis=dis.grid),se=T)
se.bands<-cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(Boston$dis,Boston$nox,xlab="weighted mean of distances to five Boston 
     employment centres",ylab="nitrogen oxides concentration")
title("Cubic Polynominal Regression")
lines(dis.grid,preds$fit,lwd=2,col="red")
matlines(dis.grid,se.bands,lwd=1,col="red")

##b
for (i in 1:10){
  glm.boston<-lm(nox~poly(dis,i,raw =T),data=Boston)
  pred.fit<-predict(glm.boston,newdata=Boston)
}
rss.sum<-sum((Boston$dis-pred.fit)^2)
rss.sum
#The sum of square of residual is 7746.625




#c

#perform cross validation for selecting the i
cross.error<-rep(0,10)
for (i in 1:10) {
  glm.boston<- glm(nox~poly(dis,i,raw = T),data=Boston)
  cross.error[i]<-cv.glm(Boston, glm.boston,K=10)$delta[1]
  pred<-predict(glm.boston,newdata=list(dis=dis.grid),se=T)
  pred.fit<-predict(glm.boston,newdata=Boston)
  rss.sum<-sum((Boston$dis-pred.fit)^2)
}
cross.error
which.min(cross.error)
#the polynominal degree of 3 gives the minimun cross-validation error
#plot the test error versus polynominal degree
plot(1:10,cross.error,type = "l", ylab="Cross-validation errors",xlab="degree of polynominal")
#Th cross-validation errors shows that 2 to 5 degree are almost same 7 degree and 9 degree tend to overfit
#the data.


##d
summary(Boston$dis)
#Becuase we need to use the degree of freedom is 4 and we need four interval, we need three cut points,
#I pick 1st, mean and 3rd quantitle to cut the interval. the points are 2, 4 and 6

spline.boston<- glm(nox~bs(dis,df=4,knot=c(2,4,6)),data=Boston)
summary(spline.boston)
dislimit<-range(Boston$dis)
dis.grid<-seq(from=dislimit[1],to=dislimit[2])

sp.pred<-predict(spline.boston,newdata = list(dis=dis.grid),se=T)

se.band<-cbind(sp.pred$fit-2*sp.pred$se.fit,sp.pred$fit+2*sp.pred$se.fit)
with(Boston,plot(dis,nox))
lines(dis.grid,sp.pred$fit,type="l",col="red")
matlines(dis.grid,se.band,type = "l",col = "red")


#e
rss<-rep(0,15)
for (i in 1:15) {
dislimit<-range(Boston$dis)
dis.grid<-seq(from=dislimit[1],to=dislimit[2])

sr.fit<- glm(nox~ns(dis,df=i),data=Boston)

rss[i]<-sum(sr.fit$residuals^2)

}
plot(1:15,rss,type = "l",xlab = "Degree of Freedom",ylab = "Residual Sum of Square")

#The RSS was decrease while the degree of freedom was increase, because we only use the training data, it tend to be overfit


##f
#we add traing and test data set in selecting the results

k=10
opt.df=11
set.seed(1)
folds<-sample(1:k,nrow(Boston),replace = T)
cv.errors=matrix(NA,k,opt.df-2,dimnames=list(NULL,paste(3:opt.df)))

for(j in 1:k){
  for(i in 3:opt.df){
    gam.fit<-lm(nox~bs(dis,df=i),data=Boston[folds!=j,])
    
    pred<-predict(gam.fit,Boston[folds==j,])
    cv.errors[j,i-2]=mean((Boston$nox[folds==j]-pred)^2)
  }
}
#calculate the mean errors over the column, if you want to calculate over row, then use 1
#col.sums <- apply(x, 2, sum)
#row.sums <- apply(x, 1, sum)

mean.cv.errors<-apply(cv.errors,2,mean)
mean.cv.errors
mean.cv.errors[which.min(mean.cv.errors)]
#It seems like degree of 5 will give the optimal results

#10

##a

library(ISLR)
library(leaps)
names(College)

train<-sample(dim(College)[1],1/2*dim(College)[1])
best.sub<-regsubsets(Outstate~.,data=College[train,],method = "forward",nvmax=17)

reg.summary<-summary(best.sub)

par(mfrow=c(1,3))
plot(reg.summary$cp,xlab="Number of Variables",ylab="AIC",type="l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

#It seems like 10 variable gives minimum AIC and 8 variables gives minimum BIC. 
plot(reg.summary$bic,xlab="Number of variables",ylab="BIC",type="l")
which.min(reg.summary$bic)
points(8,reg.summary$bic[8],col="red",cex=2,pch=20)

plot(reg.summary$adjr2,xlab="Number of variables",ylab="Adjusted RSQ",type="l")
which.max(reg.summary$adjr2)
points(13,reg.summary$adjr2[13],col="red",cex=2,pch=20)

#It seems like 8 variable will give be the optimal model, because according to AIC all the values are almost 
#same after 8 variables, there is no significant increase or decrease
coefi <- coef(best.sub, id = 8)
names(coefi)
#The six variables are "PrivateYes" ,"Accept" ,"F.Undergrad","Room.Board", "PhD", "perc.alumni"
# "Expend" "Grad.Rate"

##b

library(gam)
par(mfrow=c(1,1))
#(College,plot(Grad.Rate,Outstate))
gam.fit<-gam(Outstate~Private+s(Accept,df=4)+lo(F.Undergrad,span = 0.4)+Room.Board+ns(PhD,df=4)+
               perc.alumni+s(Expend,df=4)+ns(Grad.Rate,df=4),data=College[train,])
summary(gam.fit)
#It seems like the 
par(mfrow=c(3,3))
plot.gam(gam.fit,se=T)
#the variable perc.alumni and roomboard is has more linear relationship to the Outstate tution, and Accept,
#undergrad, phd and grad rate are more non-linear


##c

gam.pred<-predict(gam.fit, College[-train,])
err<-mean((College$Outstate[-train]-gam.pred)^2)
err

gam.tss<-mean((College$Outstate[-train]-mean(College$Outstate[-train]))^2)
test.rss<-1-err/gam.tss
test.rss

#linear model R square
glm.fit<-glm(Outstate~Private+Accept+F.Undergrad+Room.Board+PhD+
  perc.alumni+Expend+Grad.Rate,data=College[train,])
summary(glm.fit)
glm.pred<-predict(glm.fit, College[-train,])
glm.err<-mean((College$Outstate[-train]-glm.pred)^2)


glm.tss<-mean((College$Outstate[-train]-mean(College$Outstate[-train]))^2)
glm.test.rss<-1-glm.err/gam.tss
glm.test.rss
#The test R square is 0.8255285 for GAM model, which is higher than linear model. 
#The GAM model is improved compared to linear model

#d which variables, if any, is there evidence of a non-linear relationship with response

summary(gam.fit)
#The Accept, F.Undergrad,Expend shows a nonlinear relationship. The room.board and perc.alumin showed linear relationship

#11

#a
set.seed(10)
x1<-rnorm(100)
x2<-rnorm(100)
eps<-rnorm(100,sd=0.1)
y=-2+1.5*x1+2*x2+eps

#b
beta0<- rep(NA,1000)
beta1<-rep(NA,1000)
beta2<-rep(NA,1000)
beta1[1]=10

#c-e
#
for (i in 1:1000){
  a=y-beta1[i]*x1
  beta2[i]=lm(a~x2)$coef[2]
  a=y-beta2[i]*x2
  lm.fit=lm(a~x1)
  if (i < 1000){
  beta1[i+1]=lm.fit$coef[2]
  }
  beta0[i]=lm.fit$coef[1]
}

par(mfrow=c(1,1))
plot(1:1000,beta0,type = "l",xlab = "iteration",ylab = "betas",
     ylim=c(-4,4),col="green")
lines(1:1000,beta1,col="red")
lines(1:1000,beta2,col="blue")
legend("right",c("beta0","beta1","beta2"),lty = 1,
       col=c("green","red","blue"))

#the coefficient decrease and obtain a steady value quickly

#f
plot(1:1000,beta0,type = "l",xlab = "iteration",ylab = "betas",
     ylim=c(-4,4),col="green")
lines(1:1000,beta1,col="red")
lines(1:1000,beta2,col="blue")
lm.fit<-lm(y~x1+x2)
abline(h=lm.fit$coef[1],lty="dashed")
abline(h=lm.fit$coef[2],lty="dashed")
abline(h=lm.fit$coef[3],lty="dashed")
legend("right",c("beta0","beta1","beta2","multiple regression"),lty = c(1,1,1,2),cex=0.5,
       col=c("green","red","blue","black"))
#from the plot, we can see that the dot line which is multiple regression coefficients, is exactly
#the same with the estimate coefficients


#g
#When the y and x relationship is linear, the iteration is not as much as possible


#12

set.seed(11)
p=100
n=100
x=matrix(ncol = p,nrow = n)
coefi<-rep(NA,p)
for (i in 1:p){
  x[,i]=rnorm(n)
  coefi[i]=rnorm(1)*100

}
y=x%*%coefi+rnorm(n)

beta=rep(0,p)
m_i=1000
error<-rep(NA,m_i+1)
e=2
error[1]=Inf
error[2]=sum((y-x%*%beta)^2)
threshold<-1e-04
while (e < m_i && error[e - 1] - error[e] > threshold){
  for (i in 1:p){
    a=y-x%*%beta+beta[i]*x[,i]
    beta[i]=lm(a~x[,i])$coef[2]
  }
  e=e+1
  error[e]=sum((y-x%*%beta)^2)
  print(c(e-2,error[e-1],error[e]))
}

#It seems like after 10 iteration, there is not too much change, although 78 iteration will gives us minimum value,

plot(1:20,error[3:22],type = "l")
points(10,error[12],col="red")

















