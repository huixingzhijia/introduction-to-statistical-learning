
#8
library(ISLR)
lm.mpg<-lm(mpg~horsepower,data=Auto)
summary(lm.mpg)
#there is a relationship between the horsepower and mpg.The relationship is very strong. (p-value<0.001)
#as the horsepower increase the mpg decrease. Their relationship is negative

#produce confidence interval and prediction intervals by using the interval=confidence or prediction
#What is the predicted mpg associated with a horsepower of 98? What are the associated 
#95% confidence and prediction intervals?

predict(lm.mpg,data.frame(horsepower=(c(98))),interval="confidence",level=0.95)
predict(lm.mpg,data.frame(horsepower=(c(95))),interval="prediction",level=0.95)

#plot(x,y) and the lm(y~x)
with(Auto,plot(horsepower,mpg))
abline(lm.mpg,col="red")
plot(predict(lm.mpg),residuals(lm.mpg))
plot(predict(lm.mpg),rstudent(lm.mpg))
#on the basis on the residuals plot, there is some evidence of non-linearity.
#leverage statistics can be computed for any number of predictors using the hatvalues() function
plot(hatvalues(lm.mpg))
#identify the index of the largest element of a vector
which.max(hatvalues(lm.mpg))
#Rmarkdown :To calculate the residual error relative to the response we use the mean of the response and the RSE. 
#The mean of mpg is r mean(mpg, na.rm=T). The RSE of the lm.fit was r 4.906 which indicates a percentage 
#error of r 4.906/mean(mpg, na.rm=T) * 100.0%. The $R^2$ of the lm.fit was about r summary(lm.fit)$r.sq, 
#meaning r summary(lm.fit)$r.sq * 100.0% of the variance in mpg is explained by horsepower.

#8(c) Based on the residual plot, there is evidence of non-linearity

#9
#a produce a scatterplot matrix which include all the variables
pairs(Auto)
#compute the matrix of correlations between the variables using the cor()
# exclude the name variable
cor(subset(Auto,select=-name))
#perform the multiple regression
lm.auto=lm(mpg~.-name,data=Auto)
summary(lm.auto)
lm.autor=lm(mpg~displacement+weight+year+origin,data=Auto)
#Yes. there is a relationship between the predictors and response.there is a relatioship between 
#the predictors and the response by testing the null hypothesis of whether all the regression coefficients 
#are zero. The F -statistic is far from 1 (with a small p-value), indicating evidence against the null hypothesis.
#displacement,weight,year and origin have a significant effect on mpg.  

#the coefficient of year
coefficients(lm.auto)["year"]
#the coefficient of year is 0.750773. It is means that as the year incease on unit the mpg is inceased in 0.750733
# In other words, cars become more fuel efficient every year by almost 1 mpg / year.

#d

#using plot and the object was using the regression object, it can produce the diagnosticplots for the regression
#fits. That's why we need par(mfrow=c(2,2)) to give four plots.
#Four diagnostic plots are automatically produced by applying the plot() function directly to the 
#output from lm()

par(mfrow=c(2,2))
plot(lm.auto)

par(mfrow=c(1,1))

plot(predict(lm.auto),residuals(lm.auto))
plot(predict(lm.auto),rstudent(lm.auto))
#on the basis on the residuals plot, there is some evidence of non-linearity.
#leverage statistics can be computed for any number of predictors using the hatvalues() function
plot(hatvalues(lm.mpg))
#identify the index of the largest element of a vector
which.max(hatvalues(lm.mpg))

#The fit does not appear to be accurate because there is a discernible curve pattern to the residuals plots.
#From the leverage plot, point 14 appears to have high leverage, although not a high magnitude residual.
#There are possible outliers as seen in the plot of studentized residuals because there are data with 
#a value greater than 3.

#e 
names(Auto)
#after fitting all the interactions we found that the following interaction was significant different
#check the scatterplot we can found that some variable have linearity
lm.fit3=lm(mpg~cylinders*displacement+displacement*year+acceleration*year+acceleration*origin,data=Auto )
summary(lm.fit3)

#f: try a few different transformations of the variables, such as log(x), sqrt(x), x^2.
lm.autor=lm(mpg~displacement+weight+year+origin,data=Auto)
lm.fit4 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2),data=Auto)
summary(lm.fit4)
par(mfrow=c(2,2))
plot(lm.fit4)
summary(lm.autor)
#Apparently, from the p-values, the log(weight), sqrt(horsepower), and acceleration^2 all have 
#statistical significance of some sort. The residuals plot has less of a discernible pattern than the 
#plot of all linear regression terms. The studentized residuals displays potential outliers (>3). 
#The leverage plot indicates more than three points with high leverage.
#However, 2 problems are observed from the above plots: 
#1) the residuals vs fitted plot indicates heteroskedasticity (unconstant variance over mean) in the model. 
#2) The Q-Q plot indicates somewhat unnormality of the residuals.

#So, a better transformation need to be applied to our model. From the correlation matrix in 9a.,
#displacement, horsepower and weight show a similar nonlinear pattern against our response mpg. 
#This nonlinear pattern is very close to a log form. So in the next attempt, we use log(mpg) as our 
#response variable.

lm.autol=lm(log(mpg)~displacement+weight+year+origin,data=Auto)
summary(lm.autol)
par(mfrow=c(2,2)) 
plot(lm.autol)
# log transform of mpg yield better model fitting (better R^2, normality of residuals).
par(mfrow=c(1,1)) 
plot(predict(lm.autol),rstudent(lm.autol))

#10
#fit a multiple regression model to predict sales using price, urban and us
#(a)
library(ISLR)
names(Carseats)
lm.reg=lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.reg)

#(b)provide an interpretation of each coefficient in the model
#Price
#The linear regression suggests a relationship between price and sales given the low p-value of the t-statistic. 
#The coefficient states a negative relationship between Price and Sales: as Price increases,Sales decreases.

#UrbanYes
#The linear regression suggests that there isn't a relationship between the location of the store and 
#the number of sales based on the high p-value of the t-statistic.

#USYes
#The linear regression suggests there is a relationship between whether the store is in the US or not
#and the amount of sales. The coefficient states a positive relationship between USYes and Sales: 
#if the store is in the US, the sales will increase by approximately 1201 units.

#(c)
#sales=13.0434-0.054459*Price+1.200573*US(Yes)-0.021916*UrbanYes

#(d)
#the UrbanYes is not significant different, beta is not zero

#(e) fit a smaller model that only uses the predictors for which there is evidence of association with the outcome
lm.fitm = lm(Sales ~ Price + US,data=Carseats)
summary(lm.fitm)

#(f)
#Based on the RSE and R^2 of the linear regressions, they both fit the data similarly,
#with linear regression from (e) fitting the data slightly better.

#(g) using the model from (e), obtan 95% confidence intervals for the coefficients
confint(lm.fitm)

#(h)
#Is there evidence of residuals like outliers in the model e
plot(predict(lm.fitm),rstudent(lm.fitm))
#All studentized residuals appear to be bounded by 
#-3 to 3, so not potential outliers are suggested from the linear regression.

#Is there evidence of high leverage observations in the model e
par(mfrow=c(2,2))
plot(lm.fitm)
#There are a few observations that greatly exceed $(p+1)/n$ (r 3/397) on the 
#leverage-statistic plot that suggest that the corresponding points have high leverage.

#11
#we investgate the t-statistic for the null hypothesis in simple linear regression without an intercept
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

#a perform a simple linear regression of y onto x, without an intercept.
lm.11=lm(y~x+0)
summary(lm.11)
#The p-value of the t-statistic is near zero so the null hypothesis is rejected.

#b) perform a simple linear regression of x onto y without an intercept
lm.11b=lm(x~y+0)
summary(lm.11b)
confint(lm.11b)
#The p-value of the t-statistic is near zero so the null hypothesis is rejected.

#c) what is the relationship between the results obtained in a and b
#Both results in (a) and (b) reflect the same line created in 11a. 

#d
(sqrt(length(x)-1))*sum(x*y)/(sqrt(sum(x^2)*sum(y^2)-(sum(x*y))^2)) 
#this is same with the regression calculation about Standard Error

#e: argue that the t-statistic for the regression of y onto x is the same as the t-statistic
#If you swap t(x,y) as t(y,x), then you will find t(x,y) = t(y,x).

#f: In R, show that when regression 
lm.fit = lm(y~x)
lm.fit2 = lm(x~y)
summary(lm.fit)
summary(lm.fit2)
#You can see the t-statistic is the same for the two linear regressions.

#12 (a) Under what circumstance is the coefficient estimate for the regression of X onto Y the same as the 
#coefficient estimate for the regression of Y onto X
#When the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values.

#(b) Generate an example in R with n=100 observations in which the coefficient estimate for the regression
# of X onto Y is different from the coefficient estimate for the regression of Y onto X
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
#The regression coefficients are different for each linear regression.

#(c)Generate an example in R with n=100 observations in which the coefficient estimate for the regression
# of X onto Y is same as the coefficient estimate for the regression of Y onto X
set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)

#The regression coefficients are the same for each linear regression. So long as sum sum(x^2) = sum(y^2) 
#the condition in 12a. will be satisfied. Here we have simply taken all the $x_i$ in a different order 
#and made them negative.

#13(a)using the rnor() funcition, create a vector, x containing 100
#obs drawn from a N(0,1) distribution
set.seed(1)
x = rnorm(100)

#(b) create a vector, eps, containing 100 obs drawn from N(0,0.25),
#variance 0.25, mean zero. rnorm(n, mean = 0, sd = 1)
eps=rnorm(100,0,sqrt(0.25))

#(c)using x and eps, generate a vector y accoriding the model
#y=-1+o.5x+elispon
y=-1+0.5*x+eps
length(y)
#length of vector y is 100, beta(zero) is -1, beta(1) is 0.5

#(d)
plot(x,y)
#there is a positive relationship between x and y

#(e)
lm.fit=lm(y~x)
summary(lm.fit)
#The linear regression fits a model close to the true value of the coefficients as was constructed. The model
#has a large F-statistic with a near-zero p-value so the null hypothesis can be rejected.
#beta(zero) is -1.01885, beta(one) is 0.49947

#(f)col=2 is red, col=3 is green
plot(x,y)
abline(lm.fit)
abline(lm.fit, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

#(g)
lm.po=lm(y~x+I(x^2))
summary(lm.po)
#There is evidence that model fit has increased over the training data given the slight increase in r square
#and RSE. Although, the p-value of the t-statistic suggests that there isn't a relationship between y and x^2.

#(h)Decrease the variance of the normal distribution so there is less noise in the data
eps=rnorm(100,0,sqrt(0.125))
y1=-1+0.5*x+eps
plot(x,y1)
lm.fit1=lm(y1~x)
summary(lm.fit1)
abline(lm.fit1)
abline(lm.fit1, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
#As the data was less noise by reducing the variance, we can see that the model has less error term 

#(i)Increase the variance of the normal distribution so there is more noise in the data
eps=rnorm(100,0,sqrt(0.5))
y2=-1+0.5*x+eps
plot(x,y2)
lm.fit2=lm(y2~x)
summary(lm.fit2)
abline(lm.fit2)
abline(lm.fit2, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
#As we can see, the residual standard error was increased since the data was more noise

#(j)What are the confidence intervals for beta(0) and beta(1) based on the original data set.
#confint was used to give the confidence interval for the coefficient
confint(lm.fit)
confint(lm.fit1)
confint(lm.fit2)
#All intervals seem to be centered on approximately 0.5, with the second fit's interval being narrower than
#the first fit's interval and the last fit's interval being wider than the first fit's interval.

#14(a)
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
#the beta(0) is 2 and beta(1) is 2 and beta(2) is 0.3.

#(b)What is the correlation between x1 and x2? it is 0.5
plot(x1,x2)

#(c)
lm.xy=lm(y~x1+x2)
summary(lm.xy)
# the coefficient are close to the true coefficient.
#beta(0) is 2.13, beta(1) is 1.4396 and beta(2) is 1.0097 We can't reject the null hypothesis that beta(2) is 
#zero because, the p-value is greater than 0.5, we can reject the null hypothesis that beta(1) is zero

#(d)
lm.x=lm(y~x1)
summary(lm.x)
#Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its
#t-statistic is near zero

#e
lm.y=lm(y~x2)
summary(lm.y)
#Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.

#f
#No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed upon
#together. When they are regressed upon separately, the linear relationship between y and each predictor 
#is indicated more clearly.

#g
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)
lm.xy=lm(y~x1+x2)
summary(lm.xy)
lm.x=lm(y~x1)
summary(lm.x)
lm.y=lm(y~x2)
summary(lm.y)
plot(lm.xy)
par(mfrow=c(2,2))
plot(lm.xy)
par(mfrow=c(2,2))
plot(lm.x)
par(mfrow=c(2,2))
plot(lm.y)
#In the first and third models, the point becomes a high leverage point.

plot(predict(lm.xy), rstudent(lm.xy))
plot(predict(lm.x), rstudent(lm.x))
plot(predict(lm.y), rstudent(lm.y))
#Looking at the studentized residuals, we don't observe points too far from the |3| value cutoff, 
#except for the second linear regression: y ~ x1.

plot(hatvalues(lm.xy))


#15
#a
library(MASS)
summary(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas) 
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes

#b
lm.all = lm(crim~., data=Boston)
summary(lm.all)
#zn, dis, rad, black, medv we reject the null hypothesis

#c
#Its coefficient in a simple linear regression model is shown on the x-axis, and its coefficient estimate in the
#multiple linear regression model is shown on the y-axis
#coefficients function is only used for extracting the estimate coefficient, nothing else
#if we use summary and coefficient variable, it can be used to extract all the paramters with row and columns 
#if we are use the summary(object)$coefficient[n] with a single number, we need count the value from first row,
#first column to next
coefficients(lm.zn)[2]
#the followings are same
coefficients(lm.zn)
lm.zn$coefficients
#the followings are same
summary(lm.zn)$coefficients[5]
summary(lm.zn)$coefficients[1,3]
summary(lm.zn)$fstatistic[1]
summary(lm.zn)$r.squared
#once again, the coefficients are only used for 
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
y
plot(x, y)

#Coefficient for nox is approximately -10 in univariate model and 31 in multiple regression model.

#d Is there evidence of non-linear association between any of the predictors and the response?
lm.zn=lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3
# lm.chas = lm(crim~poly(chas,3)) : qualitative predictor
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3
lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1, 2
lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1, 2, 3
lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1, 2, 3
lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1, 2
lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1, 2
lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1, 2, 3
lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1
lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1, 2
lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # 1, 2, 3

#the answer is yes for most, except for black and chas.











