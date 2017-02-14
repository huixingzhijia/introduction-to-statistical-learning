#college=read.csv("college.csv",header=T,na.strings="?")
#another way to fix the header
college=read.csv("college.csv")
#R given each row a name corresponding to the appropriate university. R will not try to perform calculations on the 
#row names
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
summary(college)
A<-college
pairs(A[,1:10])
pairs(college[,1:10])
names(college)

boxplot(college$Outstate,college$Private)
#create a new qualitative variable called elite by binning the top10perc  variable. 
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
#data frame is like table concatenate the tables
college=data.frame(college,Elite)
fix(college)
summary(college)
#there are 78 elite university
boxplot(college$Outstate, Elite,xlab="Outstate",ylab="Elite")
#data frame like concatenae
x <- 1:4; n <- 10; M <- c(10, 35); y <- 2:4
data.frame(x, n)
data.frame(x, M)
#it will give error if we put data.frame(x, y), because x and y  are differen size
#divde the print window into four regious so that four plots can be seen
#v
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)

#vi
par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
# High tuition correlates to high graduation rate.
plot(college$Accept / college$Apps, college$S.F.Ratio)
# Colleges with low acceptance rate tend to have low S:F ratio.
plot(college$Top10perc, college$Grad.Rate)
# Colleges with the most students from top 10% perc don't necessarily have
# the highest graduation rate. Also, rate > 100 is erroneous!

#9
Auto1=read.csv("Auto.csv",header=T,na.strings="?")
summary(Auto1)
fix(Auto1)
# quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin
# qualitative: name

range(Auto$mpg)
# 9.0 46.6
range(Auto$cylinders)
# 3 8
range(Auto$displacement)
# 68 455
range(Auto$horsepower)
# 46 230
range(Auto$weight)
# 1613 5140
range(Auto$acceleration)
# 8.0 24.8
range(Auto$year)
# 70 82
range(Auto$origin)
# 1 3

summary(Auto1)
mean(Auto$mpg); sd(Auto$mpg)
# 23.44592 7.805007
mean(Auto$cylinders); sd(Auto$cylinders)
# 5.471939 1.705783
mean(Auto$displacement); sd(Auto$displacement)
# 194.412 104.644
mean(Auto$horsepower); sd(Auto$horsepower)
# 104.4694 38.49116
mean(Auto$weight); sd(Auto$weight)
# 2977.584 849.4026
mean(Auto$acceleration); sd(Auto$acceleration)
# 15.54133 2.758864
mean(Auto$year); sd(Auto$year)
# 75.97959 3.683737
mean(Auto$origin); sd(Auto$origin)
# 1.576531 0.8055182

#remove 10th through 85th observation  d
#use the negative sign - in the index tells R to keep all rows or columns except those indicated in the index
newAuto = Auto1[-(10:85),]
mean(newAuto$mpg); sd(newAuto$mpg); range(newAuto$mpg)
# 24.40443 7.867283 (11.0 46.6)

mean(newAuto$cylinders); sd(newAuto$cylinders);
# 5.373418 1.654179
range(newAuto$cylinders)
# 3 8

mean(newAuto$displacement); sd(newAuto$displacement);
# 187.2405 99.67837
range(newAuto$displacement)
# 68 455

mean(newAuto$horsepower); sd(newAuto$horsepower); range(newAuto$horsepower)
# 100.7215
# 35.70885
# 46 230

mean(newAuto$weight); sd(newAuto$weight); range(newAuto$weight)
# 2935.972
# 811.3002
# 1649 4997

mean(newAuto$acceleration); sd(newAuto$acceleration);
# 15.7269
# 2.693721
range(newAuto$acceleration)
# 8.5 24.8

mean(newAuto$year); sd(newAuto$year); range(newAuto$year)
# 77.14557
# 3.106217
# 70 82

mean(newAuto$origin); sd(newAuto$origin); range(newAuto$origin)
# 1.601266
# 0.81991
# 1 3

#e and f, yes like weight and cylinder and year have related to mpg
pairs(Auto)
plot(Auto$mpg, Auto$weight)
# Heavier weight correlates with lower mpg.
plot(Auto$mpg, Auto$cylinders)
# More cylinders, less mpg.
plot(Auto$mpg, Auto$year)
# Cars become more efficient over time.


#10
library(MASS)
#it contains the dataset boston
Boston
#read about the data:506 rows and 14 columns.
?Boston
summary(Boston)
pairs(Boston)
names(Boston)
attach(Boston)
plot(Boston$age, Boston$crim)
# Older age, more crime
plot(Boston$dis, Boston$crim)
# Closer to work-area, more crime
plot(Boston$rad, Boston$crim)
# Higher index of accessibility to radial highways, more crime
plot(Boston$tax, Boston$crim)
# Higher tax rate, more crime
plot(Boston$ptratio, Boston$crim)
# Higher pupil:teacher ratio, more crime

#d the data is from suburbs
#do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher
#ratios? Comment on them
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=25)
# most cities have low crime rates, but there is a long tail: 18 suburbs appear
# to have a crime rate > 20, reaching to above 80
hist(Boston$tax, breaks=25)
# there is a large divide between suburbs with low tax rates and a peak at 660-680
hist(Boston$ptratio, breaks=25)
# a skew towards high ratios, but no particularly high ratios

#e how may of the subburbs in this data set bound the Charles river?
dim(subset(Boston, chas == 1))

#f what is the median puil-teacher ratio among the towns in this data set?
median(Boston$ptratio)

#g which subburb of Boston has lowest median value of owner-occupied homes?
#t(x) x is a matrix or data frame
t(subset(Boston, medv == min(Boston$medv)))
#what are the values of the other predictors for the suburbs, and how do those values compare to the overall
#ranges for those findings?
summary(Boston)

#h
dim(subset(Boston,Boston$rm>=7))
# 64
dim(subset(Boston, rm > 8))
# 13
summary(subset(Boston, rm > 8))
summary(Boston)
# relatively lower crime (comparing range), lower lstat (comparing range)




































