# Author: Wenhui Zeng
# Title: Mid-Term Exam Project


#########################################
#  Data management part
#############################################


#predictor variables are:

#The SAS and R has different variables
#_STATE (region)
#MSCODE (Urbanity)
#X_RACE(Race group)
#X_AGE_G (age)
#SEX (gender)
#INCOME2 (income)
#X_EDUCAG (education)
#EMPLOY1 (employment status)
#HLTHPLN1 (health insurance)
#AVEDRNK2(about how many drinks did you drink on the average?)

#outcome variables are:

#numeric
# poor mental health days: MENTHLTH (1-30, 88 = None, 77 = don't know, 99 = refused)

#The binary health outcome variable 
#HAVARTH3 (Ever told) you have some form of depression, rheumatoid depression, gout, lupus, 
#or fibromyalgia? (depression diagnoses include: rheumatism, polymyalgia rheumatica; 
#osteodepression (not osteporosis); tendonitis, bursitis, bunion, tennis elbow; 
#carpal tunnel syndrome, tarsal tunnel syndrome; joint infection, etc.)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

#Run all the library
library(leaps)#best subset and backward
library(glmnet)#shrikage method
library(pls)#principle component
library(boot)#cv.glm
library(foreign)#read.xport
library(MASS)#lda
library(class)#knn
#library(survey)#surveylogistic regression
library(tree)
library(e1071)

#Subset and clearn the data
LLCP2014 <- read.xport(file = "D:/courses/BSTcourse/machine learning and predictive modeling/midterm/LLCP2014XPT/LLCP2014.XPT")
#LLCP2014<-llcp2014[ ,c(X_STATE","MSCODE","X_RACE","X_AGE_G","SEX","INCOME2","EMPLOY1","HLTHPLN1","AVEDRNK2","X_EDUCAG","ADDEPEV2","MENTHLTH")]

LLCP2015 <- read.xport(file = "D:/courses/BSTcourse/machine learning and predictive modeling/midterm/LLCP2015XPT/LLCP2015.XPT")
#LLCP2015<-llcp2015[ ,c( "X_STATE","MSCODE",
#         "X_RACE","X_AGE_G","SEX","INCOME2","EMPLOY1","HLTHPLN1","AVEDRNK2","X_EDUCAG","ADDEPEV2","MENTHLTH")]

dim(LLCP2015)
dim(LLCP2014)

names(LLCP2014)

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

LLCP2014$MENTHLTH[LLCP2014$MENTHLTH %in% c(77,99)] <-NA
LLCP2014$MENTHLTH[LLCP2014$MENTHLTH ==88] <-0

#The binary health outcome variable 
#HAVARTH3 (If it have skin cancer)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

LLCP2014$depression[LLCP2014$ADDEPEV2=="1"]<-"Yes"
LLCP2014$depression[LLCP2014$ADDEPEV2==2]<-"No"
LLCP2014$depression[LLCP2014$ADDEPEV2 %in% c(7,9)]<-NA
LLCP2014$depression<- as.factor(LLCP2014$depression)

LLCP2014<-LLCP2014[,c("Hinsurance","employ","gender","income","Age","Race","Urbanity","Region",
                      "AVEDRNK2","Education","MENTHLTH","depression")]

LLCP2014<-na.omit(LLCP2014)
sum(is.na(LLCP2014))

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

LLCP2015$MENTHLTH[LLCP2015$MENTHLTH %in% c(77,99)] <-NA
LLCP2015$MENTHLTH[LLCP2015$MENTHLTH ==88] <-0
LLCP2015$MENTHLTH<-as.numeric(LLCP2015$MENTHLTH)

#The binary health outcome variable 
#HAVARTH3 (Ever told) you have some form of depression, rheumatoid depression, gout,
#lupus, or fibromyalgia? (depression diagnoses include: rheumatism, polymyalgia rheumatica; 
#osteodepression (not osteporosis); tendonitis, bursitis, bunion, tennis elbow; carpal tunnel 
#syndrome, tarsal tunnel syndrome; joint infection, etc.)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

LLCP2015$depression[LLCP2015$ADDEPEV2=="1"]<-"Yes"
LLCP2015$depression[LLCP2015$ADDEPEV2==2]<-"No"
LLCP2015$depression[LLCP2015$ADDEPEV2 %in% c(7,9)]<-NA
LLCP2015$depression<- as.factor(LLCP2015$depression)





LLCP2015<-LLCP2015[,c("Hinsurance","employ","gender","income","Age","Race","Urbanity","Region",
                                      "AVEDRNK2","Education","MENTHLTH","depression")]

#get rid of missing values
LLCP2015<-na.omit(LLCP2015)
names(LLCP2015)
sum(is.na(LLCP2015))#there is no missing value

dim(LLCP2014)
#112879  rows
dim(LLCP2015)
#93753 
# Recode variables as indicated by the codebook


library(ggplot2)
library(dplyr)

names(LLCP2014)

ggplot(LLCP2014,aes(x = gender, y=depression))+ 
  geom_bar(stat = "summary", fun.y = "mean",fill="steelblue")
table(LLCP2014$gender,LLCP2014$depression)

###########
#CREATE TRAIN and TEST SET
##########

#

set.seed(1)
train<-sample(dim(LLCP2014)[1],dim(LLCP2014)[1]/2)
brfss.train<-LLCP2014[train,]
brfss.test<-LLCP2014[-train,]

m.train<-LLCP2014$MENTHLTH[train]
m.test<-LLCP2014$MENTHLTH[-train]
sc.train<-LLCP2014$depression[train]
sc.test<-LLCP2014$depression[-train]


#Analysis
################################

# Classification Model Fitting  and Prediction using 2015

################################

# Using any/all methods from the first half of the semester, create a model that
# predicts your binary outcome variable as accurately as possible from your
# explanitory variables. Describe the methods you used, and why you chose those
# methods. Report any relevant training and testing errors, along with any final
# model coefficients and how those coefficients should be interpreted.


#in this Section, we use the k-fold cross-validation method to estimate the test error!, and 
#Use the data 2015 to calculate the real test error!!!

library(survey)
library(srvyr)
##Surveylogistic regression
names(LLCP2014)

#not sophisitcated. 

#des <- LLCP2014_1 %>% as_survey_design(ids=X_PSU, strata=X_STSTR, weights=X_LLCPWT,nest=TRUE,
#                             variables=c(Hinsurance,employ,gender,income,Age,Race,Urbanity,
 #                                       Region,AVEDRNK2,Education,MENTHLTH))

#design<- svydesign(id=~1, strata=~X_STSTR, weights=~X_LLCPWT,nest=TRUE,data=LLCP2014)

#logisticm <- svyglm(MENTHLTH~Hinsurance+Region+AVEDRNK2,family=gaussian,design=design)


#logistic model
lg.fit.0<-glm(depression~.-MENTHLTH,family = binomial,data=brfss.train)
summary(lg.fit.0)
lg.pred <- predict(lg.fit.0,newdata=brfss.test, type = "response")
glm.pred <- rep("No", length(lg.pred))
glm.pred[lg.pred>0.5] <- "Yes"

# get vector of predicted classifications:
(class.table<-table(glm.pred,sc.test))
# and our crosstabulation of the predicted vs the actual classification
# Finally, our TEST ERROR RATE
a<-1-sum(diag(class.table))/sum(class.table)

lg.fit<-glm(depression~.-MENTHLTH-AVEDRNK2-Education,family = binomial,data=brfss.train)
summary(lg.fit)
#training error
#lg.pred <- predict(lg.fit,newdata=brfss.train, type = "response")
#glm.pred <- rep("No", length(lg.pred))
#glm.pred[lg.pred>0.5] <- "Yes"
# get vector of predicted classifications:
#(class.table<-table(glm.pred,sc.train))
# and our crosstabulation of the predicted vs the actual classification
# Finally, our TEST ERROR RATE
#b1<-1-sum(diag(class.table))/sum(class.table)


# Get fitted probabilities from test set error:
lg.pred <- predict(lg.fit,newdata=brfss.test, type = "response")
glm.pred <- rep("No", length(lg.pred))
glm.pred[lg.pred>0.5] <- "Yes"
# get vector of predicted classifications:
(a<-table(glm.pred,sc.test))
# and our crosstabulation of the predicted vs the actual classification
# Finally, our TEST ERROR RATE

b<-1-sum(diag(a))/sum(a)



boot.fn <- function(data, index){
  return(coef(glm(depression~.-MENTHLTH,family = binomial,data=brfss.train, subset = index)))
}

boot.fn(brfss.train, 1:dim(brfss.train)[1])
set.seed(1)
boot.fn(brfss.train, sample(dim(brfss.train)[1], dim(brfss.train)[1], replace = T))
#coef(lg.fit)
# now let's see two steps of a bootstrap using sample()
# ... x1000 times... or use boot() to estimate the standard error

class.table<-boot(brfss.train, boot.fn, R=100)

#The model with all the ten variables was signficant. but the variable AVEDRNK2 was not significant
#different, so after we remove it from the model the test error is about 31%, is decrease from
#the model with all variables, so for logistic regression, the model with the other 9 variables
#gives good prediction.Since ohter categorical variable has different subset groups, some sub groups
#has significant effect, so we keep the main groups.


# get vector of predicted classifications:
(class.table<-table(glm.pred,sc.test))
# and our crosstabulation of the predicted vs the actual classification

# Finally, our TEST ERROR RATE
b<-1-sum(diag(class.table))/sum(class.table)
#The final test errror is 0.3367116

boot.fn <- function(data, index){
  return(coef(glm(depression~.-MENTHLTH,family = binomial,data=brfss.train, subset = index)))
}

boot.fn(brfss.train, 1:dim(brfss.train)[1])
set.seed(1)
boot.f1<-boot.fn(brfss.train, sample(dim(brfss.train)[1], dim(brfss.train)[1], replace = T))
names<-names(boot.f1)
a<-data.frame(boot.f1,names)
#Try to make it looks like a table, so it can be printed in the R markdown. 
print(a)

coef(lg.fit)
print(boot.f1,caption = "boostrap estimate the coefficients",comment=F,caption.placement="top")
# now let's see two steps of a bootstrap using sample()
# ... x1000 times... or use boot() to estimate the standard error
boot(brfss.train, boot.fn, R=100)


#Lindear Discriminant Analysis
lda.fit<-lda(depression~.-MENTHLTH,data=brfss.train)
summary(lda.fit)
lda.pred <- predict(lda.fit, brfss.test)
b<-table(lda.pred$class, sc.test)
1-sum(diag(b))/sum(b)

plot(lda.fit)


#Test errror is 0.3367727

#QDA
qda.fit <- qda(depression~.-MENTHLTH,data=brfss.train)

qda.pred <- predict(qda.fit, brfss.test)
c<-table(qda.pred$class, sc.test)
c<-1-sum(diag(c))/sum(c)
mean(qda.pred$class != sc.test)
t<-summary(qda.fit)
t<-data.frame(t)

#Test error is  0.39514

### K-Nearest Neighbors
names(brfss.train)
# Create training matrix
####Is there any easy way to create a matrix for KNN?

x.train<-cbind(brfss.train$Hinsurance,brfss.train$employ,brfss.train$gender,brfss.train$income,brfss.train$Age,
              brfss.train$Race,brfss.train$Urbanity,brfss.train$Region,brfss.train$AVEDRNK2,
              brfss.train$Education)

x.test<-cbind(brfss.test$Hinsurance,brfss.test$employ,brfss.test$gender,brfss.test$income,brfss.test$Age,
              brfss.test$Race,brfss.test$Urbanity,brfss.test$Region,brfss.test$AVEDRNK2,
              brfss.test$Education)

# Create testing matrix

# Get t

# Now run knn()

set.seed(1) # set's the random seed number so that results can be reproduced, tell the computer 
#where should we start pulling the random number 
# run knn with k = 1

#k=3
knn.pred.3 <- knn(x.train,x.test,sc.train, k = 3)

table(knn.pred.3, sc.test)
mean(knn.pred.3 != sc.test)
#Test error is 0.3412098

#k=5
knn.pred.5 <- knn(x.train,x.test,sc.train, k = 5)
table(knn.pred.5, sc.test)
mean(knn.pred.5 != sc.test)
#Test error is 0.3313704

#k=10
knn.pred.10 <- knn(x.train,x.test,sc.train, k = 10)
table(knn.pred.10, sc.test)
mean(knn.pred.10 != sc.test)
#Test error is  0.3216801

#K=100

knn.pred.100 <- knn(x.train,x.test,sc.train, k = 100)
table(knn.pred.100, sc.test)
mean(knn.pred.100 != sc.test)
#The test error is 0.3157



### Model selection by Validation Set and Cross-Validation approaches




############################
# Regression Model Fitting #
############################

#####linear regression

lm.fit<-glm(MENTHLTH~.-depression,data=brfss.train)
summary(lm.fit)

lm.pred<-predict(lm.fit,brfss.test)
#the mean square error of the test data was
lm.MSE<-mean((m.test-lm.pred)^2)

lm.fit<-glm(MENTHLTH~.-depression-gender-Urbanity,data=brfss.train)
summary(lm.fit)

lm.pred<-predict(lm.fit,brfss.test)
#the mean square error of the test data was not change, but we tend to select the simple model without the gender and Urbanity
#we can see gender, urbanity are both not significant different!
lm.MSE<-mean((m.test-lm.pred)^2)

lm.pred<-predict(lm.fit,brfss.train)
mean((m.train-lm.pred)^2)

#### 6.5.1 Best Subset selection

#Create a plot function

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


############Best Subset selection

regfit.best <- regsubsets(MENTHLTH~.-depression, data = brfss.train, nvmax = 26)
reg.summary <- summary(regfit.best)

plot.regsummary(reg.summary)
#According to the plot, it seems like the RSS, CP and BIC was decreased as the number 
#of variables increase. We can't make a good decision. We try the validation approach to estimate the 
#test error to determine the best model


test.mat <- model.matrix(MENTHLTH~.-depression, data = brfss.test)
val.errors <- rep(NA, 26)
for (i in 1:26){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((m.test-pred)^2)
}
which.min(val.errors)
#In this setting, we can select a model using the
#one-standard-error rule. We first calculate the standard
#error of the estimated test MSE for each model size, and
#then select the smallest model for which the estimated test
#error is within one standard error of the lowest point on
#the curve

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:26,val.errors,xlab = "Number of Variables",ylab = "Estimate Test Error",ylim = c(80,90))
abline(h=a,lty=2)
abline(h=b,lty=2)

#According to the rule, 10 variables gives the simplest model. It was within the one standard from the minumum value

n<-names(coef(regfit.best,6))
coef<-data.frame(coef(regfit.best, 10))
print(coef,caption="coefficients using best subset method")



### 6.5.2 Forward and Backward Stepwise Selection
# regsubsets() works here too; just specify method = "forward" or "backward"
# method = "forward":
regfit.fwd <- regsubsets(MENTHLTH~.-depression, data = brfss.train, nvmax = 26, 
                         method ="forward")
(fwd.summary <- summary(regfit.fwd))

plot.regsummary(fwd.summary)

test.mat <- model.matrix(MENTHLTH~.-depression, data = brfss.test,method ="forward")
val.errors <- rep(NA, 26)
for (i in 1:26){
  coefi <- coef(regfit.fwd, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((m.test-pred)^2)
}
which.min(val.errors)
#In this setting, we can select a model using the
#one-standard-error rule. We first calculate the standard
#error of the estimated test MSE for each model size, and
#then select the smallest model for which the estimated test
#error is within one standard error of the lowest point on
#the curve

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:26,val.errors,xlab = "Number of Variables",ylab = "Estimate Test Error",ylim = c(80,90))
abline(h=a,lty=2)
abline(h=b,lty=2)

#we can see the 5 variables gives a estimate test error that falls with one standard of the minimum
coefi <- coef(regfit.fwd, id = 6)

# method = "backward":
regfit.bwd = regsubsets(MENTHLTH~.-depression, data = brfss.train, nvmax = 26, 
                        method = "backward")
(bwd.summary <- summary(regfit.bwd))
# Now use our new plot function:
plot.regsummary(bwd.summary)

test.mat <- model.matrix(MENTHLTH~.-depression, data = brfss.test, nvmax = 26, 
                         method = "backward")
val.errors <- rep(NA, 26)
for (i in 1:26){
  coefi <- coef(regfit.bwd, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((m.test-pred)^2)
}

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:26,val.errors,xlab = "Number of Variables",ylab = "Estimate Test Error",ylim = c(80,90))
abline(h=a,lty=2)
abline(h=b,lty=2)
coefi <- coef(regfit.bwd, id = 10)


#Ridge Regression
# Create model matrix and outcome vector. predict matrix
names(brfss.train)
names(BRFSS)

x.train <- model.matrix(MENTHLTH~.-depression, brfss.train)[ ,-c(11,12)]
x.test <- model.matrix(MENTHLTH~.-depression, brfss.test)[ ,-c(11,12)]
x<-model.matrix(MENTHLTH~.-depression,LLCP2014)[ ,-c(11,12)]
y<-LLCP2014$MENTHLTH

grid <- 10^seq(10, -2, length = 100)
# fot ridge regression (alpha = 0)
#find the lambda
ridge.mod <- glmnet(x.train, m.train, alpha = 0, lambda = grid)
## Training and Testing fits:

# Create vector for subsetting data into training and testing sets
set.seed(1)

# cv.glmnet will do a cross validation of lambda, with 10-fold CV
cv.out <- cv.glmnet(x.train, m.train, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam 

ridge.pred <- predict(ridge.mod, s = bestlam ,newx = x.test)
mean((ridge.pred - m.test)^2)

out <- glmnet(x, y, alpha = 0)

a<-predict(out, type="coefficients", s = bestlam)
names<-rbind("Intercept","Intercep","HinsuranceYes","employunemployed","genderMale","income$25,000-$35,000","income$35,000-$50,000","income$50,000-$75,000",
             "income$75,000+","income<=$15,000", "Age25-34","Age55-64","Age65+","RaceNon-Hispanic Black","RaceNon-Hispanic Others","RaceNon-Hispanic White","UrbanityNon-metropolitan",
             "UrbanityOutside MSA", "UrbanitySuburb MSA", "RegionNortheast", "RegionSouth","RegionWest","AVEDRNK2", "EducationDid not graduate High School",
             "EducationGraduated from College/Technical School", "EducationGraduated from High
             School")

r.table<-cbind(name=names,coef=a[1:26,1])




### Lab 6.6.2 The Lasso

# To do a Lasso, simply set alpha = 1 instead of 0
lasso.mod <- glmnet(x.train, m.train, alpha = 1, lambda = grid)
# see that some estimates will be zero depending on lambda
plot(lasso.mod)

# Now let's try performing Cross Validation, we cross validation on a range of lambda
set.seed(1)
cv.out <- cv.glmnet(x.train, m.train, alpha = 1)
plot(cv.out) # compare MSE for lambda = 0 (least squares) out to lambda = BIG 
# (a null model)
# our best lambda is:
#put the whole thing in (), you not only sign it, but also print out
(bestlam <- cv.out$lambda.min)
# Now get Testing MSE for best lambda
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x.test)
mean((lasso.pred - m.test)^2)
# This Lasso Testing MSE is better than than the null model (lambda = big), 
# better than the least squqres model (lambda = 0), and comperable to the 
# Testing MSE calculated using Ridge Regression.
#the mean square error is not as good as ridge, but it has simpler model

# Lasso's advantabge over Ridge is that it estimates "sparse" coefficients,
# i.e. many estimates will = 0, indicating that the model only needs a subset
# of available features. 

# Refit Lasso on full data:
out <- glmnet(x, y, alpha = 1, lambda = grid)
# Extract coefficients  associated with best lambda
(lasso.coef <- predict(out, type = "coefficients", s = bestlam))
# Which aren't zero?



### 6.7.1 Principal Components Regression
#fit on the full data set:
pcr.fit <- pcr(MENTHLTH~.-depression, data=brfss.train, scale=TRUE, 
               validation ="CV")

val.errors <- rep(NA, 26)
for (i in 1:26){
  pcr.pred <- predict(pcr.fit, brfss.test, ncomp = i)
  val.errors[i] <-  mean((pcr.pred - m.test)^2)
}

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:26,val.errors,xlab = "number of component",ylab = "Estimate Test Error",ylim = c(82,88))
abline(h=a,lty=2)
abline(h=b,lty=2)

#20 components gives good predictions
pcr.fit <- pcr(MENTHLTH~.-depression, data=LLCP2015, scale=TRUE, ncomp = 20)
summary(pcr.fit)


### 6.7.2 Partial Least Squares

# the implemtation of PLS is similar to PCR
#partial least sqare looks the rotation the cloud, finds the rotation that accounts most related 
#to y. Different from principle, rotate cloud independent from y, and find the one that 
#best explained the x

set.seed(1)
pls.fit <- plsr(MENTHLTH~.-depression, data = brfss.train, scale = T, 
                validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")


summary(pls.fit)
val.errors <- rep(NA, 26)

for (i in 1:26){
  pls.pred <- predict(pls.fit, brfss.test, ncomp = i)
  val.errors[i] <-  mean((pls.pred - m.test)^2)
}

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:26,val.errors,xlab = "number of component",ylab = "Estimate Test Error",ylim = c(82,88))
abline(h=a,lty=2)
abline(h=b,lty=2)

# So finally fit M=2 to the full model
pls.fit <- plsr(MENTHLTH~.-depression,data=LLCP2014, scale = TRUE, ncomp = 4)
print(names(summary(pls.fit)))

# Notice that the M = 2 PLS model explains 46% of the variability, where it took
# PCR M=7 to get 46%. PLS "rotates" to find the best explanation of the outcome,
# where PCA "rotates" only the predictor variables.





#the mean square error of the test data was not change, but we tend to select the simple model without the gender and Urbanity
#we can see gender, urbanity are both not significant different!


#SVM

library(e1071)
set.seed(1)
#cost=0.01

train.tree<-sample(nrow(brfss.train),0.7*nrow(brfss.train))
train.tree.x<-brfss.train[train.tree,]
tree.test.x<-brfss.train[-train.tree,]
tree.train.y<-brfss.train$depression[train.tree]
tree.test.y<-brfss.train$depression[-train.tree]

brfss2014.svm <- svm(depression ~ .-MENTHLTH, data = train.tree.x, kernel="linear",
                     cost=0.01)
summary(brfss2014.svm)

ypred <- predict(brfss2014.svm, tree.test.x)
svm.table<-table(predict = ypred, truth = tree.test.y)
1-sum(diag(svm.table))/sum(svm.table)#0.1683203

#Kernels

brfss2014.svm <- svm(depression ~ .-MENTHLTH, data = train.tree.x, kernel="radial",
                     cost=0.1,gamma=1,scale = FALSE)
summary(brfss2014.svm)

ypred <- predict(brfss2014.svm, tree.test.x)
svm.table<-table(predict = ypred, truth = tree.test.y)
1-sum(diag(svm.table))/sum(svm.table)#0.8067863


#Tree

set.seed(1)
library(tree)
library(e1071)
tree.brfss2014 <- tree(depression~.-MENTHLTH,data= train.tree.x)
summary(tree.brfss2014)

# Show a plot of the tree with text() node labels
plot(tree.brfss2014)
text(tree.brfss2014, pretty = 0)

tree.pred <- predict(tree.brfss2014,tree.test.x, type = "class")
(brfss.table <- table(tree.pred, tree.test.y))
1-sum(diag(brfss.table ))/sum(brfss.table)

cv.brfss2014<- cv.tree(tree.brfss2014, FUN = prune.misclass )
cv.brfss2014

par(mfrow = c(1, 2))
plot(cv.brfss2014$size, cv.brfss2014$dev, type = "b")
plot(cv.brfss2014$k, cv.brfss2014$dev, type = "b")

prune.brfss <- prune.misclass(tree.brfss2014, best = 2)
plot(prune.brfss)
text(prune.brfss,pretty = 0)

tree.pred <- predict(prune.brfss, tree.test.x, type = "class")
prune.carseats.tab <- table(tree.pred, tree.test.y)
1-sum(diag(prune.carseats.tab))/sum(prune.carseats.tab)


#continuous variable 

train.tree<-sample(nrow(brfss.train),0.7*nrow(brfss.train))
train.tree.x<-brfss.train[train.tree,]
tree.test.x<-brfss.train[-train.tree,]
tree.train.y<-brfss.train$MENTHLTH[train.tree]
tree.test.y<-brfss.train$MENTHLTH[-train.tree]


set.seed(1)
library(tree)
library(e1071)
tree.brfss2014 <- tree(MENTHLTH~.-depression,data= train.tree.x)
summary(tree.brfss2014)

# Show a plot of the tree with text() node labels
plot(tree.brfss2014)
text(tree.brfss2014, pretty = 0)

tree.pred <- predict(tree.brfss2014,tree.test.x)


cv.brfss2014<- cv.tree(tree.brfss2014 )
cv.brfss2014

par(mfrow = c(1, 2))
plot(cv.brfss2014$size, cv.brfss2014$dev, type = "b")
plot(cv.brfss2014$k, cv.brfss2014$dev, type = "b")

prune.brfss <- prune.tree(tree.brfss2014, best = 3)
plot(prune.brfss)
text(prune.brfss,pretty = 0)

tree.pred <- predict(prune.brfss, tree.test.x)
mean((tree.pred-tree.test.y)^2)


#Random Forest
set.seed(1)
library(randomForest)
rf.brfss2014 <- randomForest(MENTHLTH ~ .-depression, data = train.tree.x, mtry = 3, importance = TRUE)
rf.brfss2014 
predict.rf <- predict(rf.brfss2014, newdata = LLCP2015)
mean((predict.rf-LLCP2014$MENTHLTH)^2)
importance(rf.brfss2014)
varImpPlot(rf.brfss2014)






#Prediction

#Using LDA and the best subset to predict the 2015 data set

#Lindear Discriminant Analysis
lda.fit.2015<-lda(depression~.-MENTHLTH,data=LLCP2015)
summary(lda.fit.2015)
lda.pred <- predict(lda.fit.2015, LLCP2015)
table(lda.pred$class, LLCP2015$depression)
1-mean(lda.pred$class != LLCP2015$depression)
plot(lda.fit.2015)

#logistic regression
glm.fit<-glm(depression~.-MENTHLTH,family = binomial,data=LLCP2015)
glm.prob<-predict(glm.fit,LLCP2015)
glm.pred <- rep("No", length(glm.prob))
glm.pred[glm.prob>0.5] <- "Yes"
a<-table(glm.pred,LLCP2015$depression)
sum(diag(a))/sum(a)


#QDA
qda.fit <- qda(depression~.-MENTHLTH,data=LLCP2015)
qda.pred <- predict(qda.fit, LLCP2015)
c<-table(qda.pred$class, LLCP2015$depression)
c<-1-sum(diag(c))/sum(c)
mean(qda.pred$class != sc.test)



#KNN

train.2015<-cbind(LLCP2015$Hinsurance,LLCP2015$employ,LLCP2015$gender,LLCP2015$income,LLCP2015$Age,
               LLCP2015$Race,LLCP2015$Urbanity,LLCP2015$Region,LLCP2015$AVEDRNK2,
               LLCP2015$Education)

test.2015<-cbind(LLCP2015$Hinsurance,LLCP2015$employ,LLCP2015$gender,LLCP2015$income,LLCP2015$Age,
              LLCP2015$Race,LLCP2015$Urbanity,LLCP2015$Region,LLCP2015$AVEDRNK2,
              LLCP2015$Education)


knn.pred.10 <- knn(train.2015,test.2015,LLCP2015$depression, k = 10)
table(knn.pred.10, LLCP2015$depression)
1-mean(knn.pred.10 != LLCP2015$depression)


#SVM

brfss2015.svm <- svm(depression ~ .-MENTHLTH, data = LLCP2015, kernel="linear",
                     cost=0.01)
summary(brfss2015.svm)

ypred <- predict(brfss2015.svm, LLCP2015)
svm.table<-table(predict = ypred, truth = LLCP2015$depression)
1-sum(diag(svm.table))/sum(svm.table)#0.1683203



#Tree

set.seed(1)
library(tree)
tree.brfss2015 <- tree(depression~.-MENTHLTH,data=LLCP2015)
summary(tree.brfss2015)

# Show a plot of the tree with text() node labels
plot(tree.brfss2015)
text(tree.brfss2015, pretty = 0)

tree.pred <- predict(tree.brfss2015,LLCP2015, type = "class")
(brfss.table <- table(tree.pred, LLCP2015$depression))
1-sum(diag(brfss.table ))/sum(brfss.table)

cv.brfss2015<- cv.tree(tree.brfss2015, FUN = prune.misclass )
cv.brfss2015

par(mfrow = c(1, 2))
plot(cv.brfss2015$size, cv.brfss2015$dev, type = "b")
plot(cv.brfss2015$k, cv.brfss2015$dev, type = "b")

prune.brfss <- prune.misclass(tree.brfss2015, best = 2)
plot(prune.brfss)
text(prune.brfss,pretty = 0)

tree.pred <- predict(prune.brfss, LLCP2015, type = "class")
prune.carseats.tab <- table(tree.pred, LLCP2015$depression)
1-sum(diag(prune.carseats.tab))/sum(prune.carseats.tab)



#Random Forest
set.seed(1)
library(randomForest)
rf.brfss2015 <- randomForest(MENTHLTH ~ .-depression, data = LLCP2015, mtry = 3, importance = TRUE)
rf.brfss2015 
predict.bag <- predict(rf.brfss2015, newdata = LLCP2015,type = "response")
(brfss.table <- table(predict.bag, LLCP2015$depression))
(1-sum(diag(brfss.table))/sum(brfss.table))
importance(rf.brfss2015)
varImpPlot(rf.brfss2015)





###Linear regression prediction 

####Best Subset selection

regfit.best <- regsubsets(MENTHLTH~.-depression, data = LLCP2015, nvmax = 26)
reg.summary <- summary(regfit.best)

plot.regsummary(reg.summary)
#According to the plot, it seems like the RSS, CP and BIC was decreased as the number 
#of variables increase. We can't make a good decision. We try the validation approach to estimate the 
#test error to determine the best model

n<-names(coef(regfit.best,10))
coef<-data.frame(coef(regfit.best, 10))
print(coef,caption="coefficients using best subset method")



test.mat <- model.matrix(MENTHLTH~.-depression, data = LLCP2015)
val.errors <- rep(NA, 26)
for (i in 1:26){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((LLCP2015$MENTHLTH-pred)^2)
}
which.min(val.errors)
#In this setting, we can select a model using the
#one-standard-error rule. We first calculate the standard
#error of the estimated test MSE for each model size, and
#then select the smallest model for which the estimated test
#error is within one standard error of the lowest point on
#the curve

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:26,val.errors,xlab = "Number of Variables",ylab = "Estimate Test Error",ylim = c(80,90))
abline(h=a,lty=2)
abline(h=b,lty=2)

#According to the rule, 10 variables gives the simplest model. It was within the one standard from the minumum value

n<-names(coef(regfit.best,6))
coef<-data.frame(coef(regfit.best, 10))
print(coef,caption="coefficients using best subset method")

names(LLCP2014)




lm.fit<-glm(MENTHLTH~.-depression-gender-Urbanity,data=LLCP2015)
summary(lm.fit)

lm.pred<-predict(lm.fit,LLCP2015)
#the mean square error of the test data was not change, but we tend to select the simple model without the gender and Urbanity
#we can see gender, urbanity are both not significant different!
lm.MSE<-mean((LLCP2015$MENTHLTH-lm.pred)^2)



#Ridge Regression
# Create model matrix and outcome vector. predict matrix
names(brfss.train)
names(BRFSS)

x.train <- model.matrix(MENTHLTH~.-depression, LLCP2015)[ ,-c(11,12)]
x.test <- model.matrix(MENTHLTH~.-depression, LLCP2015)[ ,-c(11,12)]
x<-model.matrix(MENTHLTH~.-depression,LLCP2015)[ ,-c(11,12)]
y<-LLCP2015$MENTHLTH

grid <- 10^seq(10, -2, length = 100)
# fot ridge regression (alpha = 0)
#find the lambda
ridge.mod <- glmnet(x.train, LLCP2015$MENTHLTH, alpha = 0, lambda = grid)
## Training and Testing fits:

# Create vector for subsetting data into training and testing sets
set.seed(1)

# cv.glmnet will do a cross validation of lambda, with 10-fold CV
cv.out <- cv.glmnet(x.train, LLCP2015$MENTHLTH, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam 

ridge.pred <- predict(ridge.mod, s = bestlam ,newx = x.test)
mean((ridge.pred -LLCP2015$MENTHLTH )^2)
mean(ridge.pred)
mean(LLCP2015$MENTHLTH)
out <- glmnet(x, y, alpha = 0)

a<-predict(out, type="coefficients", s = bestlam)
names<-rbind("Intercept","Intercep","HinsuranceYes","employunemployed","genderMale","income$25,000-$35,000","income$35,000-$50,000","income$50,000-$75,000",
             "income$75,000+","income<=$15,000", "Age25-34","Age55-64","Age65+","RaceNon-Hispanic Black","RaceNon-Hispanic Others","RaceNon-Hispanic White","UrbanityNon-metropolitan",
             "UrbanityOutside MSA", "UrbanitySuburb MSA", "RegionNortheast", "RegionSouth","RegionWest","AVEDRNK2", "EducationDid not graduate High School",
             "EducationGraduated from College/Technical School", "EducationGraduated from High
             School")

r.table<-cbind(name=names,coef=a[1:26,1])




### Lab 6.6.2 The Lasso

# To do a Lasso, simply set alpha = 1 instead of 0
lasso.mod <- glmnet(x.train, LLCP2015$MENTHLTH, alpha = 1, lambda = grid)
# see that some estimates will be zero depending on lambda
plot(lasso.mod)

# Now let's try performing Cross Validation, we cross validation on a range of lambda
set.seed(1)
cv.out <- cv.glmnet(x.train, LLCP2015$MENTHLTH, alpha = 1)
plot(cv.out) # compare MSE for lambda = 0 (least squares) out to lambda = BIG 
# (a null model)
# our best lambda is:
#put the whole thing in (), you not only sign it, but also print out
(bestlam <- cv.out$lambda.min)
# Now get Testing MSE for best lambda
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x.test)
mean((lasso.pred - LLCP2015$MENTHLTH)^2)
mean(lasso.pred)
# This Lasso Testing MSE is better than than the null model (lambda = big), 
# better than the least squqres model (lambda = 0), and comperable to the 
# Testing MSE calculated using Ridge Regression.
#the mean square error is not as good as ridge, but it has simpler model

# Lasso's advantabge over Ridge is that it estimates "sparse" coefficients,
# i.e. many estimates will = 0, indicating that the model only needs a subset
# of available features. 

# Refit Lasso on full data:
out <- glmnet(x, y, alpha = 1, lambda = grid)
# Extract coefficients  associated with best lambda
(lasso.coef <- predict(out, type = "coefficients", s = bestlam))
# Which aren't zero?


#tree
tree.brfss2015 <- tree(MENTHLTH~.-depression,data= LLCP2015)
summary(tree.brfss2015)

# Show a plot of the tree with text() node labels
plot(tree.brfss2015)
text(tree.brfss2015, pretty = 0)

tree.pred <- predict(tree.brfss2015,LLCP2015)
(brfss.table <- table(tree.pred, LLCP2015$MENTHLTH))
1-sum(diag(brfss.table ))/sum(brfss.table)

cv.brfss2015<- cv.tree(tree.brfss2015 )
cv.brfss2015

par(mfrow = c(1, 2))
plot(cv.brfss2015$size, cv.brfss2015$dev, type = "b")
plot(cv.brfss2015$k, cv.brfss2015$dev, type = "b")

prune.brfss <- prune.tree(tree.brfss2015, best = 3)
plot(prune.brfss)
text(prune.brfss,pretty = 0)

tree.pred <- predict(prune.brfss, LLCP2015)
mean((tree.pred-LLCP2015$MENTHLTH)^2)





#Random Forest
set.seed(1)
library(randomForest)
rf.brfss2014 <- randomForest(depression ~ .-MENTHLTH, data = train.tree.x, mtry = 3, importance = TRUE)
rf.brfss2014 
predict.bag <- predict(rf.brfss2014 , newdata = tree.test.x,type = "response")
(brfss.table <- table(predict.bag, tree.test.y))
1-sum(diag(brfss.table ))/sum(brfss.table)
importance(rf.brfss2014)
varImpPlot(rf.brfss2014)




