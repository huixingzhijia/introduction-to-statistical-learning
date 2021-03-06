#12


library(tree)
library(ISLR)
library(randomForest)
library(MASS)
library(gbm)
library(glmnet)
library(foreign)#read.xport


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
train<-1:dim(LLCP2014)[1]
brfss.train<-BRFSS[1:dim(LLCP2014)[1],]
brfss.test<-BRFSS[(dim(LLCP2014)[1]+1):dim(BRFSS)[1],]
m.train<-BRFSS$MENTHLTH[1:dim(LLCP2014)[1]]
m.test<-BRFSS$MENTHLTH[(dim(LLCP2014)[1]+1):dim(BRFSS)[1]]
sc.train<-BRFSS$arthritis[1:dim(LLCP2014)[1]]
sc.test<-BRFSS$arthritis[(dim(LLCP2014)[1]+1):dim(BRFSS)[1]]


#logistic regression

lg.fit<-glm(arthritis~.-MENTHLTH-AVEDRNK2,family = binomial,data=brfss.train)
# Get fitted probabilities from test set error:
lg.pred <- predict(lg.fit,newdata=brfss.test, type = "response")
glm.pred <- rep("No", length(lg.pred))
glm.pred[lg.pred>0.5] <- "Yes"
# get vector of predicted classifications:
(class.table<-table(glm.pred,sc.test))
# and our crosstabulation of the predicted vHinsurances the actual classification
# Finally, our TEST ERROR RATE
b<-1-sum(diag(class.table))/sum(class.table)

##boosting
BRFSS$arthritis = ifelse(BRFSS$arthritis == "Yes", 1, 0)
boost.fit<-gbm(arthritis~.-MENTHLTH-AVEDRNK2,data=BRFSS[train,],distribution="gaussian",n.trees=1000, verbose=F)
summary(boost.fit)

#Age and employ are the two most important variables.

boost.pred<-predict(boost.fit,brfss.test,n.trees=1000)
boost.prob<-rep(NA,length(boost.pred))
boost.prob = ifelse(boost.pred >0.5, 1, 0)
table(boost.prob,sc.test)
#Test error is 0.3901457
#10468/(10468+16363)


#Bagging

brfss.bag<-randomForest(arthritis~.-MENTHLTH-AVEDRNK2,data=brfss.train,mtry=8,importance=T)
importance(brfss.bag)

yhat.bag<-predict(brfss.bag,newdata=brfss.test)
table(yhat.bag,sc.test)
test.error<-mean(yhat.bag!=sc.test)
#The age, income and employ are important variables.The test error is 0.3375573.

#Random forest
brfss.rf<-randomForest(arthritis~.-MENTHLTH-AVEDRNK2,data=brfss.train,mtry=sqrt(8),importance=T)
importance(brfss.rf)

yhat.bag<-predict(brfss.rf,newdata=brfss.test)
table(yhat.bag,sc.test)
test.error<-mean(yhat.bag!=sc.test)

#The test error is  0.314077.

#The random forest and logistic regression has the lowest test error. 









