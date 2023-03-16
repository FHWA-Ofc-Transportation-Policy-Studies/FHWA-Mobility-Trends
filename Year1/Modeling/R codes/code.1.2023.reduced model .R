setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)


#VMT######################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)

#examine the scatterplots/correlations
cpairs(VMT.dat.s)
cor(VMT.dat.s)

#lasso regression
#library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(VMT.dat.s[,-c(1,3,10)], VMT.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.s[,-c(1,3,10)]), VMT.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min
optimal.lambda
plot(cv.lasso)
coef(lasso.mod, s= optimal.lambda)

plot(lasso.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.lasso$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(lasso.mod)
coefplot(lasso.mod, lambda=cv.lasso$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]



#ridge regression
lambdas=10^seq(5,-5,by=-.1)
ridge.mod=glmnet(VMT.dat.s[,-c(1,3,10)], VMT.dat.s[,1], alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.s[,-c(1,3,10)]), VMT.dat.s[,1], alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=20)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
plot(cv.ridge)
coef(ridge.mod, s= optimal.lambda)

plot(ridge.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.ridge$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(ridge.mod)
coefplot(ridge.mod, lambda=cv.ridge$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.ridge$lambda, ridge.mod$dev.ratio)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]



library(gam)
summary(gam(VMT~s(Unemployment.Rate.2)+s(TotUrbanMiles)+TransitActiveFleet+EVPrivateandPublicChargingStationLocations+TeleworkCollegeGraduates, data=VMT.dat.s))

summary(lm(VMT~Unemployment.Rate.2+TotUrbanMiles+TransitActiveFleet+EVPrivateandPublicChargingStationLocations+TeleworkCollegeGraduates+EVPrivateandPublicChargingStationLocations*TeleworkCollegeGraduates+TotUrbanMiles*TransitActiveFleet
           +TransitActiveFleet*EVPrivateandPublicChargingStationLocations+TotUrbanMiles*TeleworkCollegeGraduates+TeleworkCollegeGraduates*TransitActiveFleet, data=VMT.dat.s))
ridge.mod=glmnet(VMT.dat.s[,c(6,16,23,35,40)], VMT.dat.s[,2], alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.s[,c(6,16,23,35,40)]), VMT.dat.s[,2], alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=20)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
plot(cv.ridge)
coef(ridge.mod, s= optimal.lambda)


y<-c(1,2,3)
x1<-c(1,1.9,3)
x2<-c(2.7,2,0.9)
summary(lm(y~x1+x2))



#GHG######################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5:6,15,17,26,28,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)

#examine the scatterplots/correlations
cpairs(GHG.dat.s)
cor(GHG.dat.s)

#lasso regression
#library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(GHG.dat.s[,-c(1,3,9)], GHG.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.s[,-c(1,3,9)]), GHG.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min
optimal.lambda
plot(cv.lasso)
coef(lasso.mod, s= optimal.lambda)

plot(lasso.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.lasso$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(lasso.mod)
coefplot(lasso.mod, lambda=cv.lasso$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

#ridge regression
lambdas=10^seq(5,-5,by=-.1)
ridge.mod=glmnet(GHG.dat.s[,-c(1,3,9)], GHG.dat.s[,1], alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.s[,-c(1,3,9)]), GHG.dat.s[,1], alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=20)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
plot(cv.ridge)
coef(ridge.mod, s= optimal.lambda)

plot(ridge.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.ridge$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(ridge.mod)
coefplot(ridge.mod, lambda=cv.ridge$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.ridge$lambda, ridge.mod$dev.ratio, cv.ridge$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]



#TMS########################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:3,14:15,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)

#examine the scatterplots/correlations
cpairs(TMS.dat.s)
cor(TMS.dat.s)

#lasso regression
#library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(TMS.dat.s[,-c(1,3,9)], TMS.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.s[,-c(1,3,9)]), TMS.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=16, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min
optimal.lambda
plot(cv.lasso)
coef(lasso.mod, s= optimal.lambda)

plot(lasso.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.lasso$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(lasso.mod)
coefplot(lasso.mod, lambda=cv.lasso$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

#ridge regression
lambdas=10^seq(5,-5,by=-.1)
ridge.mod=glmnet(TMS.dat.s[,-c(1,3,9)], TMS.dat.s[,1], alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.s[,-c(1,3,9)]), GHG.dat.s[,1], alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=16)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
plot(cv.ridge)
coef(ridge.mod, s= optimal.lambda)

plot(ridge.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.ridge$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(ridge.mod)
coefplot(ridge.mod, lambda=cv.ridge$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.ridge$lambda, ridge.mod$dev.ratio, cv.ridge$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

