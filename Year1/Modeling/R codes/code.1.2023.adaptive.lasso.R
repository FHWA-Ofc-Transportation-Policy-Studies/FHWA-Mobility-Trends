setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#Separate data by each measure############################################################################# 
#VMT vs. others 
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,-c(1,3,4)]
VMT.dat.s<-as.data.frame(VMT.dat.s)

#GHG vs. others 
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,-c(1,2,4)]
GHG.dat.s<-as.data.frame(GHG.dat.s)

#TMS vs. others
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-as.data.frame(TMS.dat.s)
############################################################################################################


#Perform Ridge Regression for VMT ##########################################################################
#library(glmnet)
lambdas=10^seq(50,-50,by=-.1)
ridge.mod=glmnet(VMT.dat.s[,-1], VMT.dat.s[,1], nlambda=100, alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.s[,-1]), VMT.dat.s[,1],alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=5)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
coef(ridge.mod, s= optimal.lambda)

best_ridge_coef<-as.numeric(coef(ridge.mod, s= optimal.lambda))[-1]
############################################################################################################

#Adaptive LASSO Regression for VMT####################################################################################
#library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(VMT.dat.s[,-1], VMT.dat.s[,1], alpha=1, penalty.factor=1/abs(best_ridge_coef) , family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.s[,-1]), VMT.dat.s[,1],alpha=1, penalty.factor=1/abs(best_ridge_coef), type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min
optimal.lambda
plot(cv.lasso)
coef(lasso.mod, s=optimal.lambda)

plot(lasso.mod, xvar="lambda", lwd=2, label=T)
abline(v=log(cv.lasso$lambda.min), col="blue", lty=2)

#library(coefplot)
coefpath(lasso.mod)
coefplot(lasso.mod, lambda=cv.lasso$lambda.min, sort="magnitude")

lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]



#Perform Ridge Regression for GHG ##########################################################################
#library(glmnet)
lambdas=10^seq(50,-50,by=-.1)
ridge.mod=glmnet(GHG.dat.s[,-1], GHG.dat.s[,1], nlambda=100, alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.s[,-1]), GHG.dat.s[,1],alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=5)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
coef(ridge.mod, s= optimal.lambda)

best_ridge_coef<-as.numeric(coef(ridge.mod, s= optimal.lambda))[-1]
############################################################################################################

#Adaptive LASSO Regression - GHG################################################################################################
#library(glmnet)
lambdas=10^seq(10,-10,by=-.1)
lasso.mod=glmnet(GHG.dat.s[,-1], GHG.dat.s[,1], alpha=1, penalty.factor=1/abs(best_ridge_coef), family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.s[,-1]), GHG.dat.s[,1],alpha=1, penalty.factor=1/abs(best_ridge_coef), type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
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


#Perform Ridge Regression for TMS ##########################################################################
#library(glmnet)
lambdas=10^seq(50,-50,by=-.1)
ridge.mod=glmnet(TMS.dat.s[,-1], TMS.dat.s[,1], nlambda=100, alpha=0, family="gaussian" , lambda=lambdas)
summary(ridge.mod)
cv.ridge<-cv.glmnet(as.matrix(TMS.dat.s[,-1]), TMS.dat.s[,1],alpha=0,type="deviance", family="gaussian", standardize=TRUE, nfolds=5)
optimal.lambda <- cv.ridge$lambda.min
optimal.lambda
coef(ridge.mod, s= optimal.lambda)

best_ridge_coef<-as.numeric(coef(ridge.mod, s= optimal.lambda))[-1]
############################################################################################################

#Adaptive LASSO Regression - TMS################################################################################################
#library(glmnet)
lambdas=10^seq(10,-10,by=-.1)
lasso.mod=glmnet(TMS.dat.s[,-1], TMS.dat.s[,1], alpha=1, penalty.factor=1/abs(best_ridge_coef), family="gaussian" , lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.s[,-1]), TMS.dat.s[,1],alpha=1, penalty.factor=1/abs(best_ridge_coef), type="deviance", family="gaussian", standardize=TRUE, nfolds=16, lambda=lambdas)
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


