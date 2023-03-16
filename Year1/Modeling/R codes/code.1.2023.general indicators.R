setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)


#Separate data by each measure############################################################################# 
#Add higher order terms###############
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


#LASSO Regression for VMT with general indicators#############################################################
#library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(VMT.dat.s[,c(2,12:30,33)], VMT.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.s[,c(2,12:30,33)]), VMT.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
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
 

#LASSO Regression for GHG with general indicators#############################################################
#library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(GHG.dat.s[,c(2,12:30,33)], GHG.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.s[,c(2,12:30,33)]), GHG.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
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


#LASSO Regression - TMS################################################################################################
#library(glmnet)
lambdas=10^seq(10,-10,by=-.1)
lasso.mod=glmnet(TMS.dat.s[,c(2,12:30,33)], TMS.dat.s[,1], alpha=1, family="gaussian" , lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.s[,c(2,12:30,33)]), TMS.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=16, lambda=lambdas)
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

