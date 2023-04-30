#This script runs Nonlinear LASSO models for each of the performance metrics. These results were not used in the final models or report.

#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)


#Add higher order terms and fit LASSO###################################################################################################
#VMT vs. others 
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,-c(1,3,4)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.s$Unemployment.Rate.1.sq<-VMT.dat.s$Unemployment.Rate.1^2
VMT.dat.s$TransitTotCapacity.sq<-VMT.dat.s$TransitTotCapacity^2

lambdas=10^seq(5,-5,by=-.1)
lasso.mod=glmnet(VMT.dat.s[,-1], VMT.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.s[,-1]), VMT.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
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


#Add higher order terms and fit LASSO###################################################################################################
#GHG vs. others 
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,-c(1,2,4)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.s$TotLaneMiles.sq<-GHG.dat.s$TotLaneMiles^2
GHG.dat.s$TotLaneMiles.cu<-GHG.dat.s$TotLaneMiles^3

lambdas=10^seq(10,-10,by=-.1)
lasso.mod=glmnet(GHG.dat.s[,-1], GHG.dat.s[,1], alpha=1, family="gaussian", lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.s[,-1]), GHG.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
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


#Add higher order terms and fit LASSO###################################################################################################
#TMS vs. others
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.s$TeleworkLowIncome.sq<-TMS.dat.s$TeleworkLowIncome^2

lambdas=10^seq(10,-10,by=-.1)
lasso.mod=glmnet(TMS.dat.s[,-1], TMS.dat.s[,1], alpha=1, family="gaussian" , lambda=lambdas)
summary(lasso.mod)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.s[,-1]), TMS.dat.s[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=16, lambda=lambdas)
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



