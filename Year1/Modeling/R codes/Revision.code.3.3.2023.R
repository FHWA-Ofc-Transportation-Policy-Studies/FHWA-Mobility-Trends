setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#library(glmnet)

#A list of subset X variables: 
#Employment: Unemployment.Rate.1 
#Hwys: TotLaneMiles, TotalArtToFwyMiles
#Transit: TransitPMT, TransitUPT
#Telework: populationTelework
#EVs: EVPrivateandPublicChargingStationLocations

#Create variables: VMT.Cap=VMT/Population and GHG.Cap<-GHG/Population
dat$VMT.Cap<-dat$VMT/dat$Population
dat$GHG.Cap<-dat$TransportGHG/dat$Population

dat.sub<-dat[,c("year","VMT.Cap","GHG.Cap","Unemployment.Rate.1","TotLaneMiles","TotalArtToFwyMiles",
                "TransitPMT","TransitUPT","populationTelework","EVPrivateandPublicChargingStationLocations")]
dat.sub$VMT.Cap.t1<-c(NA,dat.sub[1:19,"VMT.Cap"])  #add lag variables
dat.sub$GHG.Cap.t1<-c(NA,dat.sub[1:19,"GHG.Cap"])  #add lag variables

dat.sub.y<-dat[,c("year","VMT","TransportGHG","Unemployment.Rate.1","TotLaneMiles","TotalArtToFwyMiles",
                  "TransitPMT","TransitUPT","populationTelework","EVPrivateandPublicChargingStationLocations","Population")]
dat.sub.y$VMT.t1<-c(NA,dat.sub.y[1:19,"VMT"])  #add lag variables
dat.sub.y$GHG.t1<-c(NA,dat.sub.y[1:19,"TransportGHG"])  #add lag variables


#Scale the data
dat.sub.scale<-scale(dat.sub)
dat.sub.scale<-as.data.frame(dat.sub.scale)

par(mfrow=c(2,1))
plot(dat$Population, dat$VMT)
plot(dat$Population, dat$VMT.Cap)

plot(dat$Population, dat$TransportGHG)
plot(dat$Population, dat$GHG.Cap)

###########VMT#####################
#Select variables using Ridge/LASSO #library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
VMT.ridge.in<-glmnet(dat.sub.scale[2:18,-c(1,2,3,11,12)], dat.sub.scale[2:18,"VMT.Cap"], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(dat.sub.scale[2:18,-c(1,2,3,11,12)]), dat.sub.scale[2:18,"VMT.Cap"],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min
plot(cv.ridge)
coef(VMT.ridge.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.ridge.in)
coefplot(VMT.ridge.in, lambda=cv.ridge$lambda.min, sort="magnitude",title="Ridge Regression")
lambda.rsq<-cbind(cv.ridge$lambda,VMT.ridge.in$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

lambdas=10^seq(5,-5,by=-.1)
VMT.lasso.in<-glmnet(dat.sub.scale[2:18,-c(1,2,3,11,12)], dat.sub.scale[2:18,"VMT.Cap"], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(dat.sub.scale[2:18,-c(1,2,3,11,12)]), dat.sub.scale[2:18,"VMT.Cap"],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min
plot(cv.lasso)
coef(VMT.lasso.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.lasso.in)
coefplot(VMT.lasso.in, lambda=cv.lasso$lambda.min, sort="magnitude",title="LASSO Regression")
lambda.rsq<-cbind(cv.lasso$lambda,VMT.lasso.in$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]



#VMT: Selected Variable.  Fit the model with the selected variables (Standardized.)
#Unemployment	TotLaneMiles TransitPMT EVPrivateandPublicChargingStationLocations %populationTelework
lambdas=10^seq(5,-5,by=-.1)
VMT.ridge.in<-glmnet(dat.sub.scale[2:18,-c(1,2,3,6,8,12)], dat.sub.scale[2:18,"VMT.Cap"], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(dat.sub.scale[2:18,-c(1,2,3,6,8,12)]), dat.sub.scale[2:18,"VMT.Cap"],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min
plot(cv.ridge)
coef(VMT.ridge.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.ridge.in)
coefplot(VMT.ridge.in, lambda=cv.ridge$lambda.min, sort="magnitude",title="Ridge Regression")
lambda.rsq<-cbind(cv.ridge$lambda,VMT.ridge.in$dev.ratio, cv.ridge$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

#library(glmnetSE)
#Compute p-values based on nonparametric bootstrapping
summary(glmnetSE(data=dat.sub.scale[2:18,-c(1,3,12)], 
cf.no.shrnkg = c("VMT.Cap","Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework","VMT.Cap.t1"), alpha=0,r=500, method="none",seed = 1234, ncore = 1))

a<-lm(VMT.Cap~Unemployment.Rate.1+TotLaneMiles+TransitPMT+populationTelework+EVPrivateandPublicChargingStationLocations, data=dat.sub.scale)
summary(a)

#Make prediction
VMT.pred.in<-coef(VMT.ridge.in, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), dat.sub.scale[2:18,-c(1,2,3,6,8,12)])) #fitted values
VMT.pred.out<-coef(VMT.ridge.in, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),dat.sub.scale[19:20,-c(1,2,3,6,8,12)])) 

resid.ridge<-dat.sub.scale[2:18,2]-VMT.pred.in; r2<-1-sum(resid.ridge^2)/sum((dat.sub.scale[2:18,2]-mean(dat.sub.scale[2:18,2]))^2 );r2
DW.ridge<-sum((resid.ridge[2:17]-resid.ridge[1:16])^2)/sum((resid.ridge[2:17])^2) #DW=2.72

MSE.in.VMT<- sum( (dat.sub.scale[2:18,2] - VMT.pred.in)^2)/17; MSE.in.VMT
MSE.out.VMT<- sum( (dat.sub.scale[19:20,2] - VMT.pred.out)^2 )/2; MSE.out.VMT

MAE.in.VMT<- sum( abs(dat.sub.scale[2:18,2] - VMT.pred.in) )/17; MAE.in.VMT
MAE.out.VMT<- sum( abs(dat.sub.scale[19:20,2] - VMT.pred.out) )/2; MAE.out.VMT


X.X<-t(as.matrix(cbind(rep(1,17), dat.sub.scale[2:18,-c(1,2,3,6,8,12)]))) %*% as.matrix(cbind(rep(1,17), dat.sub.scale[2:18,-c(1,2,3,6,8,12)]) )
x0<-cbind(1, dat.sub.scale[19,-c(1,2,3,6,8,12)])  #2019 X values
x1<-cbind(1, dat.sub.scale[20,-c(1,2,3,6,8,12)])  #2020 X values

#Plot the best model 
lo.1<-VMT.pred.out[1]-qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.1<-VMT.pred.out[1]+qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-VMT.pred.out[2]-qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.2<-VMT.pred.out[2]+qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[2:18,1], dat.sub.scale[2:18,2], xlim=c(2000,2020), ylim=c(-2,2), xlab="Year", ylab="VMT/Population")
lines(dat[2:18,1],VMT.pred.in,  xlim=c(2000,2020), ylim=c(-2,2),col="blue")
lines(dat[19:20,1], dat.sub.scale[19:20,2], xlim=c(2000,2020),ylim=c(-2,2), type="p", pch=16)
lines(dat[19:20,1], VMT.pred.out, xlim=c(2000,2020), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")


dat.sub<-dat.sub.y


#VMT: Selected Variable.  Fit the model with the selected variables (Non-standardized.)
#Unemployment	TotLaneMiles TransitPMT EVPrivateandPublicChargingStationLocations %populationTelework
lambdas=10^seq(10,-10,by=-.1)
VMT.ridge.in<-glmnet(dat.sub[2:18,-c(1,2,3,6,8,12)], dat.sub[2:18,"VMT.Cap"], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(dat.sub[2:18,-c(1,2,3,6,8,12)]), dat.sub[2:18,"VMT.Cap"],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min
plot(cv.ridge)
coef(VMT.ridge.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.ridge.in)

#library(glmnetSE)
#Compute p-values based on nonparametric bootstrapping
summary(glmnetSE(data=dat.sub[2:18,-c(1,3,13)], 
                 cf.no.shrnkg = c("VMT","Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework","Population","VMT.t1"), alpha=0,r=500, method="none",seed = 1234, ncore = 1))

a<-lm(VMT.Cap~Unemployment.Rate.1+TotLaneMiles+TransitPMT+populationTelework+EVPrivateandPublicChargingStationLocations, data=dat.sub)
summary(a)

#Make prediction
VMT.pred.in<-coef(VMT.ridge.in, s=optimal.lambda)[1:8] %*% t(cbind(rep(1,17), dat.sub[2:18,-c(1,2,3,6,8,13)])) #fitted values
VMT.pred.out<-coef(VMT.ridge.in, s=optimal.lambda)[1:8] %*% t(cbind(rep(1,2),dat.sub[19:20,-c(1,2,3,6,8,13)])) 

resid.ridge<-dat.sub[2:18,2]-VMT.pred.in; r2<-1-sum(resid.ridge^2)/sum((dat.sub[2:18,2]-mean(dat.sub[2:18,2]))^2 );r2
DW.ridge<-sum((resid.ridge[2:17]-resid.ridge[1:16])^2)/sum((resid.ridge[2:17])^2) #DW=2.72

MSE.in.VMT<- sum( (dat.sub[2:18,2] - VMT.pred.in)^2)/17; MSE.in.VMT
MSE.out.VMT<- sum( (dat.sub[19:20,2] - VMT.pred.out)^2 )/2; MSE.out.VMT

MAE.in.VMT<- sum( abs(dat.sub[2:18,2] - VMT.pred.in) )/17; MAE.in.VMT
MAE.out.VMT<- sum( abs(dat.sub[19:20,2] - VMT.pred.out) )/2; MAE.out.VMT


X.X<-t(as.matrix(cbind(rep(1,17), dat.sub[2:18,-c(1,2,3,6,8,13)]))) %*% as.matrix(cbind(rep(1,17), dat.sub[2:18,-c(1,2,3,6,8,13)]) )
x0<-cbind(1, dat.sub[19,-c(1,2,3,6,8,13)])  #2019 X values
x1<-cbind(1, dat.sub[20,-c(1,2,3,6,8,13)])  #2020 X values

#Plot the best model 
lo.1<-VMT.pred.out[1]-qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X, tol=1e-33) %*% t(as.matrix(x0))))
hi.1<-VMT.pred.out[1]+qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X, tol=1e-33) %*% t(as.matrix(x0))))
lo.2<-VMT.pred.out[2]-qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X, tol=1e-33) %*% t(as.matrix(x1))))
hi.2<-VMT.pred.out[2]+qt(.975,15)*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X, tol=1e-33) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[2:18,1], dat.sub[2:18,2], xlim=c(2000,2020),ylim=c(2.7e+12, 3.5e+12), xlab="Year", ylab="VMT")
lines(dat[2:18,1],VMT.pred.in,  xlim=c(2000,2020),col="blue")
lines(dat[19:20,1], dat.sub[19:20,2], xlim=c(2000,2020), type="p", pch=16)
lines(dat[19:20,1], VMT.pred.out, xlim=c(2000,2020), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")








###########GHG#####################
#Select variables using Ridge/LASSO #library(glmnet)
lambdas=10^seq(5,-5,by=-.1)
VMT.ridge.in<-glmnet(dat.sub.scale[2:18,-c(1,2,3,11,12)], dat.sub.scale[2:18,"GHG.Cap"], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(dat.sub.scale[2:18,-c(1,2,3,11,12)]), dat.sub.scale[2:18,"GHG.Cap"],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min
plot(cv.ridge)
coef(VMT.ridge.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.ridge.in)
coefplot(VMT.ridge.in, lambda=cv.ridge$lambda.min, sort="magnitude",title="Ridge Regression")
lambda.rsq<-cbind(cv.ridge$lambda,VMT.ridge.in$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

lambdas=10^seq(5,-5,by=-.1)
VMT.lasso.in<-glmnet(dat.sub.scale[2:18,-c(1,2,3,11,12)], dat.sub.scale[2:18,"GHG.Cap"], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(dat.sub.scale[2:18,-c(1,2,3,11,12)]), dat.sub.scale[2:18,"GHG.Cap"],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min
plot(cv.lasso)
coef(VMT.lasso.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.lasso.in)
coefplot(VMT.lasso.in, lambda=cv.lasso$lambda.min, sort="magnitude",title="LASSO Regression")
lambda.rsq<-cbind(cv.lasso$lambda,VMT.lasso.in$dev.ratio, cv.lasso$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]











#GHG: Selected Variable.  Fit the model with the selected variables. (Standardized.) 
#Unemployment	TransitPMT EVPrivateandPublicChargingStationLocations %populationTelework
lambdas=10^seq(5,-5,by=-.1)
GHG.ridge.in<-glmnet(dat.sub.scale[1:18,-c(1,2,3,5,6,8,11,12)], dat.sub.scale[1:18,"GHG.Cap"], alpha=1, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(dat.sub.scale[1:18,-c(1,2,3,5,6,8,11,12)]), dat.sub.scale[1:18,"GHG.Cap"],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min
plot(cv.ridge)
coef(GHG.ridge.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.ridge.in)
coefplot(GHG.ridge.in, lambda=cv.ridge$lambda.min, sort="magnitude",title="Ridge Regression")
lambda.rsq<-cbind(cv.ridge$lambda,GHG.ridge.in$dev.ratio, cv.ridge$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

#library(glmnetSE)
#Compute p-values based on nonparametric bootstrapping
summary(glmnetSE(data=dat.sub.scale[1:18,-c(1,2,11)], 
                 cf.no.shrnkg = c("GHG.Cap","Unemployment.Rate.1","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), 
                 alpha=1,r=500, method="none",seed = 1234, ncore = 1))

a<-lm(GHG.Cap~Unemployment.Rate.1+TransitPMT+populationTelework+EVPrivateandPublicChargingStationLocations, data=dat.sub.scale)
summary(a)

#Make prediction
GHG.pred.in<-coef(GHG.ridge.in, s=optimal.lambda)[1:5] %*% t(cbind(rep(1,18), dat.sub.scale[1:18,-c(1,2,3,5,6,8,11,12)])) #fitted values
GHG.pred.out<-coef(GHG.ridge.in, s=optimal.lambda)[1:5] %*% t(cbind(rep(1,2),dat.sub.scale[19:20,-c(1,2,3,5,6,8,11,12)])) 

resid.ridge<-dat.sub.scale[1:18,3]-GHG.pred.in; r2<-1-sum(resid.ridge^2)/sum((dat.sub.scale[1:18,3]-mean(dat.sub.scale[1:18,3]))^2 );r2
DW.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)/sum((resid.ridge[2:18])^2) #DW=2.42

MSE.in.GHG<- sum( (dat.sub.scale[1:18,3] - GHG.pred.in)^2)/18; MSE.in.VMT
MSE.out.GHG<- sum( (dat.sub.scale[19:20,3] - GHG.pred.out)^2 )/2; MSE.out.VMT

MAE.in.VMT<- sum( abs(dat.sub.scale[1:18,3] - GHG.pred.in) )/18; MAE.in.VMT
MAE.out.VMT<- sum( abs(dat.sub.scale[19:20,3] - GHG.pred.out) )/2; MAE.out.VMT


X.X<-t(as.matrix(cbind(rep(1,18), dat.sub.scale[1:18,-c(1,2,3,5,6,8,11,12)]))) %*% as.matrix(cbind(rep(1,18), dat.sub.scale[1:18,-c(1,2,3,5,6,8,11,12)]) )
x0<-cbind(1, dat.sub.scale[19,-c(1,2,3,5,6,8,11,12)])  #2019 X values
x1<-cbind(1, dat.sub.scale[20,-c(1,2,3,5,6,8,11,12)])  #2020 X values

#Plot the best model 
lo.1<-GHG.pred.out[1]-qt(.975,16)*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.1<-GHG.pred.out[1]+qt(.975,16)*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-GHG.pred.out[2]-qt(.975,16)*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.2<-GHG.pred.out[2]+qt(.975,16)*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[1:18,1], dat.sub.scale[1:18,3], xlim=c(2000,2020), ylim=c(-2,2), xlab="Year", ylab="GHG/Population")
lines(dat[1:18,1],GHG.pred.in,  xlim=c(2000,2020), ylim=c(-2,2),col="blue")
lines(dat[19:20,1], dat.sub.scale[19:20,3], xlim=c(2000,2020),ylim=c(-2,2), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out, xlim=c(2000,2020), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")





#GHG: Selected Variable.  Fit the model with the selected variables. (Non-Standardized.) 
#Unemployment	TransitPMT EVPrivateandPublicChargingStationLocations %populationTelework
lambdas=10^seq(5,-5,by=-.1)
GHG.ridge.in<-glmnet(dat.sub[1:18,-c(1,2,3,5,6,8,11,12)], 10^6*dat.sub[1:18,"GHG.Cap"], alpha=1, family="gaussian",standardize=TRUE,lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(dat.sub[1:18,-c(1,2,3,5,6,8,11,12)]), 10^6*dat.sub[1:18,"GHG.Cap"],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min
plot(cv.ridge)
coef(GHG.ridge.in, s=optimal.lambda)
#library(coefplot)
#coefpath(VMT.ridge.in)
coefplot(GHG.ridge.in, lambda=cv.ridge$lambda.min, sort="magnitude",title="Ridge Regression")
lambda.rsq<-cbind(cv.ridge$lambda,GHG.ridge.in$dev.ratio, cv.ridge$nzero)
lambda.rsq[lambda.rsq[,1]==optimal.lambda]

#library(glmnetSE)
#Compute p-values based on nonparametric bootstrapping
summary(glmnetSE(data=dat.sub[1:18,-c(1,2,11)], 
                 cf.no.shrnkg = c("GHG.Cap","Unemployment.Rate.1","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), 
                 alpha=1,r=500, method="none",seed = 1234, ncore = 1))

a<-lm(GHG.Cap~Unemployment.Rate.1+TransitPMT+populationTelework+EVPrivateandPublicChargingStationLocations, data=dat.sub.scale)
summary(a)

#Make prediction
GHG.pred.in<-coef(GHG.ridge.in, s=optimal.lambda)[1:5] %*% t(cbind(rep(1,18), dat.sub[1:18,-c(1,2,3,5,6,8,11,12)])) #fitted values
GHG.pred.out<-coef(GHG.ridge.in, s=optimal.lambda)[1:5] %*% t(cbind(rep(1,2),dat.sub[19:20,-c(1,2,3,5,6,8,11,12)])) 

resid.ridge<-10^6*dat.sub[1:18,3]-GHG.pred.in; r2<-1-sum(resid.ridge^2)/sum((10^6*dat.sub[1:18,3]-mean(10^6*dat.sub[1:18,3]))^2 );r2
DW.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)/sum((resid.ridge[2:18])^2) #DW=2.35

MSE.in.GHG<- sum( (10^6*dat.sub[1:18,3] - GHG.pred.in)^2)/18; MSE.in.GHG
MSE.out.GHG<- sum( (10^6*dat.sub[19:20,3] - GHG.pred.out)^2 )/2; MSE.out.GHG

MAE.in.GHG<- sum( abs(10^6*dat.sub[1:18,3] - GHG.pred.in) )/18; MAE.in.GHG
MAE.out.GHG<- sum( abs(10^6*dat.sub[19:20,3] - GHG.pred.out) )/2; MAE.out.GHG


X.X<-t(as.matrix(cbind(rep(1,18), dat.sub[1:18,-c(1,2,3,5,6,8,11,12)]))) %*% as.matrix(cbind(rep(1,18), dat.sub[1:18,-c(1,2,3,5,6,8,11,12)]) )
x0<-cbind(1, dat.sub[19,-c(1,2,3,5,6,8,11,12)])  #2019 X values
x1<-cbind(1, dat.sub[20,-c(1,2,3,5,6,8,11,12)])  #2020 X values

#Plot the best model 
lo.1<-GHG.pred.out[1]-qt(.975,16)*sqrt(MSE.in.GHG*(1+as.matrix(x0) %*% solve(X.X, tol=1e-29) %*% t(as.matrix(x0))))
hi.1<-GHG.pred.out[1]+qt(.975,16)*sqrt(MSE.in.GHG*(1+as.matrix(x0) %*% solve(X.X, tol=1e-29) %*% t(as.matrix(x0))))
lo.2<-GHG.pred.out[2]-qt(.975,16)*sqrt(MSE.in.GHG*(1+as.matrix(x1) %*% solve(X.X, tol=1e-29) %*% t(as.matrix(x1))))
hi.2<-GHG.pred.out[2]+qt(.975,16)*sqrt(MSE.in.GHG*(1+as.matrix(x1) %*% solve(X.X, tol=1e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[1:18,1], dat.sub[1:18,3]*10^6, xlim=c(2000,2020), ylim=c(5.4,7.0), xlab="Year", ylab="GHG/Population (1e-06)")
lines(dat[1:18,1], GHG.pred.in,  xlim=c(2000,2020),col="blue")
lines(dat[19:20,1], dat.sub[19:20,3]*10^6, xlim=c(2000,2020), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out, xlim=c(2000,2020), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")

