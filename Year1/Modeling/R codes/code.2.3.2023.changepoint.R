setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,1])

#Ridge Regression - whole data
lambdas=10^seq(5,-5,by=-.1)
VMT.lasso<-glmnet(VMT.dat.sub[,-c(1,7)], VMT.dat.sub[,1], alpha=0, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[,-c(1,7)]), VMT.dat.sub[,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

VMT.pred<-coef(VMT.lasso, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,20), VMT.dat.sub[,-c(1,7)])) #fitted values

#Plot whole data vs. predicted VMT
par(mfrow=c(1,1))
plot(dat[,1],VMT.dat.sub[,1], xlim=c(2000,2020), ylim=c(-1.5,2.2), xlab="Year", ylab="VMT", main="VMT vs. Fitted")
lines(dat[,1],VMT.pred,  xlim=c(2000,2020), ylim=c(-1.5,2.2),col="blue")


#Plot residuals
VMT.resid<-VMT.dat.sub[,1]-VMT.pred
plot(dat[,1], VMT.resid, xlab="Year", ylab="VMT Residual", type="l", pch=16, main="VMT Residuals")

VMT.ts<-ts(VMT.dat.sub[,1], start=c(2000,1), end=c(2019,1), frequency=1)
VMT.ts.res<-ts(as.numeric(VMT.resid), start=c(2000,1), end=c(2019,1), frequency=1)


#library(changepoint) - mean and variance change
#Change-point in mean - method = AMOC for 1 change point, PELT for multiple
plot(cpt.mean(VMT.ts, method="PELT"), ylab="VMT", main="VMT - Change-point in mean")
plot(cpt.mean(VMT.ts.res, method="PELT"), ylab="VMT Residual" ,main="VMT Residuals - Change-point in mean")

plot(cpt.var(VMT.ts, method="PELT"), ylab="VMT", main="VMT - Change-point in variance")
plot(cpt.var(as.numeric(VMT.resid), method="PELT"), ylab="VMT", main="VMT Residuals - Change-point in variance")

plot(cpt.meanvar(VMT.ts, method="PELT"), main="VMT - Change-point in mean/variance")
plot(cpt.meanvar(as.numeric(VMT.resid), method="PELT"), main="VMT Residuals - Change-point in mean/variance")


#library(strucchange) - structural change 
VMT.ts.dat<-ts(VMT.dat.sub, start=c(2000,1), end=c(2019,1), frequency=1)
plot(dat[,1], residuals(lm(VMT.dat.sub$VMT~VMT.dat.sub$Unemployment.Rate.1)))
qlr<- Fstats(VMT ~ Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+VMT.1, data=VMT.ts.dat)
breakpoints(qlr)
sctest(qlr, type = "supF")
plot(qlr,  main="VMT - Change-point in strucutre")

Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+VMT.1

par(mfrow=c(3,2))
qlr<- Fstats(VMT ~ Unemployment.Rate.1, data=VMT.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(VMT.dat.sub[2:20,]$Unemployment.Rate.1, VMT.dat.sub[2:20,]$VMT, xlab="Unemployment", ylab="VMT", main="VMT - Structural Change Point")
lines(breakpoints(qlr), xlab="Unemployment",, ylab="VMT", col="red")

qlr<- Fstats(VMT ~ TotLaneMiles, data=VMT.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(VMT.dat.sub[2:20,]$TotLaneMiles, VMT.dat.sub[2:20,]$VMT, xlab="Total Lane Miles", ylab="VMT", main="VMT - Structural Change Point")
lines(breakpoints(qlr), xlab="Total Lane Miles",, ylab="VMT", col="red")

qlr<- Fstats(VMT ~ TransitPMT, data=VMT.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(VMT.dat.sub[2:20,]$TransitPMT, VMT.dat.sub[2:20,]$VMT, xlab="Transit PMT", ylab="VMT", main="VMT - Structural Change Point")
lines(breakpoints(qlr), xlab="Transit PMT",, ylab="VMT", col="red")

qlr<- Fstats(VMT ~ EVPrivateandPublicChargingStationLocations, data=VMT.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(VMT.dat.sub[2:20,]$EVPrivateandPublicChargingStationLocations, VMT.dat.sub[2:20,]$VMT, xlab="EVPrivateandPublicChargingStationLocations", ylab="VMT", main="VMT - Structural Change Point")
lines(breakpoints(qlr), xlab="EVPrivateandPublicChargingStationLocations",, ylab="VMT", col="red")

qlr<- Fstats(VMT ~ populationTelework, data=VMT.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(VMT.dat.sub[2:20,]$populationTelework, VMT.dat.sub[2:20,]$VMT, xlab="Population Telework", ylab="VMT", main="VMT - Structural Change Point")
lines(breakpoints(qlr), xlab="Population Telework",, ylab="VMT", col="red")

qlr<- Fstats(VMT ~ VMT.1, data=VMT.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(VMT.dat.sub[2:20,]$VMT.1, VMT.dat.sub[2:20,]$VMT, xlab="VMT(t-1)", ylab="VMT", main="VMT - Structural Change Point")
lines(breakpoints(qlr), xlab="VMT(t-1)",, ylab="VMT", col="red")



#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
  
#LASSO - Whole data with last year
lambdas=10^seq(5,-5,by=-.1)
GHG.lasso.in.1<-glmnet(GHG.dat.sub[2:20,-c(1)], GHG.dat.sub[2:20,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[2:20,-c(1)]), GHG.dat.sub[2:20,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=19, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,19), GHG.dat.sub[2:20,-c(1)])) #fitted values

#Plot whole data vs. predicted VMT
par(mfrow=c(1,1))
plot(dat[,1],GHG.dat.sub[,1], xlim=c(2000,2020), ylim=c(-1.5,2.2), xlab="Year", ylab="GHG", main="GHG vs. Fitted")
lines(dat[,1],GHG.pred.1,  xlim=c(2000,2020), ylim=c(-1.5,2.2),col="blue")

#Plot residuals
GHG.resid<-GHG.dat.sub[2:20,1]-GHG.pred.1
plot(dat[2:20,1], GHG.resid, xlab="Year", ylab="GHG Residual", type="l", pch=16, main="GHG Residuals")

GHG.ts<-ts(GHG.dat.sub[,1], start=c(2001,1), end=c(2019,1), frequency=1)
GHG.ts.res<-ts(as.numeric(GHG.resid), start=c(2001,1), end=c(2019,1), frequency=1)


#library(changepoint) - mean and variance change
#Change-point in mean - method = AMOC for 1 change point, PELT for multiple
plot(cpt.mean(GHG.ts, method="PELT"),ylab="GHG", main="GHG - Change-point in mean")
plot(cpt.mean(GHG.ts.res, method="PELT"),ylab="GHG", main="GHG Residuals - Change-point in mean")

plot(cpt.var(GHG.ts, method="PELT"), ylab="GHG",main="GHG - Change-point in variance")
plot(cpt.var(as.numeric(GHG.resid), method="PELT"), ylab="GHG Residual",main="GHG Residuals - Change-point in variance")

plot(cpt.meanvar(VMT.ts, method="PELT"), main="VMT - Change-point in mean/variance")
plot(cpt.meanvar(as.numeric(VMT.resid), method="PELT"), main="VMT Residuals - Change-point in mean/variance")


#library(strucchange) - structural change 
plot(dat[,1], residuals(lm(GHG.dat.sub$GHG~GHG.dat.sub$Unemployment.Rate.1)))
qlr<- Fstats(TransportGHG ~ Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+GHG.1, data=GHG.dat.sub[2:20,])
breakpoints(qlr)
sctest(qlr, type = "supF")
plot(qlr, main= "GHG - Change-point in strucutre")


Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+GHG.1

par(mfrow=c(3,2))
qlr<- Fstats(TransportGHG ~ Unemployment.Rate.1, data=GHG.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(GHG.dat.sub[2:20,]$Unemployment.Rate.1, GHG.dat.sub[2:20,]$TransportGHG, xlab="Unemployment", ylab="GHG", main="GHG - Structural Change Point")
lines(breakpoints(qlr), xlab="Unemployment",, ylab="GHG", col="red")

qlr<- Fstats(TransportGHG ~ TotLaneMiles, data=GHG.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(GHG.dat.sub[2:20,]$TotLaneMiles, GHG.dat.sub[2:20,]$TransportGHG, xlab="Total Lane Miles", ylab="GHG", main="GHG - Structural Change Point")
lines(breakpoints(qlr), xlab="Total Lane Miles",, ylab="GHG", col="red")

qlr<- Fstats(TransportGHG ~ TransitPMT, data=GHG.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(GHG.dat.sub[2:20,]$TransitPMT, GHG.dat.sub[2:20,]$TransportGHG, xlab="Transit PMT", ylab="GHG", main="GHG - Structural Change Point")
lines(breakpoints(qlr), xlab="Transit PMT",, ylab="GHG", col="red")

qlr<- Fstats(TransportGHG ~ EVPrivateandPublicChargingStationLocations, data=GHG.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(GHG.dat.sub[2:20,]$EVPrivateandPublicChargingStationLocations, GHG.dat.sub[2:20,]$TransportGHG, xlab="EVPrivateandPublicChargingStationLocations", ylab="GHG", main="GHG - Structural Change Point")
lines(breakpoints(qlr), xlab="EVPrivateandPublicChargingStationLocations",, ylab="GHG", col="red")

qlr<- Fstats(TransportGHG ~ populationTelework, data=GHG.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(GHG.dat.sub[2:20,]$populationTelework, GHG.dat.sub[2:20,]$TransportGHG, xlab="Population Telework", ylab="GHG", main="GHG - Structural Change Point")
lines(breakpoints(qlr), xlab="Population Telework",, ylab="GHG", col="red")

qlr<- Fstats(TransportGHG ~ GHG.1, data=GHG.dat.sub[2:20,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(GHG.dat.sub[2:20,]$GHG.1, GHG.dat.sub[2:20,]$TransportGHG, xlab="GHG(t-1)", ylab="GHG", main="GHG - Structural Change Point")
lines(breakpoints(qlr), xlab="GHG(t-1)",, ylab="GHG", col="red")


par(mfrow=c(1,1))





#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])


#LASSO - Training vs. Testing (with TMS(t-1))
TMS.lasso.in.1<-glmnet(TMS.dat.sub[2:16,-1], TMS.dat.sub[2:16,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[2:16,-1]), TMS.dat.sub[2:16,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=15, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

TMS.pred.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,15), TMS.dat.sub[2:16,-c(1)])) #fitted values

#Plot whole data vs. predicted TMS
par(mfrow=c(2,1))
plot(dat[6:20,1],TMS.dat.sub[2:16,1], xlim=c(2000,2020), ylim=c(-1.5,2.2), xlab="Year", ylab="TMS", main="TMS vs. Fitted")
lines(dat[6:20,1],TMS.pred.1,  xlim=c(2000,2020), ylim=c(-1.5,2.2),col="blue")

#Plot residuals
TMS.resid<-TMS.dat.sub[2:16,1]-TMS.pred.1
plot(dat[6:20,1], TMS.resid, xlab="Year", ylab="TMS Residual", type="l", pch=16, main="TMS Residuals")

TMS.ts<-ts(TMS.dat.sub[2:16,1], start=c(2006,1), end=c(2019,1), frequency=1)
TMS.ts.res<-ts(as.numeric(TMS.resid), start=c(2006,1), end=c(2019,1), frequency=1)


#library(changepoint) - mean and variance change
#Change-point in mean - method = AMOC for 1 change point, PELT for multiple
plot(cpt.mean(TMS.ts, method="PELT"), ylab="TMS", main="TMS - Change-point in mean")
plot(cpt.mean(TMS.ts.res, method="PELT"), ylab="TMS Residuals", main="TMS Residuals - Change-point in mean")

plot(cpt.var(TMS.ts, method="PELT"), ylab="TMS", main="TMS - Change-point in variance")
plot(cpt.var(as.numeric(TMS.resid), method="PELT"),ylab="TMS Residuals", main="TMS Residuals - Change-point in variance")

plot(cpt.meanvar(TMS.ts, method="PELT"), main="TMS - Change-point in mean/variance")
plot(cpt.meanvar(as.numeric(TMS.resid), method="PELT"), main="TMS Residuals - Change-point in mean/variance")


#library(strucchange) - structural change 
plot(dat[,1], residuals(lm(GHG.dat.sub$GHG~GHG.dat.sub$Unemployment.Rate.1)))
#TotalArtToFwyMiles+TransitUPT+RegisteredBEVCarsMillions+populationTelework+TMS.1
qlr<- Fstats(TransitModeShare ~ Unemployment.Rate.1+TotalArtToFwyMiles+TransitUPT+RegisteredBEVCarsMillions+populationTelework+TMS.1, data=TMS.dat.sub[2:16,])
breakpoints(qlr)
sctest(qlr, type = "supF")
plot(qlr, main= "TMS - Change-point in strucutre")


par(mfrow=c(3,2))
qlr<- Fstats(TransitModeShare ~ Unemployment.Rate.1, data=TMS.dat.sub[2:16,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(TMS.dat.sub[2:16,]$Unemployment.Rate.1, TMS.dat.sub[2:16,]$TransitModeShare, xlab="Unemployment", ylab="TMS", main="TMS - Structural Change Point")
lines(breakpoints(qlr), xlab="Unemployment", ylab="TMS", col="red")

qlr<- Fstats(TransitModeShare ~ TotalArtToFwyMiles, data=TMS.dat.sub[2:16,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(TMS.dat.sub[2:16,]$TotalArtToFwyMiles, TMS.dat.sub[2:16,]$TransitModeShare, xlab="TotalArtToFwyMiles", ylab="TMS", main="TMS - Structural Change Point")
lines(breakpoints(qlr), xlab="TotalArtToFwyMiles", ylab="TMS", col="red")

qlr<- Fstats(TransitModeShare ~ TransitUPT, data=TMS.dat.sub[2:16,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(TMS.dat.sub[2:16,]$TransitUPT, TMS.dat.sub[2:16,]$TransitModeShare, xlab="Transit UPT", ylab="TMS", main="TMS - Structural Change Point")
lines(breakpoints(qlr), xlab="Transit UPT", ylab="TMS", col="red")

qlr<- Fstats(TransitModeShare ~ RegisteredBEVCarsMillions, data=TMS.dat.sub[2:16,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(TMS.dat.sub[2:16,]$RegisteredBEVCarsMillions, TMS.dat.sub[2:16,]$TransitModeShare, xlab="RegisteredBEVCarsMillions", ylab="TMS", main="TMS - Structural Change Point")
lines(breakpoints(qlr), xlab="RegisteredBEVCarsMillions", ylab="TMS", col="red")

qlr<- Fstats(TransitModeShare ~ populationTelework, data=TMS.dat.sub[2:16,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(TMS.dat.sub[2:16,]$populationTelework, TMS.dat.sub[2:16,]$TransitModeShare, xlab="Population Telework", ylab="TMS", main="TMS - Structural Change Point")
lines(breakpoints(qlr), xlab="Population Telework", ylab="TMS", col="red")

qlr<- Fstats(TransitModeShare ~ TMS.1, data=TMS.dat.sub[2:16,])
sctest(qlr, type = "supF")
breakpoints(qlr)
plot(TMS.dat.sub[2:16,]$TMS.1, TMS.dat.sub[2:16,]$TransitModeShare, xlab="TMS(t-1)", ylab="TMS", main="TMS - Structural Change Point")
lines(breakpoints(qlr), xlab="TMS(t-1)", ylab="TMS", col="red")

TransitModeShare RegisteredBEVCarsMillions+populationTelework+TMS.1
