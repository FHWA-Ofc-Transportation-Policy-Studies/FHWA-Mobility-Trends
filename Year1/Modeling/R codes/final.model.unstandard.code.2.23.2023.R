setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-VMT.dat[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,1])


#Ridge - Training vs. Testing (without VMT(t-1))
lambdas=10^seq(5,-5,by=-.1)
VMT.ridge.in<-glmnet(VMT.dat.sub[1:18,-c(1,7)], VMT.dat.sub[1:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.sub[1:18,-c(1,7)]), VMT.dat.sub[1:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

VMT.pred.in<-coef(VMT.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)])) #fitted values
VMT.pred.out<-coef(VMT.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1,7)])) 

resid.ridge<-VMT.dat.sub[1:18,1]-VMT.pred.in; r2<-1-sum(resid.ridge^2)/sum(  (VMT.dat.sub[1:18,1]-mean(VMT.dat.sub[1:18,1]))^2 );r2
DW.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)  /sum((resid.ridge[2:18])^2) #DW=1.76

MSE.in.VMT<- sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18
MSE.out.VMT<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2

MAE.in.VMT<- sum( abs(VMT.dat.sub[1:18,1] - VMT.pred.in) )/18
MAE.out.VMT<- sum( abs(VMT.dat.sub[19:20,1] - VMT.pred.out) )/2

#Bootstrap
summary(glmnetSE(data=VMT.dat.s[2:18,], cf.no.shrnkg = c("Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), alpha=0,r=1500, method="none",seed = 1234, ncore = 1))

#Plot
3.045146*sd(VMT.dat$VMT) +mean(VMT.dat$VMT)

plot(dat[1:18,1],VMT.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), xlab="Year", ylab="VMT")
lines(dat[1:18,1],VMT.pred.in,  xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), col="blue")
lines(dat[19:20,1], VMT.dat.sub[19:20,1], xlim=c(2000,2020),ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16)
lines(dat[19:20,1], VMT.pred.out, xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=3.149277e+12, y1=3.340359e+12, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=3.203917e+12, y1=3.435582e+12, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=3.149277e+12, y1=3.149277e+12, col="red")
segments(x0=2017.6,x1=2018.6, y0=3.340359e+12, y1=3.340359e+12, col="red")
segments(x0=2018.6,x1=2019.4, y0=3.203917e+12, y1=3.203917e+12, col="red")
segments(x0=2018.6,x1=2019.6, y0=3.435582e+12, y1=3.435582e+12, col="red")




#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
#GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])


#LASSO - Training vs. Testing (with GHG(t-1)) (BEST model)
lambdas=10^seq(5,-5,by=-.1)
GHG.lasso.in.1<-glmnet(GHG.dat.sub[2:18,-c(1,3,5,6)], GHG.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1,3,5,6)]), GHG.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.in.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:4] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1,3,5,6)])) #fitted values
GHG.pred.out.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:4] %*% t(cbind(rep(1,2), GHG.dat.sub[19:20,-c(1,3,5,6)])) 


#GHG.dat.s$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
#summary(glmnetSE(data=GHG.dat.s[2:18,], cf.no.shrnkg = c("Unemployment.Rate.1","TransitPMT","GHG.1"), alpha=1,r=500, method="none",seed = 1234, ncore = 1))

resid.lasso.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (GHG.dat.sub[1:18,1]-mean(GHG.dat.sub[1:18,1]))^2 );r2
DW.lasso.1<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=3.626901

MSE.in.GHG.1<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.1<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.1<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.1<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2



#Plot
1.1041305*sd(GHG.dat$TransportGHG) +mean(GHG.dat$TransportGHG)

GHG.pred.in.unstand<-GHG.pred.in.1*sd(GHG.dat$TransportGHG) +mean(GHG.dat$TransportGHG)
GHG.pred.out.unstand<-GHG.pred.out.1*sd(GHG.dat$TransportGHG) +mean(GHG.dat$TransportGHG)

plot(dat[1:18,1],GHG.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(1747.5,2106.217), xlab="Year", ylab="GHG")
lines(dat[2:18,1],GHG.pred.in.unstand,  xlim=c(2000,2020), ylim=c(1747.5,2106.217),col="blue")
lines(dat[19:20,1], GHG.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out.unstand, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=1805.807, y1=1933.002, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=1797.044, y1=1950.575, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=1805.807, y1=1805.807, col="red")
segments(x0=2017.6,x1=2018.6, y0=1933.002, y1=1933.002, col="red")
segments(x0=2018.6,x1=2019.4, y0=1797.044, y1=1797.044, col="red")
segments(x0=2018.6,x1=2019.6, y0=1950.575, y1=1950.575, col="red")


#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
#TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])

#LASSO - Training vs. Testing (with TMS(t-1)) (Best model)
lambdas=10^seq(5,-5,by=-.1)
TMS.lasso.in.1<-glmnet(TMS.dat.sub[2:14,-1], TMS.dat.sub[2:14,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[2:14,-1]), TMS.dat.sub[2:14,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=13, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

#coef(TMS.lasso.in, s=optimal.lambda)[2]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[3]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[5]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[6]<-0
TMS.pred.in.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)])) #fitted values
TMS.pred.out.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1)])) 

TMS.dat.s$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
summary(glmnetSE(data=TMS.dat.s[2:15,], cf.no.shrnkg = c("TransitUPT", "TMS.1"), alpha=1,r=300, method="none",seed = 1234, ncore = 1))


MSE.in.TMS.1<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.1<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.1<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.1<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2

#Plot 
0.8834176*sd(TMS.dat$TransitModeShare) +mean(TMS.dat$TransitModeShare)

plot(dat[6:18,1],TMS.dat.sub[2:14,1], xlim=c(2000,2020), ylim=c(0.04708909,0.05265305), xlab="Year", ylab="TMS")
lines(dat[6:18,1],TMS.pred.in.1,  xlim=c(2000,2020), ylim=c(0.04708909,0.05265305),col="blue")
lines(dat[19:20,1], TMS.dat.sub[15:16,1], xlim=c(2000,2020), ylim=c(0.04708909,0.05265305), type="p", pch=16)
lines(dat[19:20,1], TMS.pred.out.1, xlim=c(2000,2020), ylim=c(0.04708909,0.05265305), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=0.04778834, y1=0.05126581, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=0.04740801, y1=0.05123323, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=0.04778834, y1=0.04778834, col="red")
segments(x0=2017.6,x1=2018.6, y0=0.05126581, y1=0.05126581, col="red")
segments(x0=2018.6,x1=2019.4, y0=0.051235, y1=0.051235, col="red")
segments(x0=2018.6,x1=2019.6, y0=0.0474, y1=0.0474, col="red")







