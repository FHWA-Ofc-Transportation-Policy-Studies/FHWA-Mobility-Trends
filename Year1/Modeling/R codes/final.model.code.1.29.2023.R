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

#Linear - whole data
VMT.lm<-lm(VMT~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub)
VMT.lm.1<-lm(VMT~VMT.1+Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub[2:20,])

VMT.test.dat<-VMT.dat.sub[19:20,]

#Linear - Training vs. Testing
VMT.lm.train<-lm(VMT~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub[1:18,])
VMT.lm.train.1<-lm(VMT~VMT.1+Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub[2:18,])

resid.lm<-resid(VMT.lm.train); resid.lm.1<-resid(VMT.lm.train.1)
DW<-sum((resid.lm[2:18]-resid.lm[1:17])^2)/sum((resid.lm[2:18])^2) #DW=2.37
resid.lm.1<-resid(VMT.lm.train.1)
DW.1<-sum((resid.lm.1[2:16]-resid.lm.1[1:15])^2)/sum((resid.lm.1[2:16])^2) #DW=2.43

MSE.in.VMT<- sum((VMT.dat.sub$VMT[1:18]-VMT.lm.train$fitted.values)^2)/18
MSE.in.VMT.1<- sum((VMT.dat.sub$VMT[2:18]-VMT.lm.train.1$fitted.values)^2)/17

MAE.in.VMT<- sum(abs(VMT.dat.sub$VMT[1:18]-VMT.lm.train$fitted.values))/18
MAE.in.VMT.1<- sum(abs(VMT.dat.sub$VMT[2:18]-VMT.lm.train.1$fitted.values))/17


VMT.pred.lm<-predict(VMT.lm.train, newdata=VMT.test.dat[,-1])
VMT.pred.lm.1<-predict(VMT.lm.train.1, newdata=VMT.test.dat[,-1])

MSE.out.VMT<- sum((VMT.test.dat[,1]-VMT.pred.lm)^2)/2
MSE.out.VMT.1<- sum((VMT.test.dat[,1]-VMT.pred.lm.1)^2)/2

MAE.out.VMT<- sum(abs(VMT.test.dat[,1]-VMT.pred.lm))/2
MAE.out.VMT.1<- sum(abs(VMT.test.dat[,1]-VMT.pred.lm.1))/2

#LASSO - whole data
lambdas=10^seq(5,-5,by=-.1)
VMT.lasso<-glmnet(VMT.dat.sub[,-c(1,7)], VMT.dat.sub[,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[,-c(1,7)]), VMT.dat.sub[,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

VMT.pred<-coef(VMT.lasso, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,20), VMT.dat.sub[,-c(1,7)])) #fitted values

#LASSO - Training vs. Testing (without VMT(t-1))
VMT.lasso.in<-glmnet(VMT.dat.sub[1:18,-c(1,7)], VMT.dat.sub[1:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[1:18,-c(1,7)]), VMT.dat.sub[1:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

VMT.pred.in<-coef(VMT.lasso.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)])) #fitted values
VMT.pred.out<-coef(VMT.lasso.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1,7)])) 

resid.lasso<-VMT.dat.sub[1:18,1]-VMT.pred.in
DW.lasso<-sum((resid.lasso[2:18]-resid.lasso[1:17])^2)  /sum((resid.lasso[2:18])^2) #DW=1.85

MSE.in.VMT<- sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18
MSE.out.VMT<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2

MAE.in.VMT<- sum( abs(VMT.dat.sub[1:18,1] - VMT.pred.in) )/18
MAE.out.VMT<- sum( abs(VMT.dat.sub[19:20,1] - VMT.pred.out) )/2


#LASSO - Training vs. Testing (with VMT(t-1))
VMT.lasso.in.1<-glmnet(VMT.dat.sub[2:18,-c(1)], VMT.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[2:18,-c(1)]), VMT.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

VMT.pred.in.1<-coef(VMT.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), VMT.dat.sub[2:18,-c(1)])) #fitted values
VMT.pred.out.1<-coef(VMT.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1)])) 

resid.lasso.1<-VMT.dat.sub[2:18,1]-VMT.pred.in.1
DW.lasso.1<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=2.65

MSE.in.VMT.1<- sum( (VMT.dat.sub[2:18,1] - VMT.pred.in.1)^2 )/17
MSE.out.VMT.1<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out.1)^2 )/2

MAE.in.VMT.1<- sum(abs(VMT.dat.sub[2:18,1] - VMT.pred.in.1))/17
MAE.out.VMT.1<- sum(abs(VMT.dat.sub[19:20,1] - VMT.pred.out.1))/2


#Ridge - Training vs. Testing (without VMT(t-1))
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


X.X<-t(as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]))) %*% as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]) )
x0<-cbind(1, VMT.dat.sub[19,-c(1,7)])
x1<-cbind(1, VMT.dat.sub[20,-c(1,7)])

#Plot the best model 
lo.1<-1.650694-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-2.19843-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<-1.650694+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<-2.19843+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[1:18,1],VMT.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), xlab="Year", ylab="VMT")
lines(dat[1:18,1],VMT.pred.in,  xlim=c(2000,2020), ylim=c(-1.5,3.10),col="blue")
lines(dat[19:20,1], VMT.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], VMT.pred.out, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=0.9523072, y1=2.349081, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=1.3517142, y1=3.045146, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=0.9523072, y1=0.9523072, col="red")
segments(x0=2017.6,x1=2018.6, y0=2.349081, y1=2.349081, col="red")
segments(x0=2018.6,x1=2019.4, y0=1.3517142, y1=1.3517142, col="red")
segments(x0=2018.6,x1=2019.6, y0=3.045146, y1=3.045146, col="red")



#Ridge - Training vs. Testing (with VMT(t-1))
VMT.ridge.in.1<-glmnet(VMT.dat.sub[2:18,-c(1)], VMT.dat.sub[2:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.sub[2:18,-c(1)]), VMT.dat.sub[2:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

VMT.pred.in.1<-coef(VMT.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), VMT.dat.sub[2:18,-c(1)])) #fitted values
VMT.pred.out.1<-coef(VMT.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1)])) 

resid.ridge.1<-VMT.dat.sub[2:18,1]-VMT.pred.in.1
DW.ridge.1<-sum((resid.ridge.1[2:17]-resid.ridge.1[1:16])^2)  /sum((resid.ridge.1[2:17])^2) #DW=2.466

MSE.in.VMT.1<- sum( (VMT.dat.sub[2:18,1] - VMT.pred.in.1)^2 )/17
MSE.out.VMT.1<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out.1)^2 )/2

MAE.in.VMT.1<- sum(abs(VMT.dat.sub[2:18,1] - VMT.pred.in.1))/17
MAE.out.VMT.1<- sum(abs(VMT.dat.sub[19:20,1] - VMT.pred.out.1))/2




#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])


#Linear - whole data
GHG.lm<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=GHG.dat.sub)
GHG.lm.1<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+GHG.1, data=GHG.dat.sub[2:20,])

GHG.test.dat<-GHG.dat.sub[19:20,]

#Linear - Training vs. Testing
GHG.lm.train<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=GHG.dat.sub[1:18,])
GHG.lm.train.1<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+GHG.1, data=GHG.dat.sub[2:18,])

resid.lm<-resid(GHG.lm.train)
DW<-sum((resid.lm[2:18]-resid.lm[1:17])^2)/sum((resid.lm[2:18])^2) #DW=2.314663
resid.lm.1<-resid(GHG.lm.train.1)
DW.1<-sum((resid.lm.1[2:16]-resid.lm.1[1:15])^2)/sum((resid.lm.1[2:16])^2) #DW=3.258663

MSE.in.GHG<- sum((GHG.dat.sub$TransportGHG[1:18]-GHG.lm.train$fitted.values)^2)/18
MSE.in.GHG.1<- sum((GHG.dat.sub$TransportGHG[2:18]-GHG.lm.train.1$fitted.values)^2)/17

MAE.in.GHG<- sum(abs(GHG.dat.sub$TransportGHG[1:18]-GHG.lm.train$fitted.values))/18
MAE.in.GHG.1<- sum(abs(GHG.dat.sub$TransportGHG[2:18]-GHG.lm.train.1$fitted.values))/17


GHG.pred.lm<-predict(GHG.lm.train, newdata=GHG.test.dat[,-1])
GHG.pred.lm.1<-predict(GHG.lm.train.1, newdata=GHG.dat.sub[19:20,-1])

MSE.out.GHG<- sum((GHG.test.dat[,1]-GHG.pred.lm)^2)/2
MSE.out.GHG.1<- sum((GHG.test.dat[,1]-GHG.pred.lm.1)^2)/2

MAE.out.GHG<- sum(abs(GHG.test.dat[,1]-GHG.pred.lm))/2
MAE.out.GHG.1<- sum(abs(GHG.test.dat[,1]-GHG.pred.lm.1))/2

#Plot the best model - Not chosen
plot(dat[1:18,1],GHG.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(-1.5,1.7), xlab="Year", ylab="GHG")
lines(dat[1:18,1],GHG.lm.train$fitted.values,  xlim=c(2000,2020), ylim=c(-1.5,1.7),col="blue")
lines(dat[19:20,1], GHG.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,1.7), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.lm, xlim=c(2000,2020), ylim=c(-1.5,1.7), type="p", pch=16, col="blue")


#LASSO - whole data
lambdas=10^seq(5,-5,by=-.1)
GHG.lasso<-glmnet(GHG.dat.sub[,-c(1,7)], GHG.dat.sub[,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[,-c(1,7)]), GHG.dat.sub[,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred<-coef(GHG.lasso, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,20), GHG.dat.sub[,-c(1,7)])) #fitted values

#LASSO - Training vs. Testing (without GHG(t-1))
GHG.lasso.in<-glmnet(GHG.dat.sub[1:18,-c(1,7)], GHG.dat.sub[1:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[1:18,-c(1,7)]), GHG.dat.sub[1:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.in<-coef(GHG.lasso.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), GHG.dat.sub[1:18,-c(1,7)])) #fitted values
GHG.pred.out<-coef(GHG.lasso.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1,7)])) 

resid.lasso<-GHG.dat.sub[1:18,1]-GHG.pred.in
DW.lasso<-sum((resid.lasso[2:18]-resid.lasso[1:17])^2)/sum((resid.lasso[2:18])^2) #DW=2.055043

MSE.in.GHG<- sum( (GHG.dat.sub[1:18,1] - GHG.pred.in)^2 )/18
MSE.out.GHG<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out)^2 )/2

MAE.in.GHG<- sum( abs(GHG.dat.sub[1:18,1] - GHG.pred.in) )/18
MAE.out.GHG<- sum( abs(GHG.dat.sub[19:20,1] - GHG.pred.out) )/2


#LASSO - Training vs. Testing (with GHG(t-1)) (BEST model)
GHG.lasso.in.1<-glmnet(GHG.dat.sub[2:18,-c(1)], GHG.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1)]), GHG.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.in.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)])) #fitted values
GHG.pred.out.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1)])) 


#GHG.dat.s$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
#summary(glmnetSE(data=GHG.dat.s[2:18,], 
                 cf.no.shrnkg = c("Unemployment.Rate.1","TransitPMT","GHG.1"), 
                alpha=1,r=500, method="none",seed = 1234, ncore = 1))

resid.lasso.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (GHG.dat.sub[1:18,1]-mean(GHG.dat.sub[1:18,1]))^2 );r2
DW.lasso.1<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=3.626901

MSE.in.GHG.1<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.1<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.1<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.1<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2



#Add a higher order to reduce DW
pairs(GHG.dat.sub)
#GHG.dat.sub$Unemployment.Rate.1.2<-GHG.dat.sub$Unemployment.Rate.1^2 #square unemployment
GHG.dat.sub$TotLaneMiles.2<-GHG.dat.sub$TotLaneMiles^2 #square unemployment

GHG.lasso.in.1.2<-glmnet(GHG.dat.sub[2:18,-c(1)], GHG.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1)]), GHG.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.in.1.2<-coef(GHG.lasso.in.1.2, s=optimal.lambda)[1:8] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)])) #fitted values
GHG.pred.out.1.2<-coef(GHG.lasso.in.1.2, s=optimal.lambda)[1:8] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1)])) 

resid.lasso.1.2<-GHG.dat.sub[2:18,1]-GHG.pred.in.1.2
DW.lasso.1.2<-sum((resid.lasso.1.2[2:17]-resid.lasso.1.2[1:16])^2)  /sum((resid.lasso.1.2[2:17])^2) #DW=2.687651

MSE.in.GHG.1.2<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1.2)^2 )/17
MSE.out.GHG.1.2<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1.2)^2 )/2

MAE.in.GHG.1.2<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1.2))/17
MAE.out.GHG.1.2<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1.2))/2


#Plot the best model 
X.X<-t(as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)]))) %*% as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)]) )
x0<-cbind(1, GHG.dat.sub[19,-c(1)])
x1<-cbind(1, GHG.dat.sub[20,-c(1)])

lo.1<-0.06323732-2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-0.1197243-2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<-0.06323732+2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<-0.1197243+2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[2:18,1],GHG.dat.sub[2:18,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), xlab="Year", ylab="GHG")
lines(dat[2:18,1],GHG.pred.in.1,  xlim=c(2000,2020), ylim=c(-1.5,3.10),col="blue")
lines(dat[19:20,1], GHG.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out.1, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=-0.7523015, y1=0.8787762, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=-0.8646819, y1=1.1041305, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=-0.7523015, y1=-0.7523015, col="red")
segments(x0=2017.6,x1=2018.6, y0=0.8787762, y1=0.8787762, col="red")
segments(x0=2018.6,x1=2019.4, y0=-0.8646819, y1=-0.8646819, col="red")
segments(x0=2018.6,x1=2019.6, y0=1.1041305, y1=1.1041305, col="red")


#Ridge - Training vs. Testing (without GHG(t-1))
GHG.ridge.in<-glmnet(GHG.dat.sub[1:18,-c(1,7)], GHG.dat.sub[1:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.sub[1:18,-c(1,7)]), GHG.dat.sub[1:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

GHG.pred.in<-coef(GHG.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), GHG.dat.sub[1:18,-c(1,7)])) #fitted values
GHG.pred.out<-coef(GHG.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1,7)])) 

resid.ridge<-GHG.dat.sub[1:18,1]-GHG.pred.in
DW.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)/sum((resid.ridge[2:18])^2) #DW=1.88

MSE.in.GHG<- sum( (GHG.dat.sub[1:18,1] - GHG.pred.in)^2 )/18
MSE.out.GHG<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out)^2 )/2

MAE.in.GHG<- sum( abs(GHG.dat.sub[1:18,1] - GHG.pred.in) )/18
MAE.out.GHG<- sum( abs(GHG.dat.sub[19:20,1] - GHG.pred.out) )/2


#Ridge - Training vs. Testing (with GHG(t-1))
GHG.ridge.in.1<-glmnet(GHG.dat.sub[2:18,-c(1)], GHG.dat.sub[2:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1)]), GHG.dat.sub[2:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

GHG.pred.in.1<-coef(GHG.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)])) #fitted values
GHG.pred.out.1<-coef(GHG.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1)])) 

resid.ridge.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1
DW.ridge.1<-sum((resid.ridge.1[2:17]-resid.ridge.1[1:16])^2)  /sum((resid.ridge.1[2:17])^2) #DW=3.62

MSE.in.GHG.1<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.1<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.1<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.1<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2





#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])

#Linear - whole data
TMS.lm<-lm(TransitModeShare~Unemployment.Rate.1+TotalArtToFwyMiles+TransitUPT+RegisteredBEVCarsMillions+populationTelework, data=TMS.dat.sub)
TMS.lm.1<-lm(TransitModeShare~TMS.1+Unemployment.Rate.1+TotalArtToFwyMiles+TransitUPT+RegisteredBEVCarsMillions+populationTelework, data=TMS.dat.sub[2:16,])

TMS.test.dat<-TMS.dat.sub[15:16,]

#Linear - Training vs. Testing
TMS.lm.train<-lm(TransitModeShare~Unemployment.Rate.1+TotalArtToFwyMiles+TransitUPT+RegisteredBEVCarsMillions+populationTelework, data=TMS.dat.sub[1:14,])
TMS.lm.train.1<-lm(TransitModeShare~TMS.1+Unemployment.Rate.1+TotalArtToFwyMiles+TransitUPT+RegisteredBEVCarsMillions+populationTelework, data=TMS.dat.sub[2:14,])

resid.lm<-resid(TMS.lm.train)
DW<-sum((resid.lm[2:14]-resid.lm[1:13])^2)/sum((resid.lm[2:14])^2) #DW=2.666778
resid.lm.1<-resid(TMS.lm.train.1)
DW.1<-sum((resid.lm.1[2:13]-resid.lm.1[1:12])^2)/sum((resid.lm.1[2:13])^2) #DW=3.124956

MSE.in.TMS<- sum((TMS.dat.sub$TransitModeShare[1:14]-TMS.lm.train$fitted.values)^2)/14
MSE.in.TMS.1<- sum((TMS.dat.sub$TransitModeShare[2:14]-TMS.lm.train.1$fitted.values)^2)/13

MAE.in.TMS<- sum(abs(TMS.dat.sub$TransitModeShare[1:14]-TMS.lm.train$fitted.values))/14
MAE.in.TMS.1<- sum(abs(TMS.dat.sub$TransitModeShare[2:14]-TMS.lm.train.1$fitted.values))/13


TMS.pred.lm<-predict(TMS.lm.train, newdata=TMS.test.dat[,-1])
TMS.pred.lm.1<-predict(TMS.lm.train.1, newdata=TMS.dat.sub[15:16,-1])

MSE.out.TMS<- sum((TMS.test.dat[,1]-TMS.pred.lm)^2)/2
MSE.out.TMS.1<- sum((TMS.test.dat[,1]-TMS.pred.lm.1)^2)/2

MAE.out.TMS<- sum(abs(TMS.test.dat[,1]-TMS.pred.lm))/2
MAE.out.TMS.1<- sum(abs(TMS.test.dat[,1]-TMS.pred.lm.1))/2


#LASSO - whole data
lambdas=10^seq(5,-5,by=-.1)
TMS.lasso<-glmnet(TMS.dat.sub[,-c(1,7)], TMS.dat.sub[,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[,-c(1,7)]), TMS.dat.sub[,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=16, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

TMS.pred<-coef(TMS.lasso, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,16), TMS.dat.sub[,-c(1,7)])) #fitted values

#LASSO - Training vs. Testing (without TMS(t-1))
TMS.lasso.in<-glmnet(TMS.dat.sub[1:14,-c(1,7)], TMS.dat.sub[1:14,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[1:14,-c(1,7)]), TMS.dat.sub[1:14,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=14, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

#coef(TMS.lasso.in, s=optimal.lambda)[2]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[6]<-0
TMS.pred.in<-coef(TMS.lasso.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,14), TMS.dat.sub[1:14,-c(1,7)])) #fitted values
TMS.pred.out<-coef(TMS.lasso.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1,7)])) 

resid.lasso<-TMS.dat.sub[1:14,1]-TMS.pred.in
DW.lasso<-sum((resid.lasso[2:14]-resid.lasso[1:13])^2)  /sum((resid.lasso[2:14])^2) #DW=2.071597

MSE.in.TMS<- sum( (TMS.dat.sub[1:14,1] - TMS.pred.in)^2 )/14
MSE.out.TMS<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out)^2 )/2

MAE.in.TMS<- sum( abs(TMS.dat.sub[1:14,1] - TMS.pred.in) )/14
MAE.out.TMS<- sum( abs(TMS.dat.sub[15:16,1] - TMS.pred.out) )/2


#LASSO - Training vs. Testing (with TMS(t-1)) (Best model)
TMS.lasso.in.1<-glmnet(TMS.dat.sub[2:14,-1], TMS.dat.sub[2:14,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[2:14,-1]), TMS.dat.sub[2:14,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=13, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

#coef(TMS.lasso.in, s=optimal.lambda)[2]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[3]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[5]<-0
#coef(TMS.lasso.in, s=optimal.lambda)[6]<-0
TMS.pred.in.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)])) #fitted values
TMS.pred.out.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1)])) 

resid.lasso.1<-TMS.dat.sub[2:14,1]-TMS.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (TMS.dat.sub[2:14,1]-mean(TMS.dat.sub[2:14,1]))^2 );r2
DW.lasso.1<-sum((resid.lasso.1[2:13]-resid.lasso.1[1:12])^2)/sum((resid.lasso.1[2:13])^2) #DW=2.211832

TMS.dat.s$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
summary(glmnetSE(data=TMS.dat.s[2:15,], cf.no.shrnkg = c("TransitUPT", "TMS.1"), alpha=1,r=300, method="none",seed = 1234, ncore = 1))




MSE.in.TMS.1<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.1<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.1<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.1<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2

#Plot the best model 
X.X<-t(as.matrix(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)]))) %*% as.matrix(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)]) )
x0<-cbind(1, TMS.dat.sub[15,-c(1)])
x1<-cbind(1, TMS.dat.sub[16,-c(1)])

lo.1<--0.09784331-2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<--0.2165824-2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<--0.09784331+2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<--0.2165824+2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

plot(dat[1:18,1],GHG.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), xlab="Year", ylab="GHG")
lines(dat[1:18,1],GHG.pred.in,  xlim=c(2000,2020), ylim=c(-1.5,3.10),col="blue")
lines(dat[19:20,1], GHG.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")

plot(dat[6:18,1],TMS.dat.sub[2:14,1], xlim=c(2000,2020), ylim=c(-1.5,1.7), xlab="Year", ylab="TMS")
lines(dat[6:18,1],TMS.pred.in.1,  xlim=c(2000,2020), ylim=c(-1.5,1.7),col="blue")
lines(dat[19:20,1], TMS.dat.sub[15:16,1], xlim=c(2000,2020), ylim=c(-1.5,1.7), type="p", pch=16)
lines(dat[19:20,1], TMS.pred.out.1, xlim=c(2000,2020), ylim=c(-1.5,1.7), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=-1.097843, y1=0.9021567, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=-1.316582, y1=0.8834176, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=-1.097843, y1=-1.097843, col="red")
segments(x0=2017.6,x1=2018.6, y0=0.9021567, y1=0.9021567, col="red")
segments(x0=2018.6,x1=2019.4, y0=-1.316582, y1=-1.316582, col="red")
segments(x0=2018.6,x1=2019.6, y0=0.8834176, y1=0.8834176, col="red")









#Ridge - Training vs. Testing (without TMS(t-1))
TMS.ridge.in<-glmnet(TMS.dat.sub[1:14,-c(1,7)], TMS.dat.sub[1:14,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(TMS.dat.sub[1:14,-c(1,7)]), TMS.dat.sub[1:14,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=14, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

TMS.pred.in<-coef(TMS.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,14), TMS.dat.sub[1:14,-c(1,7)])) #fitted values
TMS.pred.out<-coef(TMS.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1,7)])) 

resid.ridge<-TMS.dat.sub[1:14,1]-TMS.pred.in
DW.ridge<-sum((resid.ridge[2:14]-resid.ridge[1:13])^2)  /sum((resid.ridge[2:14])^2) #DW=2.12

MSE.in.TMS<- sum( (TMS.dat.sub[1:14,1] - TMS.pred.in)^2 )/14
MSE.out.TMS<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out)^2 )/2

MAE.in.TMS<- sum( abs(TMS.dat.sub[1:14,1] - TMS.pred.in) )/14
MAE.out.TMS<- sum( abs(TMS.dat.sub[15:16,1] - TMS.pred.out) )/2


#Ridge - Training vs. Testing (with VMT(t-1))
TMS.ridge.in.1<-glmnet(TMS.dat.sub[2:14,-1], TMS.dat.sub[2:14,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(TMS.dat.sub[2:14,-1]), TMS.dat.sub[2:14,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=13, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

TMS.pred.in.1<-coef(TMS.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)])) #fitted values
TMS.pred.out.1<-coef(TMS.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1)])) 

resid.ridge.1<-TMS.dat.sub[2:14,1]-TMS.pred.in.1
DW.ridge.1<-sum((resid.ridge.1[2:13]-resid.ridge.1[1:12])^2)  /sum((resid.ridge.1[2:13])^2) #DW=2.58

MSE.in.TMS.1<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.1<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.1<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.1<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2


