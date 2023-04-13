#scaled models 

#relates to the figure 7 of the report (time series vs non-time series)

#The below analysis compares Linear Regression, Lasso, and Ridge Regression for VMT
#Each model will be evaluated with and without time-series considerations (ie, time series models will include the 
#performance metric lagged (t-1) within the model. )

#The final results for each section will relate to figure 7 within the Trends Indicator Memorandum.

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

#VMT ##############################################################################################################
#filter to correct data and scale. 
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,1])

#Linear - whole data
VMT.lm<-lm(VMT~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub)
VMT.lm.Timeseries<-lm(VMT~VMT.1+Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub[2:20,])

VMT.test.dat<-VMT.dat.sub[19:20,]

#Linear - Training vs. Testing
VMT.lm.train<-lm(VMT~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub[1:18,])
VMT.lm.train.Timeseries<-lm(VMT~VMT.1+Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=VMT.dat.sub[2:18,])

resid.lm<-resid(VMT.lm.train); resid.lm.1<-resid(VMT.lm.train.Timeseries)
DW.VMT.LM<-sum((resid.lm[2:18]-resid.lm[1:17])^2)/sum((resid.lm[2:18])^2) #DW=2.37
resid.lm.Timeseries<-resid(VMT.lm.train.Timeseries)
DW.VMT.LM.Timeseries<-sum((resid.lm.Timeseries[2:16]-resid.lm.Timeseries[1:15])^2)/sum((resid.lm.Timeseries[2:16])^2) #DW=2.43

MSE.in.VMT.LM<- sum((VMT.dat.sub$VMT[1:18]-VMT.lm.train$fitted.values)^2)/18
MSE.in.VMT.LM.Timeseries<- sum((VMT.dat.sub$VMT[2:18]-VMT.lm.train.Timeseries$fitted.values)^2)/17

MAE.in.VMT.LM<- sum(abs(VMT.dat.sub$VMT[1:18]-VMT.lm.train$fitted.values))/18
MAE.in.VMT.LM.Timeseries<- sum(abs(VMT.dat.sub$VMT[2:18]-VMT.lm.train.Timeseries$fitted.values))/17


VMT.pred.lm<-predict(VMT.lm.train, newdata=VMT.test.dat[,-1])
VMT.pred.lm.Timeseries<-predict(VMT.lm.train.Timeseries, newdata=VMT.test.dat[,-1])

MSE.out.VMT.LM<- sum((VMT.test.dat[,1]-VMT.pred.lm)^2)/2
MSE.out.VMT.LM.Timeseries<- sum((VMT.test.dat[,1]-VMT.pred.lm.Timeseries)^2)/2

MAE.out.VMT.LM<- sum(abs(VMT.test.dat[,1]-VMT.pred.lm))/2
MAE.out.VMT.LM.Timeseries<- sum(abs(VMT.test.dat[,1]-VMT.pred.lm.Timeseries))/2



#LASSO - whole data, no Timeseries
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
DW.VMT.lasso.<-sum((resid.lasso[2:18]-resid.lasso[1:17])^2)  /sum((resid.lasso[2:18])^2) #DW=1.85

MSE.in.VMT.Lasso<- sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18
MSE.out.VMT.Lasso<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2

MAE.in.VMT.Lasso<- sum( abs(VMT.dat.sub[1:18,1] - VMT.pred.in) )/18
MAE.out.VMT.Lasso<- sum( abs(VMT.dat.sub[19:20,1] - VMT.pred.out) )/2



#LASSO - Training vs. Testing (with VMT(t-1))
VMT.lasso.in.1<-glmnet(VMT.dat.sub[2:18,-c(1)], VMT.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[2:18,-c(1)]), VMT.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

VMT.pred.in.1<-coef(VMT.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), VMT.dat.sub[2:18,-c(1)])) #fitted values
VMT.pred.out.1<-coef(VMT.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1)])) 

resid.lasso.1<-VMT.dat.sub[2:18,1]-VMT.pred.in.1
DW.lasso.Timeseries<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=2.65

MSE.in.VMT.LASSO.Timeseries<- sum( (VMT.dat.sub[2:18,1] - VMT.pred.in.1)^2 )/17
MSE.out.VMT.LASSO.Timeseries<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out.1)^2 )/2

MAE.in.VMT.LASSO.Timeseries<- sum(abs(VMT.dat.sub[2:18,1] - VMT.pred.in.1))/17
MAE.out.VMT.LASSO.Timeseries<- sum(abs(VMT.dat.sub[19:20,1] - VMT.pred.out.1))/2


#Ridge - Training vs. Testing (without VMT(t-1)) - final model
VMT.ridge.in<-glmnet(VMT.dat.sub[1:18,-c(1,7)], VMT.dat.sub[1:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.sub[1:18,-c(1,7)]), VMT.dat.sub[1:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

VMT.pred.in<-coef(VMT.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)])) #fitted values
VMT.pred.out<-coef(VMT.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1,7)])) 

resid.ridge<-VMT.dat.sub[1:18,1]-VMT.pred.in; r2<-1-sum(resid.ridge^2)/sum(  (VMT.dat.sub[1:18,1]-mean(VMT.dat.sub[1:18,1]))^2 );r2
DW.VMT.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)  /sum((resid.ridge[2:18])^2) #DW=1.76

MSE.in.VMT.Ridge<- sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18
MSE.out.VMT.Ridge<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2

MAE.in.VMT.Ridge<- sum( abs(VMT.dat.sub[1:18,1] - VMT.pred.in) )/18
MAE.out.VMT.Ridge<- sum( abs(VMT.dat.sub[19:20,1] - VMT.pred.out) )/2


#summary(glmnetSE(data=VMT.dat.s[2:18,], cf.no.shrnkg = c("Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), alpha=0,r=1500, method="none",seed = 1234, ncore = 1))

#prediction interval calculation
X.X<-t(as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]))) %*% as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]) )
x0<-cbind(1, VMT.dat.sub[19,-c(1,7)])
x1<-cbind(1, VMT.dat.sub[20,-c(1,7)])

#Plot the best model 
#prediction interval caclulation
#lo.1<-1.650694-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
# lo.2<-2.19843-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
# hi.1<-1.650694+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
# hi.2<-2.19843+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
# lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)
# 
# plot(dat[1:18,1],VMT.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), xlab="Year", ylab="VMT")
# lines(dat[1:18,1],VMT.pred.in,  xlim=c(2000,2020), ylim=c(-1.5,3.10),col="blue")
# lines(dat[19:20,1], VMT.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
# lines(dat[19:20,1], VMT.pred.out, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
# segments(x0=2018,x1=2018, y0=0.9523072, y1=2.349081, lty = "dotted", col="red")
# segments(x0=2019,x1=2019, y0=1.3517142, y1=3.045146, lty = "dotted", col="red")
# segments(x0=2017.6,x1=2018.4, y0=0.9523072, y1=0.9523072, col="red")
# segments(x0=2017.6,x1=2018.6, y0=2.349081, y1=2.349081, col="red")
# segments(x0=2018.6,x1=2019.4, y0=1.3517142, y1=1.3517142, col="red")
# segments(x0=2018.6,x1=2019.6, y0=3.045146, y1=3.045146, col="red")



#Ridge - Training vs. Testing (with VMT(t-1))
VMT.ridge.in.1<-glmnet(VMT.dat.sub[2:18,-c(1)], VMT.dat.sub[2:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(VMT.dat.sub[2:18,-c(1)]), VMT.dat.sub[2:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

VMT.pred.in.1<-coef(VMT.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), VMT.dat.sub[2:18,-c(1)])) #fitted values
VMT.pred.out.1<-coef(VMT.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1)])) 

resid.ridge.1<-VMT.dat.sub[2:18,1]-VMT.pred.in.1
DW.Ridge.Timeseries<-sum((resid.ridge.1[2:17]-resid.ridge.1[1:16])^2)  /sum((resid.ridge.1[2:17])^2) #DW=2.466

MSE.in.VMT.Ridge.Timeseries<- sum( (VMT.dat.sub[2:18,1] - VMT.pred.in.1)^2 )/17
MSE.out.VMT.Ridge.Timeseries<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out.1)^2 )/2

MAE.in.VMT.Ridge.Timeseries<- sum(abs(VMT.dat.sub[2:18,1] - VMT.pred.in.1))/17
MAE.out.VMT.Ridge.Timeseries<- sum(abs(VMT.dat.sub[19:20,1] - VMT.pred.out.1))/2


VMT.df <- data.frame(Models = (c('VMT - Linear Regression without VMT(t-1)', 'VMT - Linear Regression with VMT(t-1)', 
                                    'VMT - Ridge Regression without VMT(t-1)', 'VMT - Ridge Regression with VMT(t-1)',
                                    'VMT - LASSO without VMT(t-1)', 'VMT - LASSO with VMT(t-1)')),
                      MAE.IN = (c(MAE.in.VMT.LM, MAE.in.VMT.LASSO.Timeseries,
                                  MAE.in.VMT.Ridge, MAE.in.VMT.Ridge.Timeseries,
                                  MAE.in.VMT.Lasso, MAE.in.VMT.LASSO.Timeseries)),
                        MAE.OUT = (c(MAE.out.VMT.LM, MAE.out.VMT.LM.Timeseries,
                                     MAE.out.VMT.Ridge, MAE.out.VMT.Ridge.Timeseries,
                                     MAE.out.VMT.Lasso, MAE.out.VMT.LASSO.Timeseries)),
                        DurbinWatson = (c(DW.VMT.LM, DW.VMT.LM.Timeseries,
                                          DW.VMT.ridge,DW.Ridge.Timeseries,
                                          DW.VMT.lasso., DW.lasso.Timeseries
                                          )))

#output of VMT models as it relates to figure 7
VMT.df

