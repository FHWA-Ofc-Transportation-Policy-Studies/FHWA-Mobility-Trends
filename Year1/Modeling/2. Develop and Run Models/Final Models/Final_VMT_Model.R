
#This script creates the VMT 1.0 Model, which encompasses:
#VMT Scaled Appendix B: Table 26, Table 27, Figure 21, Table 28 of the Trends Indicator Report
#VMT Unscaled, Step 3: Table 4, Table 5, Figure 8, and Table 6 of the Trends Indicator Report

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

#VMT Scaled - Appendix B##############################################################################################################
#filter to correct data and scale. 
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,1])
lambdas=10^seq(5,-5,by=-.1)


#Ridge - Training vs. Testing (without VMT(t-1)) - final model
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

RMSE.in.VMT<- sqrt(sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18)
RMSE.out.VMT<- sqrt(sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2)


scaled_coefficents <- coef(VMT.ridge.in, s=optimal.lambda)

bootstrap_scaled <- glmnetSE(data=VMT.dat.s[2:18,], cf.no.shrnkg = c("Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), alpha=0,r=1500, method="none",seed = 1234, ncore = 1)
summary(bootstrap_scaled)

#prediction interval calculation
X.X<-t(as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]))) %*% as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]) )
x0<-cbind(1, VMT.dat.sub[19,-c(1,7)])
x1<-cbind(1, VMT.dat.sub[20,-c(1,7)])

#Plot the best model in scaled form (not in report)
#prediction interval caclulation
lo.1<-1.650694-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-2.19843-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<-1.650694+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<-2.19843+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)


# Scaled model summary (Appendix B Table 26)
model_summary <- data.frame(Model = (c('VMT Ridge Regression, No Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.VMT)),
                            MSE = (c(MSE.out.VMT)),
                            RMSE = (c(RMSE.out.VMT)))
model_summary

#scaled model coefficents (Appendix B Table 27) 
Coefficent_summary <- data.frame(coefficents = c('Unemployment Rate 1', 'Total Lane Miles', 'Transit Passenger Miles Traveled',
                                                 'Ev Priv. and Pub. Stations', 'Pop. Telework'),
                                 Scaled_estimates = scaled_coefficents[,1][c(2,3,4,5,6)],
                                 Lower_ci = bootstrap_scaled$CI_lo[c(1, 2, 6, 9, 10)],
                                 Upper_ci = bootstrap_scaled$CI_up[c(1, 2, 6, 9, 10)],
                                 P_value = bootstrap_scaled$p.value[c(1, 2, 6, 9, 10)]
)
Coefficent_summary

#Scaled Out of sample performance (appendix B Table 28)
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_VMT = VMT.dat.sub[19:20,1],
                                 Predicted_VMT = VMT.pred.out,
                                 Prediction_interval_low = c(lo[1], lo[2]),
                                 prediction_interval_high =  c(hi[1], hi[2]))
prediction_summary

#plot of scaled model (Appendix B Figure 21)
plot(dat[1:18,1],VMT.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), xlab="Year", ylab="VMT in Scaled Units")
lines(dat[1:18,1],VMT.pred.in,  xlim=c(2000,2020), ylim=c(-1.5,3.10),col="blue")
lines(dat[19:20,1], VMT.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], VMT.pred.out, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=0.9523072, y1=2.349081, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=1.3517142, y1=3.045146, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=0.9523072, y1=0.9523072, col="red")
segments(x0=2017.6,x1=2018.6, y0=2.349081, y1=2.349081, col="red")
segments(x0=2018.6,x1=2019.4, y0=1.3517142, y1=1.3517142, col="red")
segments(x0=2018.6,x1=2019.6, y0=3.045146, y1=3.045146, col="red")



#VMT Unscaled Step 3 Model VMT 1.0##############################################################################################################
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

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

RMSE.in.VMT<- sqrt(sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18)
RMSE.out.VMT<- sqrt(sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2)

unscaled_coefficents <- coef(VMT.ridge.in, s=optimal.lambda)

#Bootstrap
bootstrap_unscaled <- glmnetSE(data=VMT.dat.s[2:18,], cf.no.shrnkg = c("Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), alpha=0,r=1500, method="none",seed = 1234, ncore = 1)
summary(bootstrap_unscaled)

#Plot the best model - Figure 8
X.X<-t(as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]))) %*% as.matrix(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)]) )
x0<-cbind(1, VMT.dat.sub[19,-c(1,7)])
x1<-cbind(1, VMT.dat.sub[20,-c(1,7)])

lo.1<-3.259926e+12-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
lo.2<-3.327951e+12-2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
hi.1<-3.259926e+12+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
hi.2<-3.327951e+12+2.16*sqrt(MSE.in.VMT*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)


#########
#Model Summary - Step 3, Table 4
model_summary <- data.frame(Model = (c('Ridge Regression, No Timeseries')),
                           R2 = (c(r2)),
                           MAE = (c(MAE.out.VMT)),
                           MSE = (c(MSE.out.VMT)),
                           RMSE = (c(RMSE.out.VMT)))
model_summary

#Coefficent summary - Step 3, Table 5

Coefficent_summary <- data.frame(coefficents = c('Unemployment Rate 1', 'Total Lane Miles', 'Transit Passenger Miles Traveled',
                                                 'Ev Priv. and Pub. Stations', 'Pop. Telework'),
                                 Unscaled_estimates = unscaled_coefficents[,1][c(2,3,4,5,6)],
                                 Lower_ci = bootstrap_unscaled$CI_lo[c(1, 2, 6, 9, 10)],
                                 Upper_ci = bootstrap_unscaled$CI_up[c(1, 2, 6, 9, 10)],
                                 P_value = bootstrap_unscaled$p.value[c(1, 2, 6, 9, 10)]
                                 )
Coefficent_summary

#Out of sample performance -- Step 3, table 6
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_VMT = VMT.dat.sub[19:20,1],
                                 Predicted_VMT = VMT.pred.out,
                                 Prediction_interval_low = c(lo[1], lo[2]),
                                 prediction_interval_high =  c(hi[1], hi[2]))
prediction_summary

#Plot of VMT model 1.0, Step 3, Figure 8
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
