
#This script creates the TMS 1.0 Model, which encompasses:
#TMS Scaled Appendix D: Table D-1, Table D-2, Figure D-1, Table D-3 of the Trends Indicator Report
#TMS Unscaled, Step 3: Table 3-8, Table 3-9, Figure 3-5, and Table 3-10 of the Trends Indicator Report

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)


#TMS Scaled - Appendix D###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
lambdas=10^seq(5,-5,by=-.1)


TMS.lasso.in.1<-glmnet(TMS.dat.sub[2:14,-1], TMS.dat.sub[2:14,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[2:14,-1]), TMS.dat.sub[2:14,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=13, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

TMS.pred.in.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)])) #fitted values
TMS.pred.out.1<-coef(TMS.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1)])) 

scaled_coef <- coef(TMS.lasso.in.1, s=optimal.lambda)

resid.lasso.1<-TMS.dat.sub[2:14,1]-TMS.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (TMS.dat.sub[2:14,1]-mean(TMS.dat.sub[2:14,1]))^2 );r2
DW.lasso.timeseries<-sum((resid.lasso.1[2:13]-resid.lasso.1[1:12])^2)/sum((resid.lasso.1[2:13])^2) #DW=2.211832

TMS.dat.s$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
bootstrap_scaled <- glmnetSE(data=TMS.dat.s[2:15,], cf.no.shrnkg = c("TransitUPT", "TMS.1"), alpha=1,r=300, method="none",seed = 1234, ncore = 1)
summary(bootstrap_scaled)


MSE.in.TMS.lasso.timeseries<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.lasso.timeseries<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.lasso.timeseries<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.lasso.timeseries<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2

RSME.in.TMS.lasso.timeseries<- sqrt(sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13)
RSME.out.TMS.lasso.timeseries<- sqrt(sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2)

#Plot the best model 
X.X<-t(as.matrix(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)]))) %*% as.matrix(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)]) )
x0<-cbind(1, TMS.dat.sub[15,-c(1)])
x1<-cbind(1, TMS.dat.sub[16,-c(1)])

lo.1<--0.09784331-2.201*sqrt(MSE.in.TMS.lasso.timeseries*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<--0.2165824-2.201*sqrt(MSE.in.TMS.lasso.timeseries*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<--0.09784331+2.201*sqrt(MSE.in.TMS.lasso.timeseries*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<--0.2165824+2.201*sqrt(MSE.in.TMS.lasso.timeseries*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
scaled_lo<-c(lo.1,lo.2); scaled_hi<-c(hi.1,hi.2)


#Scaled Model Summary - Table D-1
model_summary <- data.frame(Model = (c('LASSO Regression, w/ Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.TMS.lasso.timeseries)),
                            MSE = (c(MSE.out.TMS.lasso.timeseries)),
                            RMSE = (c(RSME.out.TMS.lasso.timeseries)))
model_summary


#Scaled Coefficent summary - Table D-2

Coefficent_summary <- data.frame(coefficents = c('TransitUPT', 'TMS.1'),
                                 Scaled_estimates = scaled_coef[,1][c(4,7)],
                                 Lower_ci = bootstrap_scaled$CI_lo[c(7, 11)],
                                 Upper_ci = bootstrap_scaled$CI_up[c(7, 11)],
                                 P_value = bootstrap_scaled$p.value[c(7, 11)]
)
Coefficent_summary

#Out of sample performance --Table D-3
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_TMS = TMS.dat.sub[15:16,1],
                                 Predicted_TMS = TMS.pred.out.1[1:2],
                                 Prediction_interval_scaled_low = c(scaled_lo[1], scaled_lo[2]),
                                 Prediction_interval_scaled_high =  c(scaled_hi[1], scaled_hi[2])
)
prediction_summary 


#Figure D-1
plot(dat[6:18,1],TMS.dat.sub[2:14,1], xlim=c(2000,2020), ylim=c(-5,5), xlab="Year", ylab="TMS in Scaled Units")
lines(dat[6:18,1],TMS.pred.in.1,  xlim=c(2000,2020), ylim=c(-5,5),col="blue")
lines(dat[19:20,1], TMS.dat.sub[15:16,1], xlim=c(2000,2020), ylim=c(-1.5,1.7), type="p", pch=16)
lines(dat[19:20,1], TMS.pred.out.1, xlim=c(2000,2020), ylim=c(-1.5,1.7), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=scaled_lo[1], y1=scaled_hi[1], lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=scaled_lo[2], y1=scaled_hi[2], lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=scaled_lo[1], y1=scaled_lo[1], col="red")
segments(x0=2017.6,x1=2018.6, y0=scaled_hi[1], y1=scaled_hi[1], col="red")
segments(x0=2018.6,x1=2019.4, y0=scaled_lo[2], y1=scaled_lo[2], col="red")
segments(x0=2018.6,x1=2019.6, y0=scaled_hi[2], y1=scaled_hi[2], col="red")



#TMS unscaled - Step 3 TMS Model 1.0###############################################################################################################
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

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

unscaled_coef <- coef(TMS.lasso.in.1, s=optimal.lambda)

#Bootstrap CI's and p-values
TMS.dat.s$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
bootstrap_unscaled <- glmnetSE(data=TMS.dat.s[2:15,], cf.no.shrnkg = c("TransitUPT", "TMS.1"), alpha=1,r=500, method="none",seed = 1234, ncore = 1)
summary(bootstrap_unscaled)

resid.lasso.1<-TMS.dat.sub[2:14,1]-TMS.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (TMS.dat.sub[2:14,1]-mean(TMS.dat.sub[2:14,1]))^2 );r2
DW.lasso.timeseries<-sum((resid.lasso.1[2:13]-resid.lasso.1[1:12])^2)/sum((resid.lasso.1[2:13])^2) #DW=2.211832


MSE.in.TMS.1<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.1<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.1<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.1<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2

RMSE.in.TMS.1<- sqrt(sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13)
RMSE.out.TMS.1<- sqrt(sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2)


X.X<-t(as.matrix(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1,2,3,5,6)]))) %*% as.matrix(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1,2,3,5,6)]) ) #changed last sub from GHG.dat.sub
x0<-cbind(1, TMS.dat.sub[15,-c(1,2,3,5,6)])
x1<-cbind(1, TMS.dat.sub[16,-c(1,2,3,5,6)])

lo.1<-0.04952227-2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x0) %*% solve(X.X, tol=3.39389e-27) %*% t(as.matrix(x0))))
lo.2<-0.04931173-2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x1) %*% solve(X.X, tol=3.39389e-27) %*% t(as.matrix(x1))))
hi.1<-0.04952227+2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x0) %*% solve(X.X, tol=3.39389e-27) %*% t(as.matrix(x0))))
hi.2<-0.04931173+2.201*sqrt(MSE.in.TMS.1*(1+as.matrix(x1) %*% solve(X.X, tol=3.39389e-27) %*% t(as.matrix(x1))))
unscaled_lo<-c(lo.1,lo.2); unscaled_hi<-c(hi.1,hi.2)



#Model Summary - Table 3-8
model_summary <- data.frame(Model = (c('LASSO Regression, w/ Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.TMS.1)),
                            MSE = (c(MSE.out.TMS.1)),
                            RMSE = (c(RMSE.out.TMS.1)))
model_summary


#Coefficent summary - Table 3-9

Coefficent_summary <- data.frame(coefficents = c('TransitUPT', 'TMS.1'),
                                 Unscaled_estimates = unscaled_coef[,1][c(4,7)],
                                 Lower_ci = bootstrap_unscaled$CI_lo[c(7, 11)],
                                 Upper_ci = bootstrap_unscaled$CI_up[c(7, 11)],
                                 P_value = bootstrap_unscaled$p.value[c(7, 11)]
)
Coefficent_summary

#Out of sample performance - Table 3-10
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_TMS = TMS.dat.sub[15:16,1],
                                 Predicted_TMS = TMS.pred.out.1[1:2],
                                 Prediction_interval_unscaled_low = c(unscaled_lo[1], unscaled_lo[2]),
                                 Prediction_interval_unscaled_high =  c(unscaled_hi[1], unscaled_hi[2]))

prediction_summary #incorrect TMS  prediction intervalss


#plot related to Figure 3-5
plot(dat[6:18,1],TMS.dat.sub[2:14,1], xlim=c(2000,2020), ylim=c(0.043,0.055), xlab="Year", ylab="Transit Share Percentage")
plot(dat[6:18,1],TMS.dat.sub[2:14,1], xlim=c(2000,2020), ylim=c(0.043,0.055), xlab="Year", ylab="Transit Share Percentage")
lines(dat[6:18,1],TMS.pred.in.1,  xlim=c(2000,2020), ylim=c(0.043,0.055),col="blue")
lines(dat[19:20,1], TMS.dat.sub[15:16,1], xlim=c(2000,2020), ylim=c(0.043,0.055), type="p", pch=16)
lines(dat[19:20,1], TMS.pred.out.1, xlim=c(2000,2020), ylim=c(0.043,0.055), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=unscaled_lo[1], y1=unscaled_hi[1], lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=unscaled_lo[2], y1=unscaled_hi[2], lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=unscaled_lo[1], y1=unscaled_lo[1], col="red")
segments(x0=2017.6,x1=2018.6, y0=unscaled_hi[1], y1=unscaled_hi[1], col="red")
segments(x0=2018.6,x1=2019.4, y0=unscaled_lo[2], y1=unscaled_lo[2], col="red")
segments(x0=2018.6,x1=2019.6, y0=unscaled_hi[2], y1=unscaled_hi[2], col="red")




