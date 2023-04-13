#This script creates the second best model for TMS, which was the Ridge Regression with Time Series

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)


#TMS Scaled###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
lambdas=10^seq(5,-5,by=-.1)


TMS.lasso.in.1<-glmnet(TMS.dat.sub[2:14,-1], TMS.dat.sub[2:14,1], alpha=0, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[2:14,-1]), TMS.dat.sub[2:14,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=13, lambda=lambdas)
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


#Scaled Model Summary - Appendix
model_summary <- data.frame(Model = (c('Ridge Regression, w/ Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.TMS.lasso.timeseries)),
                            MSE = (c(MSE.out.TMS.lasso.timeseries)),
                            RMSE = (c(RSME.out.TMS.lasso.timeseries)))
model_summary


#Scaled Coefficent summary - Appendix

Coefficent_summary <- data.frame(coefficents = c('TransitUPT', 'TMS.1'),
                                 Scaled_estimates = scaled_coef[,1][c(4,7)],
                                 Lower_ci = bootstrap_scaled$CI_lo[c(7, 11)],
                                 Upper_ci = bootstrap_scaled$CI_up[c(7, 11)],
                                 P_value = bootstrap_scaled$p.value[c(7, 11)]
)
Coefficent_summary

#Out of sample performance -- table 12
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_TMS = TMS.dat.sub[15:16,1],
                                 Predicted_TMS = TMS.pred.out.1[1:2],
                                 Prediction_interval_scaled_low = c(scaled_lo[1], scaled_lo[2]),
                                 Prediction_interval_scaled_high =  c(scaled_hi[1], scaled_hi[2])
)
prediction_summary 



