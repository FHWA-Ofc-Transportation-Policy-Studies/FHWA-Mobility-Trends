#This script creates the second best model for VMT, which was the LASSO Regression without Time Series

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

#VMT Scaled ##############################################################################################################
#filter to correct data and scale. 
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,1])
lambdas=10^seq(5,-5,by=-.1)


#LASSO - Training vs. Testing (without VMT(t-1)) - final model
VMT.LASSO.in<-glmnet(VMT.dat.sub[1:18,-c(1,7)], VMT.dat.sub[1:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[1:18,-c(1,7)]), VMT.dat.sub[1:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

VMT.pred.in<-coef(VMT.LASSO.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), VMT.dat.sub[1:18,-c(1,7)])) #fitted values
VMT.pred.out<-coef(VMT.LASSO.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),VMT.dat.sub[19:20,-c(1,7)])) 

resid.ridge<-VMT.dat.sub[1:18,1]-VMT.pred.in; r2<-1-sum(resid.ridge^2)/sum(  (VMT.dat.sub[1:18,1]-mean(VMT.dat.sub[1:18,1]))^2 );r2
DW.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)  /sum((resid.ridge[2:18])^2) #DW=1.76

MSE.in.VMT<- sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18
MSE.out.VMT<- sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2

MAE.in.VMT<- sum( abs(VMT.dat.sub[1:18,1] - VMT.pred.in) )/18
MAE.out.VMT<- sum( abs(VMT.dat.sub[19:20,1] - VMT.pred.out) )/2

RMSE.in.VMT<- sqrt(sum( (VMT.dat.sub[1:18,1] - VMT.pred.in)^2 )/18)
RMSE.out.VMT<- sqrt(sum( (VMT.dat.sub[19:20,1] - VMT.pred.out)^2 )/2)


scaled_coefficents <- coef(VMT.LASSO.in, s=optimal.lambda)

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

# related to Table A-1
model_summary <- data.frame(Model = (c('VMT LASSO Regression, No Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.VMT)),
                            MSE = (c(MSE.out.VMT)),
                            RMSE = (c(RMSE.out.VMT)))
model_summary

Coefficent_summary <- data.frame(coefficents = c('Unemployment Rate 1', 'Total Lane Miles', 'Transit Passenger Miles Traveled',
                                                 'Ev Priv. and Pub. Stations', 'Pop. Telework'),
                                 Scaled_estimates = scaled_coefficents[,1][c(2,3,4,5,6)],
                                 Lower_ci = bootstrap_scaled$CI_lo[c(1, 2, 6, 9, 10)],
                                 Upper_ci = bootstrap_scaled$CI_up[c(1, 2, 6, 9, 10)],
                                 P_value = bootstrap_scaled$p.value[c(1, 2, 6, 9, 10)]
)
Coefficent_summary

prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_VMT = VMT.dat.sub[19:20,1],
                                 Predicted_VMT = VMT.pred.out,
                                 Prediction_interval_low = c(lo[1], lo[2]),
                                 prediction_interval_high =  c(hi[1], hi[2]))
prediction_summary
