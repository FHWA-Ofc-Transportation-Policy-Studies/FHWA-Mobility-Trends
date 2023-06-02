
#This script creates the GHG 1.0 Model, which encompasses:
#GHG Scaled Appendix C: Table C-1, Table C-2, Figure C-1, Table C-3 of the Trends Indicator Report
#GHG Unscaled, Step 3: Table 3-5, Table 3-6, Figure 3-4, and Table 3-7 of the Trends Indicator Report


library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)

dat<-read.csv("C:/Users/zapate/Documents/task6_6_2/FHWA-Mobility-Trends/Year1//Modeling/Data/Data20221210.csv", header=TRUE)


#GHG scaled model - Appendix C###############################################################################################################
#read in columns, scale data, create time series lag
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
#additional_filter
GHG.dat.sub <- GHG.dat.sub[,c(1,2,4)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
lambdas=10^seq(5,-5,by=-.1)


#LASSO - Training vs. Testing (with GHG(t-1)) (BEST model)
GHG.lasso.in.1<-glmnet(GHG.dat.sub[2:18,-c(1)], GHG.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1)]), GHG.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.in.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:4] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)])) #fitted values
GHG.pred.out.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:4] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1)])) 


GHG.dat.s$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
scaled_bootstrap <- glmnetSE(data=GHG.dat.s[2:18,], 
                 cf.no.shrnkg = c("Unemployment.Rate.1","TransitPMT","GHG.1"), 
                 alpha=1,r=500, method="none",seed = 1234, ncore = 1)

summary(scaled_bootstrap)


resid.lasso.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (GHG.dat.sub[1:18,1]-mean(GHG.dat.sub[1:18,1]))^2 );r2
DW.lasso.LASSO.timeseries<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=3.626901

MSE.in.GHG.LASSO.timeseries<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.LASSO.timeseries<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.LASSO.timeseries<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.LASSO.timeseries<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2


RMSE.in.GHG<- sqrt(sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/18)
RMSE.out.GHG<- sqrt(sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2)

X.X<-t(as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)]))) %*% as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)]) )
x0<-cbind(1, GHG.dat.sub[19,-c(1)])
x1<-cbind(1, GHG.dat.sub[20,-c(1)])

lo.1<-0.06323732-2.201*sqrt(MSE.in.GHG.LASSO.timeseries*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-0.1197243-2.201*sqrt(MSE.in.GHG.LASSO.timeseries*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<-0.06323732+2.201*sqrt(MSE.in.GHG.LASSO.timeseries*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<-0.1197243+2.201*sqrt(MSE.in.GHG.LASSO.timeseries*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

scaled_coef <- coef(GHG.lasso.in.1, s=optimal.lambda)
scaled_coef

#Scaled Model Summary - Table C-1
model_summary <- data.frame(Model = (c('LASSO w/ Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.GHG.LASSO.timeseries)),
                            MSE = (c(MSE.out.GHG.LASSO.timeseries)),
                            RMSE = (c(RMSE.out.GHG)))
model_summary

#Scaled Coefficent summary - Table C-2

Coefficent_summary <- data.frame(coefficents = c('Unemployment Rate 1','Transit PMT', 'GHG.1'),
                                 Scaled_estimates = scaled_coef[,1][c(2, 3, 4 )],
                                 Lower_ci = scaled_bootstrap$CI_lo[c(1, 6, 11)],
                                 Upper_ci = scaled_bootstrap$CI_up[c(1, 6, 11)],
                                 P_value = scaled_bootstrap$p.value[c(1, 6, 11)]
)
Coefficent_summary


#Scaled, Out of sample performance -- Table C-3
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_GHG = GHG.dat.sub[19:20,1],
                                 Predicted_GHG = GHG.pred.out.1,
                                 Prediction_interval_low = c(lo[1], lo[2]),
                                 prediction_interval_high =  c(hi[1], hi[2]))
prediction_summary

#Figure C-1
plot(dat[2:18,1],GHG.dat.sub[2:18,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), xlab="Year", ylab="Scaled Units of GHG")
lines(dat[2:18,1],GHG.pred.in.1,  xlim=c(2000,2020), ylim=c(-1.5,3.10),col="blue")
lines(dat[19:20,1], GHG.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out.1, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=-0.7523015, y1=0.8787762, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=-0.8646819, y1=1.1041305, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=-0.7523015, y1=-0.7523015, col="red")
segments(x0=2017.6,x1=2018.6, y0=0.8787762, y1=0.8787762, col="red")
segments(x0=2018.6,x1=2019.4, y0=-0.8646819, y1=-0.8646819, col="red")
segments(x0=2018.6,x1=2019.6, y0=1.1041305, y1=1.1041305, col="red")



#GHG unscaled model - Step 3 GHG 1.0###############################################################################################################
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

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

unscaled_coef <-coef(GHG.lasso.in.1, s=optimal.lambda)
#Bootstrap CI's and p-values
#GHG.dat.s$GHG.1<-c(NA,GHG.dat.sub[1:19,1]) 
unscaled_bootstrap <- glmnetSE(data=GHG.dat.sub[2:18,], 
                               cf.no.shrnkg = c("Unemployment.Rate.1","TransitPMT","GHG.1"),
                               alpha=1,r=500, method="none",seed = 1234, ncore = 1)

summary(unscaled_bootstrap)

resid.lasso.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (GHG.dat.sub[1:18,1]-mean(GHG.dat.sub[1:18,1]))^2 );r2
DW.lasso.1<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=3.626901

MSE.in.GHG.1<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.1<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.1<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.1<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2

RMSE.in.GHG.1<- sqrt(sum( abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17)
RMSE.out.GHG.1<- sqrt(sum( abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2)


X.X<-t(as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1,3,5,6)]))) %*% as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1,3,5,6)]) )
x0<-cbind(1, GHG.dat.sub[19,-c(1,3,5,6)])
x1<-cbind(1, GHG.dat.sub[20,-c(1,3,5,6)])

lo.1<-1870.779-2.16*sqrt(MSE.in.GHG.1*(1+as.matrix(x0) %*% solve(X.X, tol=1.40178e-26) %*% t(as.matrix(x0))))
lo.2<-1887.031-2.16*sqrt(MSE.in.GHG.1*(1+as.matrix(x1) %*% solve(X.X, tol=1.40178e-26) %*% t(as.matrix(x1))))
hi.1<-1870.779+2.16*sqrt(MSE.in.GHG.1*(1+as.matrix(x0) %*% solve(X.X, tol=1.40178e-26) %*% t(as.matrix(x0))))
hi.2<-1887.031+2.16*sqrt(MSE.in.GHG.1*(1+as.matrix(x1) %*% solve(X.X, tol=1.40178e-26) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)



#Unscaled Model Summary - Table 3-5
model_summary <- data.frame(Model = (c('LASSO w/ Timeseries')),
                            R2 = (c(r2)),
                            MAE = (c(MAE.out.GHG.1)),
                            MSE = (c(MSE.out.GHG.1)),
                            RMSE = (c(RMSE.out.GHG.1)))
model_summary

#Unscaled Coefficent summary - Table 3-6

Coefficent_summary <- data.frame(coefficents = c('Unemployment Rate 1','Transit PMT', 'GHG.1'),
                                 Unscaled_estimates = unscaled_coef[,1][c(2,3,4)],
                                 Lower_ci = unscaled_bootstrap$CI_lo[c(1, 3, 6)],
                                 Upper_ci = unscaled_bootstrap$CI_up[c(1, 3, 6)],
                                 P_value = unscaled_bootstrap$p.value[c(1,3, 6)]
)
Coefficent_summary


#Out of sample performance -- Table 3-7
prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_GHG = GHG.dat.sub[18:19,1],
                                 Predicted_GHG = GHG.pred.out.1,
                                 Prediction_interval_low = c(lo[1], lo[2]),
                                 prediction_interval_high =  c(hi[1], hi[2]))
prediction_summary




#Figure 3-4

plot(dat[1:18,1],GHG.dat.sub[1:18,1], xlim=c(2000,2020), ylim=c(1747.5,2106.217), xlab="Year", ylab="GHG         (MMT CO2 eq.)")
lines(dat[2:18,1],GHG.pred.in.1,  xlim=c(2000,2020), ylim=c(1747.5,2106.217),col="blue")
lines(dat[19:20,1], GHG.dat.sub[19:20,1], xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16)
lines(dat[19:20,1], GHG.pred.out.1, xlim=c(2000,2020), ylim=c(-1.5,3.10), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=1814.732, y1=1926.826, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=1830.513, y1=1943.549, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=1814.732, y1=1814.732, col="red")
segments(x0=2017.6,x1=2018.6, y0=1926.826, y1=1926.826, col="red")
segments(x0=2018.6,x1=2019.4, y0=1830.513, y1=1830.513, col="red")
segments(x0=2018.6,x1=2019.6, y0=1943.549, y1=1943.549, col="red")





