#This script creates the second best model for GHG, which was the Ridge Regression with Time Series

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)

dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)


#GHG scaled model###############################################################################################################

GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
lambdas=10^seq(5,-5,by=-.1)

#Ridge - Training vs. Testing (with GHG(t-1)) - Ridge,appendix table 20
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

RMSE.in.GHG<- sqrt(sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/18)
RMSE.out.GHG<- sqrt(sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2)

resid.ridge.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1; r2<-1-sum(resid.ridge.1^2)/sum(  (GHG.dat.sub[1:18,1]-mean(GHG.dat.sub[1:18,1]))^2 );r2

GHG.dat.s$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
scaled_bootstrap <- glmnetSE(data=GHG.dat.s[2:18,], 
                             cf.no.shrnkg = c("Unemployment.Rate.1","TransitPMT","GHG.1"), 
                             alpha=1,r=500, method="none",seed = 1234, ncore = 1)

summary(scaled_bootstrap)

X.X<-t(as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)]))) %*% as.matrix(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)]) )
x0<-cbind(1, GHG.dat.sub[19,-c(1)])
x1<-cbind(1, GHG.dat.sub[20,-c(1)])

lo.1<-0.06323732-2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
lo.2<-0.1197243-2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
hi.1<-0.06323732+2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x0) %*% solve(X.X) %*% t(as.matrix(x0))))
hi.2<-0.1197243+2.201*sqrt(MSE.in.GHG.1*(1+as.matrix(x1) %*% solve(X.X) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

scaled_coef <- coef(GHG.ridge.in.1, s=optimal.lambda)
scaled_coef

model_summary <- data.frame(Model = (c('Ridge w/ Timeseries')),
                            R2 = (c(r2)),
                            MAE.out = (c(MAE.out.GHG.1)),
                            MSE.out = (c(MSE.out.GHG.1)),
                            RMSE = (c(RMSE.out.GHG)))
model_summary

Coefficent_summary <- data.frame(coefficents = c('Unemployment Rate 1','Transit PMT', 'GHG.1'),
                                 Scaled_estimates = scaled_coef[,1][c(2, 3, 4 )],
                                 Lower_ci = scaled_bootstrap$CI_lo[c(1, 6, 11)],
                                 Upper_ci = scaled_bootstrap$CI_up[c(1, 6, 11)],
                                 P_value = scaled_bootstrap$p.value[c(1, 6, 11)]
)
Coefficent_summary

prediction_summary <- data.frame(Year = c(2018, 2019),
                                 Actual_GHG = GHG.dat.sub[19:20,1],
                                 Predicted_GHG = GHG.pred.out.1,
                                 Prediction_interval_low = c(lo[1], lo[2]),
                                 prediction_interval_high =  c(hi[1], hi[2]))
prediction_summary

