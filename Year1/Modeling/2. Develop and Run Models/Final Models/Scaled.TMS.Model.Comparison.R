#scaled models 

#The below analysis compares Linear Regression, Lasso, and Ridge Regression for TMS
#Each model will be evaluated with and without time-series considerations (ie, time series models will include the 
#performance metric lagged (t-1) within the model. )

#The final results for each section will relate to figure 7 within the Trends Indicator Memorandum.

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)


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
DW.LM<-sum((resid.lm[2:14]-resid.lm[1:13])^2)/sum((resid.lm[2:14])^2) #DW=2.666778
resid.lm.1<-resid(TMS.lm.train.1)
DW.LM.Timeseries<-sum((resid.lm.1[2:13]-resid.lm.1[1:12])^2)/sum((resid.lm.1[2:13])^2) #DW=3.124956

MSE.in.TMS<- sum((TMS.dat.sub$TransitModeShare[1:14]-TMS.lm.train$fitted.values)^2)/14
MSE.in.TMS.1<- sum((TMS.dat.sub$TransitModeShare[2:14]-TMS.lm.train.1$fitted.values)^2)/13

MAE.in.TMS<- sum(abs(TMS.dat.sub$TransitModeShare[1:14]-TMS.lm.train$fitted.values))/14
MAE.in.TMS.1<- sum(abs(TMS.dat.sub$TransitModeShare[2:14]-TMS.lm.train.1$fitted.values))/13


TMS.pred.lm<-predict(TMS.lm.train, newdata=TMS.test.dat[,-1])
TMS.pred.lm.1<-predict(TMS.lm.train.1, newdata=TMS.dat.sub[15:16,-1])

MSE.out.TMS.LM<- sum((TMS.test.dat[,1]-TMS.pred.lm)^2)/2
MSE.out.TMS.LM.Timeseries<- sum((TMS.test.dat[,1]-TMS.pred.lm.1)^2)/2

MAE.out.TMS.LM<- sum(abs(TMS.test.dat[,1]-TMS.pred.lm))/2
MAE.out.TMS.LM.Timeseries<- sum(abs(TMS.test.dat[,1]-TMS.pred.lm.1))/2


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

MSE.in.TMS.lasso<- sum( (TMS.dat.sub[1:14,1] - TMS.pred.in)^2 )/14
MSE.out.TMS.lasso<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out)^2 )/2

MAE.in.TMS.lasso<- sum( abs(TMS.dat.sub[1:14,1] - TMS.pred.in) )/14
MAE.out.TMS.lasso<- sum( abs(TMS.dat.sub[15:16,1] - TMS.pred.out) )/2


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
DW.lasso.timeseries<-sum((resid.lasso.1[2:13]-resid.lasso.1[1:12])^2)/sum((resid.lasso.1[2:13])^2) #DW=2.211832

TMS.dat.s$TMS.1<-c(NA,TMS.dat.sub[1:15,1])
summary(glmnetSE(data=TMS.dat.s[2:15,], cf.no.shrnkg = c("TransitUPT", "TMS.1"), alpha=1,r=300, method="none",seed = 1234, ncore = 1))


MSE.in.TMS.lasso.timeseries<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.lasso.timeseries<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.lasso.timeseries<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.lasso.timeseries<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2



#Ridge - Training vs. Testing (without TMS(t-1))
TMS.ridge.in<-glmnet(TMS.dat.sub[1:14,-c(1,7)], TMS.dat.sub[1:14,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(TMS.dat.sub[1:14,-c(1,7)]), TMS.dat.sub[1:14,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=14, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

TMS.pred.in<-coef(TMS.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,14), TMS.dat.sub[1:14,-c(1,7)])) #fitted values
TMS.pred.out<-coef(TMS.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1,7)])) 

resid.ridge<-TMS.dat.sub[1:14,1]-TMS.pred.in
DW.ridge<-sum((resid.ridge[2:14]-resid.ridge[1:13])^2)  /sum((resid.ridge[2:14])^2) #DW=2.12

MSE.in.TMS.ridge<- sum( (TMS.dat.sub[1:14,1] - TMS.pred.in)^2 )/14
MSE.out.TMS.ridge<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out)^2 )/2

MAE.in.TMS.ridge<- sum( abs(TMS.dat.sub[1:14,1] - TMS.pred.in) )/14
MAE.out.TMS.ridge<- sum( abs(TMS.dat.sub[15:16,1] - TMS.pred.out) )/2


#Ridge - Training vs. Testing (with VMT(t-1)) - Table 21
TMS.ridge.in.1<-glmnet(TMS.dat.sub[2:14,-1], TMS.dat.sub[2:14,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(TMS.dat.sub[2:14,-1]), TMS.dat.sub[2:14,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=13, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

TMS.pred.in.1<-coef(TMS.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,13), TMS.dat.sub[2:14,-c(1)])) #fitted values
TMS.pred.out.1<-coef(TMS.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),TMS.dat.sub[15:16,-c(1)])) 

resid.ridge.1<-TMS.dat.sub[2:14,1]-TMS.pred.in.1
DW.ridge.timeseries<-sum((resid.ridge.1[2:13]-resid.ridge.1[1:12])^2)  /sum((resid.ridge.1[2:13])^2) #DW=2.58

MSE.in.TMS.ridge.timeseries<- sum( (TMS.dat.sub[2:14,1] - TMS.pred.in.1)^2 )/13
MSE.out.TMS.ridge.timeseries<- sum( (TMS.dat.sub[15:16,1] - TMS.pred.out.1)^2 )/2

MAE.in.TMS.ridge.timeseries<- sum(abs(TMS.dat.sub[2:14,1] - TMS.pred.in.1))/13
MAE.out.TMS.ridge.timeseries<- sum(abs(TMS.dat.sub[15:16,1] - TMS.pred.out.1))/2



TMS.df <- data.frame(Models = (c('TMS - Linear Regression without TMS(t-1)','TMS - Ridge Regression without TMS(t-1)',  'TMS - LASSO without TMS(t-1)',
                                   'TMS - Linear Regression with TMS(t-1)',  'TMS - LASSO with TMS(t-1)', 'TMS - Ridge Regression with TMS(t-1)')),
                     MAE.IN = (c(MAE.in.TMS, MAE.in.TMS.ridge, MAE.in.TMS.lasso,
                                 MAE.in.TMS.1, MAE.in.TMS.lasso.timeseries, MAE.in.TMS.ridge.timeseries)),
                     MAE.OUT = (c(MAE.out.TMS.LM, MAE.out.TMS.ridge ,MAE.out.TMS.lasso, 
                                  MAE.out.TMS.LM.Timeseries, MAE.out.TMS.lasso.timeseries, MAE.out.TMS.ridge.timeseries)),
                     DurbinWatson = (c(DW.LM, DW.ridge,DW.lasso,
                                       DW.LM.Timeseries, DW.lasso.timeseries, DW.ridge.timeseries
                     )))

#output of VMT models as it relates to figure 3-2
TMS.df

