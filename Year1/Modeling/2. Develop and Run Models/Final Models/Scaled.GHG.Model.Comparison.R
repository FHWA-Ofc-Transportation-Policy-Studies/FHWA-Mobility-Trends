#scaled models 

#The below analysis compares Linear Regression, Lasso, and Ridge Regression for GHG
#Each model will be evaluated with and without time-series considerations (ie, time series models will include the 
#performance metric lagged (t-1) within the model. )

#The final results for each section will relate to figure 3-2 within the Trends Indicator Memorandum.

library(glmnet)
library(glmnetSE)
#read the dataset (may need to change this path to get to file)
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)


#GHG###############################################################################################################
#read in columns, scale data, create time series lag
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])


#Linear - whole data
GHG.lm<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=GHG.dat.sub)
GHG.lm.timeseries<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+GHG.1, data=GHG.dat.sub[2:20,])

GHG.test.dat<-GHG.dat.sub[19:20,]

#Linear - Training vs. Testing
GHG.lm.train<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework, data=GHG.dat.sub[1:18,])
GHG.lm.train.GHG.lm.timeseries<-lm(TransportGHG~Unemployment.Rate.1+TotLaneMiles+TransitPMT+EVPrivateandPublicChargingStationLocations+populationTelework+GHG.1, data=GHG.dat.sub[2:18,])

resid.lm<-resid(GHG.lm.train)
DW.GHG.LM<-sum((resid.lm[2:18]-resid.lm[1:17])^2)/sum((resid.lm[2:18])^2) #DW=2.314663
resid.lm.1<-resid(GHG.lm.train.GHG.lm.timeseries)
DW.GHG.LM.Timeseries<-sum((resid.lm.1[2:16]-resid.lm.1[1:15])^2)/sum((resid.lm.1[2:16])^2) #DW=3.258663

MSE.in.GHG.LM<- sum((GHG.dat.sub$TransportGHG[1:18]-GHG.lm.train$fitted.values)^2)/18
MSE.in.GHG.LM.Timeseries<- sum((GHG.dat.sub$TransportGHG[2:18]-GHG.lm.train.GHG.lm.timeseries$fitted.values)^2)/17

MAE.in.GHG.LM<- sum(abs(GHG.dat.sub$TransportGHG[1:18]-GHG.lm.train$fitted.values))/18
MAE.in.GHG.LM.Timeseries<- sum(abs(GHG.dat.sub$TransportGHG[2:18]-GHG.lm.train.GHG.lm.timeseries$fitted.values))/17


GHG.pred.lm<-predict(GHG.lm.train, newdata=GHG.test.dat[,-1])
GHG.pred.lm.1<-predict(GHG.lm.train.GHG.lm.timeseries, newdata=GHG.dat.sub[19:20,-1])

MSE.out.GHG.lm<- sum((GHG.test.dat[,1]-GHG.pred.lm)^2)/2
MSE.out.GHG.lm.Timeseries<- sum((GHG.test.dat[,1]-GHG.pred.lm.1)^2)/2

MAE.out.GHG.lm<- sum(abs(GHG.test.dat[,1]-GHG.pred.lm))/2
MAE.out.GHG.lm.Timeseries<- sum(abs(GHG.test.dat[,1]-GHG.pred.lm.1))/2





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

MSE.in.GHG.lasso<- sum( (GHG.dat.sub[1:18,1] - GHG.pred.in)^2 )/18
MSE.out.GHG.lasso<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out)^2 )/2

MAE.in.GHG.lasso<- sum( abs(GHG.dat.sub[1:18,1] - GHG.pred.in) )/18
MAE.out.GHG.lasso<- sum( abs(GHG.dat.sub[19:20,1] - GHG.pred.out) )/2


#LASSO - Training vs. Testing (with GHG(t-1)) (BEST model)
GHG.lasso.in.1<-glmnet(GHG.dat.sub[2:18,-c(1)], GHG.dat.sub[2:18,1], alpha=1, family="gaussian", lambda=lambdas)
cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1)]), GHG.dat.sub[2:18,1],alpha=1,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.lasso$lambda.min; 

GHG.pred.in.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)])) #fitted values
GHG.pred.out.1<-coef(GHG.lasso.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1)])) 


GHG.dat.s$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
summary(glmnetSE(data=GHG.dat.s[2:18,], 
                 cf.no.shrnkg = c("Unemployment.Rate.1","TransitPMT","GHG.1"), 
                 alpha=1,r=500, method="none",seed = 1234, ncore = 1))

resid.lasso.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1; r2<-1-sum(resid.lasso.1^2)/sum(  (GHG.dat.sub[1:18,1]-mean(GHG.dat.sub[1:18,1]))^2 );r2
DW.lasso.LASSO.timeseries<-sum((resid.lasso.1[2:17]-resid.lasso.1[1:16])^2)  /sum((resid.lasso.1[2:17])^2) #DW=3.626901

MSE.in.GHG.LASSO.timeseries<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.LASSO.timeseries<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.LASSO.timeseries<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.LASSO.timeseries<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2


#Ridge - Training vs. Testing (without GHG(t-1))
GHG.ridge.in<-glmnet(GHG.dat.sub[1:18,-c(1,7)], GHG.dat.sub[1:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.sub[1:18,-c(1,7)]), GHG.dat.sub[1:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=18, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

GHG.pred.in<-coef(GHG.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,18), GHG.dat.sub[1:18,-c(1,7)])) #fitted values
GHG.pred.out<-coef(GHG.ridge.in, s=optimal.lambda)[1:6] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1,7)])) 

resid.ridge<-GHG.dat.sub[1:18,1]-GHG.pred.in
DW.ridge<-sum((resid.ridge[2:18]-resid.ridge[1:17])^2)/sum((resid.ridge[2:18])^2) #DW=1.88

MSE.in.GHG.ridge<- sum( (GHG.dat.sub[1:18,1] - GHG.pred.in)^2 )/18
MSE.out.GHG.ridge<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out)^2 )/2

MAE.in.GHG.ridge<- sum( abs(GHG.dat.sub[1:18,1] - GHG.pred.in) )/18
MAE.out.GHG.ridge<- sum( abs(GHG.dat.sub[19:20,1] - GHG.pred.out) )/2


#Ridge - Training vs. Testing (with GHG(t-1)) - Ridge,appendix table 20
GHG.ridge.in.1<-glmnet(GHG.dat.sub[2:18,-c(1)], GHG.dat.sub[2:18,1], alpha=0, family="gaussian", lambda=lambdas)
cv.ridge<-cv.glmnet(as.matrix(GHG.dat.sub[2:18,-c(1)]), GHG.dat.sub[2:18,1],alpha=0,type="mse", family="gaussian", standardize=TRUE, nfolds=17, lambda=lambdas)
optimal.lambda <- cv.ridge$lambda.min; 

GHG.pred.in.1<-coef(GHG.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,17), GHG.dat.sub[2:18,-c(1)])) #fitted values
GHG.pred.out.1<-coef(GHG.ridge.in.1, s=optimal.lambda)[1:7] %*% t(cbind(rep(1,2),GHG.dat.sub[19:20,-c(1)])) 

resid.ridge.1<-GHG.dat.sub[2:18,1]-GHG.pred.in.1
DW.ridge.timeseries<-sum((resid.ridge.1[2:17]-resid.ridge.1[1:16])^2)  /sum((resid.ridge.1[2:17])^2) #DW=3.62

MSE.in.GHG.ridge.timeseries<- sum( (GHG.dat.sub[2:18,1] - GHG.pred.in.1)^2 )/17
MSE.out.GHG.ridge.timeseries<- sum( (GHG.dat.sub[19:20,1] - GHG.pred.out.1)^2 )/2

MAE.in.GHG.ridge.timseries<- sum(abs(GHG.dat.sub[2:18,1] - GHG.pred.in.1))/17
MAE.out.GHG.ridge.timeseries<- sum(abs(GHG.dat.sub[19:20,1] - GHG.pred.out.1))/2



GHG.df <- data.frame(Models = (c('GHG - Linear Regression without GHG(t-1)','GHG - Ridge Regression without GHG(t-1)',  'GHG - LASSO without GHG(t-1)',
                                 'GHG - Linear Regression with GHG(t-1)',  'GHG - LASSO with GHG(t-1)', 'GHG - Ridge Regression with GHG(t-1)')),
                     MAE.IN = (c(MAE.in.GHG.LM, MAE.in.GHG.ridge ,MAE.in.GHG.lasso,
                                 MAE.in.GHG.LM.Timeseries, MAE.in.GHG.LASSO.timeseries, MAE.in.GHG.ridge.timseries)),
                     MAE.OUT = (c(MAE.out.GHG.lm, MAE.out.GHG.ridge, MAE.out.GHG.lasso,
                                  MAE.out.GHG.lm.Timeseries, MAE.out.GHG.LASSO.timeseries, MAE.out.GHG.ridge.timeseries)),
                     DurbinWatson = (c(DW.GHG.LM, DW.ridge,DW.lasso,
                                       DW.GHG.LM.Timeseries, DW.lasso.LASSO.timeseries, DW.ridge.timeseries
                     )))

#output of VMT models as it relates to figure 3-2
GHG.df

