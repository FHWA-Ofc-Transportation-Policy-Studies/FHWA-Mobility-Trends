setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#VMT#
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)

## Select LASSO model with largeest R^2 from each trend
miles<-c("TotLaneMiles", "TotLocToCollMiles", "TotalArtToFwyMiles")
transit<-c("TransitDRM","TransitPMT","TransitUPT")
ev<-c("RegisteredBEVCarsMillions","EVPrivateandPublicChargingStationLocations")
VMT.mod<-matrix(,18,7);m=1
lambdas=10^seq(5,-5,by=-.1)
#library(glmnet)
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:2){
      VMT.dat.sub<-VMT.dat.s[,c("VMT","Unemployment.Rate.1", miles[i],transit[j],ev[k],"populationTelework")]
      cv.lasso<-cv.glmnet(as.matrix(VMT.dat.sub[,-1]), VMT.dat.sub[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
      optimal.lambda<- cv.lasso$lambda.min
      lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio)
      r2<-lambda.rsq[lambda.rsq[,1]==optimal.lambda][2]
      VMT.mod[m,1]<-colnames(VMT.dat.sub)[1]; VMT.mod[m,2]<-colnames(VMT.dat.sub)[2]; VMT.mod[m,3]<-colnames(VMT.dat.sub)[3]
      VMT.mod[m,4]<-colnames(VMT.dat.sub)[4]; VMT.mod[m,5]<-colnames(VMT.dat.sub)[5]; VMT.mod[m,6]<-colnames(VMT.dat.sub)[6]
      VMT.mod[m,7]<-r2
      m<-m+1
    }
  }
}

#Export the file
write.csv(VMT.mod, 'VMT.r2.mod.csv')

#Bootstrap  p-values
#library(glmnetSE)
#Builds a LASSO, Ridge, or Elastic Net model with glmnet or cv.glmnet with bootstrap inference statistics (SE, CI, and p-value) 
#for selected coefficients with no shrinkage applied for them.
summary(glmnetSE(data=VMT.dat.s, 
                 cf.no.shrnkg = c("Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), 
                 alpha=1,r=500, method="none",seed = 123, ncore = 1))

#Compare with multiple linear regression
summary(lm(VMT.dat.s[,1]~VMT.dat.s[,"Unemployment.Rate.1"]+VMT.dat.s[,"TotLaneMiles"]+VMT.dat.s[,"TransitPMT"]+VMT.dat.s[,"EVPrivateandPublicChargingStationLocations"]+VMT.dat.s[,"populationTelework"]))





#GHG#
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)

## Select LASSO model with largest R^2 from each trend
miles<-c("TotLaneMiles", "TotLocToCollMiles", "TotalArtToFwyMiles")
transit<-c("TransitDRM","TransitPMT","TransitUPT")
ev<-c("RegisteredBEVCarsMillions","EVPrivateandPublicChargingStationLocations")
GHG.mod<-matrix(,18,7);m=1
lambdas=10^seq(5,-5,by=-.1)
#library(glmnet)
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:2){
      GHG.dat.sub<-GHG.dat.s[,c("TransportGHG","Unemployment.Rate.1", miles[i],transit[j],ev[k],"populationTelework")]
      cv.lasso<-cv.glmnet(as.matrix(GHG.dat.sub[,-1]), GHG.dat.sub[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=20, lambda=lambdas)
      optimal.lambda<- cv.lasso$lambda.min
      lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio)
      r2<-lambda.rsq[lambda.rsq[,1]==optimal.lambda][2]
      GHG.mod[m,1]<-colnames(GHG.dat.sub)[1]; GHG.mod[m,2]<-colnames(GHG.dat.sub)[2]; GHG.mod[m,3]<-colnames(GHG.dat.sub)[3]
      GHG.mod[m,4]<-colnames(GHG.dat.sub)[4]; GHG.mod[m,5]<-colnames(GHG.dat.sub)[5]; GHG.mod[m,6]<-colnames(GHG.dat.sub)[6]
      GHG.mod[m,7]<-r2
      m<-m+1
    }
  }
}

#Export the file
write.csv(GHG.mod, 'GHG.r2.mod.csv')

#Bootstrap  p-values
#library(glmnetSE)
#Builds a LASSO, Ridge, or Elastic Net model with glmnet or cv.glmnet with bootstrap inference statistics (SE, CI, and p-value) 
#for selected coefficients with no shrinkage applied for them.
summary(glmnetSE(data=GHG.dat.s, 
                 cf.no.shrnkg = c("Unemployment.Rate.1","TotLaneMiles","TransitPMT","EVPrivateandPublicChargingStationLocations","populationTelework"), 
                 alpha=1,r=500, method="none",seed = 1234, ncore = 1))

#Compare with multiple linear regression
summary(lm(GHG.dat.s[,1]~GHG.dat.s[,"Unemployment.Rate.1"]+GHG.dat.s[,"TotalArtToFwyMiles"]+GHG.dat.s[,"TransitPMT"]+GHG.dat.s[,"EVPrivateandPublicChargingStationLocations"]+GHG.dat.s[,"populationTelework"]))

GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]


#TMS#
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)

## Select LASSO model with largest R^2 from each trend
miles<-c("TotLaneMiles", "TotLocToCollMiles", "TotalArtToFwyMiles")
transit<-c("TransitDRM","TransitPMT","TransitUPT")
ev<-c("RegisteredBEVCarsMillions","EVPrivateandPublicChargingStationLocations")
TMS.mod<-matrix(,18,7);m=1
lambdas=10^seq(5,-5,by=-.1)
#library(glmnet)
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:2){
      TMS.dat.sub<-TMS.dat.s[,c("TransitModeShare","Unemployment.Rate.1", miles[i],transit[j],ev[k],"populationTelework")]
      cv.lasso<-cv.glmnet(as.matrix(TMS.dat.sub[,-1]), TMS.dat.sub[,1],alpha=1,type="deviance", family="gaussian", standardize=TRUE, nfolds=16, lambda=lambdas)
      optimal.lambda<- cv.lasso$lambda.min
      lambda.rsq<-cbind(cv.lasso$lambda,lasso.mod$dev.ratio)
      r2<-lambda.rsq[lambda.rsq[,1]==optimal.lambda][2]
      TMS.mod[m,1]<-colnames(TMS.dat.sub)[1]; TMS.mod[m,2]<-colnames(TMS.dat.sub)[2]; TMS.mod[m,3]<-colnames(TMS.dat.sub)[3]
      TMS.mod[m,4]<-colnames(TMS.dat.sub)[4]; TMS.mod[m,5]<-colnames(TMS.dat.sub)[5]; TMS.mod[m,6]<-colnames(TMS.dat.sub)[6]
      TMS.mod[m,7]<-r2
      m<-m+1
    }
  }
}

#Export the file
write.csv(TMS.mod, 'TMS.r2.mod.csv')

#Bootstrap  p-values
#library(glmnetSE)
#Builds a LASSO, Ridge, or Elastic Net model with glmnet or cv.glmnet with bootstrap inference statistics (SE, CI, and p-value) 
#for selected coefficients with no shrinkage applied for them.
summary(glmnetSE(data=TMS.dat.s, 
                 cf.no.shrnkg = c("Unemployment.Rate.1","TotalArtToFwyMiles","TransitUPT","RegisteredBEVCarsMillions","populationTelework"), 
                 alpha=1,r=500, method="none",seed = 1234, ncore = 1))

#Compare with multiple linear regression
summary(lm(TMS.dat.s[,1]~TMS.dat.s[,"Unemployment.Rate.1"]+TMS.dat.s[,"TotalArtToFwyMiles"]+TMS.dat.s[,"TransitPMT"]+TMS.dat.s[,"EVPrivateandPublicChargingStationLocations"]+TMS.dat.s[,"populationTelework"]))




