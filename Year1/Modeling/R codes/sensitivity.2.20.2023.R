setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

library(glmnet)

#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[-c(19:20),c(1,2,3,7,10,11)]

#VMT function from best model - Ridge without VMT(t-1)
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-.376350428*Unemployment.Rate.1+.790475824*TotLaneMiles-.500441968*TransitPMT-.650238158*EVPrivateandPublicChargingStationLocations+1.136597306*populationTelework
  return(VMT)
}

VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
low<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
c(mean(VMT.pred),low,high)

low.0<-mean(VMT.dat.sub$VMT)-2*sd(VMT.dat.sub$VMT)/sqrt(length(VMT.dat.sub$VMT)) #raw data
high.0<-mean(VMT.dat.sub$VMT)+2*sd(VMT.dat.sub$VMT)/sqrt(length(VMT.dat.sub$VMT))


#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
GHG.dat.sub<-GHG.dat.sub[,c(1,2,4,7)]

#GHG function from best model - lasso with VMT(t-1)
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 0.02571576-0.32966013*Unemployment.Rate.1-0.36775031*TransitPMT+0.50890281*GHG.1
  return(GHG)
}


GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1); GHG.pred<-GHG.pred[-1]
low<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
high<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
c(mean(GHG.pred),low,high)

low.0<-mean(GHG.dat.sub$TransportGHG)-2*sd(GHG.dat.sub$TransportGHG)/sqrt(length(GHG.dat.sub$TransportGHG)) #raw data
high.0<-mean(GHG.dat.sub$TransportGHG)+2*sd(GHG.dat.sub$TransportGHG)/sqrt(length(GHG.dat.sub$TransportGHG))




#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])


#TMS function from best model - lasso with VMT(t-1)
TMS<-function(TransitUPT, TMS.1){
  TMS<- 0.06859487+0.55820411*TransitUPT + 0.24366100*TMS.1
  return(TMS)
}


TMS.pred<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1); TMS.pred<-TMS.pred[-1]
low<-mean(TMS.pred)-2*sd(TMS.pred)/length(TMS.pred)
high<-mean(TMS.pred)+2*sd(TMS.pred)/length(TMS.pred)
c(mean(TMS.pred),low,high)

low.0<-mean(TMS.dat.sub$TransitModeShare)-2*sd(TMS.dat.sub$TransitModeShare)/sqrt(length(TMS.dat.sub$TransitModeShare)) #raw data
high.0<-mean(TMS.dat.sub$TransitModeShare)+2*sd(TMS.dat.sub$TransitModeShare)/sqrt(length(TMS.dat.sub$TransitModeShare))

