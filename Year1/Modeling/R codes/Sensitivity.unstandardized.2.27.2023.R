setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-VMT.dat[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,1])
VMT.dat.sub<-VMT.dat.sub[1:19,]

#VMT function from best model - Ridge without VMT(t-1)
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.2320300000E+12-3.2602450000E+10*Unemployment.Rate.1+7.8632320000E+05*TotLaneMiles-2.3706360000E+01*TransitPMT-1.3724310000E+07*EVPrivateandPublicChargingStationLocations+0*2.5227620000E+13*populationTelework
  return(VMT)
}

VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
low<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
c(mean(VMT.pred),low,high)

VMT.range<-(max(VMT.dat.sub$VMT)-min(VMT.dat.sub$VMT))
-3.2602450000E+10*(max(VMT.dat.sub$Unemployment.Rate.1)-min(VMT.dat.sub$Unemployment.Rate.1))/100
7.8632320000E+05*(max(VMT.dat.sub$TotLaneMiles)-min(VMT.dat.sub$TotLaneMiles))/100
-2.3706360000E+01*(max(VMT.dat.sub$TransitPMT)-min(VMT.dat.sub$TransitPMT))/100
-1.3724310000E+07*(max(VMT.dat.sub$EVPrivateandPublicChargingStationLocations)-min(VMT.dat.sub$EVPrivateandPublicChargingStationLocations))/100
2.5227620000E+13*(max(VMT.dat.sub$populationTelework)-min(VMT.dat.sub$populationTelework))/100


low.0<-mean(VMT.dat.sub$VMT)-2*sd(VMT.dat.sub$VMT)/sqrt(length(VMT.dat.sub$VMT)) #raw data
high.0<-mean(VMT.dat.sub$VMT)+2*sd(VMT.dat.sub$VMT)/sqrt(length(VMT.dat.sub$VMT))




#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
#GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])

#GHG function from best model - lasso with VMT(t-1)
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 1.358508e+03-1.408007e+01*Unemployment.Rate.1-6.909597e-09*TransitPMT+5.089045e-01*GHG.1
  return(GHG)
}

GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1); GHG.pred<-GHG.pred[-1]
low<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
high<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
c(mean(GHG.pred),low,high)


low.0<-mean(GHG.dat.sub$TransportGHG)-2*sd(GHG.dat.sub$TransportGHG)/sqrt(length(GHG.dat.sub$TransportGHG)) #raw data
high.0<-mean(GHG.dat.sub$TransportGHG)+2*sd(GHG.dat.sub$TransportGHG)/sqrt(length(GHG.dat.sub$TransportGHG))


GHG.range<-(max(GHG.dat.sub$TransportGHG)-min(GHG.dat.sub$TransportGHG))
-6.909597e-09*(max(GHG.dat.sub$TransitPMT)-min(GHG.dat.sub$TransitPMT))/100
-1.408007e+01*(max(GHG.dat.sub$Unemployment.Rate.1)-min(GHG.dat.sub$Unemployment.Rate.1))/100

#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
#TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])


#TMS function from best model - lasso with VMT(t-1)
TMS<-function(TransitUPT, TMS.1){
  TMS<- 1.773351e-02+1.965458e-12*TransitUPT + 2.481566e-01*TMS.1
  return(TMS)
}


TMS.pred<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1); TMS.pred<-TMS.pred[-1]
low<-mean(TMS.pred)-2*sd(TMS.pred)/length(TMS.pred)
high<-mean(TMS.pred)+2*sd(TMS.pred)/length(TMS.pred)
c(mean(TMS.pred),low,high)

low.0<-mean(TMS.dat.sub$TransitModeShare)-2*sd(TMS.dat.sub$TransitModeShare)/sqrt(length(TMS.dat.sub$TransitModeShare)) #raw data
high.0<-mean(TMS.dat.sub$TransitModeShare)+2*sd(TMS.dat.sub$TransitModeShare)/sqrt(length(TMS.dat.sub$TransitModeShare))

TMS.range<-(max(TMS.dat.sub$TransitModeShare)-min(TMS.dat.sub$TransitModeShare))
1.965458e-12*(max(TMS.dat.sub$TransitUPT)-min(TMS.dat.sub$TransitUPT))/100


