setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#Create variables: VMT.Cap=VMT/Population and GHG.Cap<-GHG/Population
dat$VMT.Cap<-dat$VMT/dat$Population
dat$GHG.Cap<-dat$TransportGHG/dat$Population

dat.sub<-dat[,c("year","VMT.Cap","GHG.Cap","Unemployment.Rate.1","TotLaneMiles","TotalArtToFwyMiles",
                "TransitPMT","TransitUPT","populationTelework","EVPrivateandPublicChargingStationLocations")]
dat.sub$VMT.Cap.t1<-c(NA,dat.sub[1:19,"VMT.Cap"])  #add lag variables
dat.sub$GHG.Cap.t1<-c(NA,dat.sub[1:19,"GHG.Cap"])  #add lag variables

#VMT.Cap function from best model 
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework,VMT.Cap.t1){
  VMT<- 3.274958e+03-3.951695e+01*Unemployment.Rate.1+3.249225e-04*TotLaneMiles-3.821160e-08*TransitPMT+5.750459e-06*EVPrivateandPublicChargingStationLocations+7.743315e+03*populationTelework+0*VMT.Cap.t1
  return(VMT)
}

#VMT<- 3.274958e+03-3.951695e+01*Unemployment.Rate.1+3.249225e-04*TotLaneMiles-3.821160e-08*TransitPMT+5.750459e-06*EVPrivateandPublicChargingStationLocations+7.743315e+03*populationTelework+5.797749e-01*VMT.Cap.t1
#dat.sub<-dat.sub[2:18,]

VMT.pred<-VMT(dat.sub$Unemployment.Rate.1, dat.sub$TotLaneMiles, dat.sub$TransitPMT, dat.sub$EVPrivateandPublicChargingStationLocations, dat.sub$populationTelework, dat.sub$VMT.Cap.t1)
low<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
c(mean(VMT.pred),low,high)

VMT.range<-(max(dat.sub$VMT.Cap)-min(dat.sub$VMT.Cap))
(-3.951695e+01*(max(dat.sub$Unemployment.Rate.1)-min(dat.sub$Unemployment.Rate.1))/100)/VMT.range
(3.249225e-04*(max(dat.sub$TotLaneMiles)-min(dat.sub$TotLaneMiles))/100)/VMT.range
(-3.821160e-08*(max(dat.sub$TransitPMT)-min(dat.sub$TransitPMT))/100)/VMT.range
(5.750459e-06*(max(dat.sub$EVPrivateandPublicChargingStationLocations)-min(dat.sub$EVPrivateandPublicChargingStationLocations))/100)/VMT.range
(7.743315e+03*(max(dat.sub$populationTelework)-min(dat.sub$populationTelework))/100)/VMT.range


low.0<-mean(VMT.dat.sub$VMT)-2*sd(VMT.dat.sub$VMT)/sqrt(length(VMT.dat.sub$VMT)) #raw data
high.0<-mean(VMT.dat.sub$VMT)+2*sd(VMT.dat.sub$VMT)/sqrt(length(VMT.dat.sub$VMT))




#GHG###############################################################################################################

#GHG function 
GHG<-function(Unemployment.Rate.1, TransitPMT, populationTelework, EVPrivateandPublicChargingStationLocations){
  GHG<- 1.00E+01-1.22E-01*Unemployment.Rate.1-7.27E-11*TransitPMT+1.98E+01*populationTelework-0*EVPrivateandPublicChargingStationLocations
  return(GHG)
}

GHG<- 1.00E+01-1.22E-01*Unemployment.Rate.1-7.27E-11*TransitPMT+1.98E+01*populationTelework-4.18E-05*EVPrivateandPublicChargingStationLocations

GHG.pred<-GHG(dat.sub$Unemployment.Rate.1, dat.sub$TransitPMT, dat.sub$populationTelework, dat.sub$EVPrivateandPublicChargingStationLocations)
low<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
high<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
c(mean(GHG.pred),low,high)


low.0<-mean(GHG.dat.sub$TransportGHG)-2*sd(GHG.dat.sub$TransportGHG)/sqrt(length(GHG.dat.sub$TransportGHG)) #raw data
high.0<-mean(GHG.dat.sub$TransportGHG)+2*sd(GHG.dat.sub$TransportGHG)/sqrt(length(GHG.dat.sub$TransportGHG))


GHG.range<-1e+06*(max(dat.sub$GHG.Cap)-min(dat.sub$GHG.Cap))
(-1.22E-01*(max(dat.sub$Unemployment.Rate.1)-min(dat.sub$Unemployment.Rate.1))/100)/GHG.range
(-7.27E-11*(max(dat.sub$TransitPMT)-min(dat.sub$TransitPMT))/100)/GHG.range
(1.98E+01*(max(dat.sub$populationTelework)-min(dat.sub$populationTelework))/100)/GHG.range
(-4.18E-05*(max(dat.sub$EVPrivateandPublicChargingStationLocations)-min(dat.sub$EVPrivateandPublicChargingStationLocations))/100)/GHG.range


