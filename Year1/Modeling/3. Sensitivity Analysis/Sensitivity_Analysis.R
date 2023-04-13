#This script is used to create the sensitivity analysis for the 1.0 Models. 
#It relates to Step 4: Tables 13, 14,and 15

#read the dataset
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

library(glmnet) 
#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-VMT.dat
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[-c(19:20),c(1,2,3,7,10,11)]

#No coefficent or variable change
#VMT function from best model - Ridge without VMT(t-1)
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.232030e+12-3.257383e+10*Unemployment.Rate.1+ 7.837331e+05*TotLaneMiles-2.367927e+01*TransitPMT-1.371791e+07*EVPrivateandPublicChargingStationLocations+ 2.528127e+13*populationTelework
  return(VMT)
}


VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)

#unemployment
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.232030e+12- 3.257383e+10*(Unemployment.Rate.1+ 1) + 7.837331e+05*TotLaneMiles-2.367927e+01*TransitPMT-1.371791e+07*EVPrivateandPublicChargingStationLocations+ 2.528127e+13*populationTelework
  return(VMT)
}

VMT.pred.unemployment<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)

#totlanemiles
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.232030e+12-3.257383e+10*Unemployment.Rate.1+ 7.837331e+05*(TotLaneMiles*1.01)-2.367927e+01*TransitPMT-1.371791e+07*EVPrivateandPublicChargingStationLocations+ 2.528127e+13*populationTelework
  return(VMT)
}

VMT.pred.lanemiles<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)

#TransitPMT
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.232030e+12-3.257383e+10*Unemployment.Rate.1+ 7.837331e+05*TotLaneMiles-2.367927e+01*(TransitPMT*1.01)-1.371791e+07*EVPrivateandPublicChargingStationLocations+ 2.528127e+13*populationTelework
  return(VMT)
}


VMT.pred.Transit<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)

#EV charging stations
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.232030e+12-3.257383e+10*Unemployment.Rate.1+ 7.837331e+05*TotLaneMiles-2.367927e+01*TransitPMT-1.371791e+07*(EVPrivateandPublicChargingStationLocations*1.01)+ 2.528127e+13*populationTelework
  return(VMT)
}

VMT.pred.EVcharging<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)

#Population Telework
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- -3.232030e+12-3.257383e+10*Unemployment.Rate.1+ 7.837331e+05*TotLaneMiles-2.367927e+01*TransitPMT-1.371791e+07*EVPrivateandPublicChargingStationLocations+ 2.528127e+13*(populationTelework + .01)
  return(VMT)
}

VMT.pred.telework <-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)

one_percent_sensitivity <-function(pred, changed_pred){
  Sensitivity <- (((changed_pred-pred)/pred)* 100)
  return(Sensitivity)
}


VMT.sensitivity.percent.change <- data.frame(Indicator = c("Unemployment.Rate", "TotLaneMiles", "TransitPMT", 'EV CHarging', '% Telework'),
                                             percent_change = c(one_percent_sensitivity(VMT.pred[1], VMT.pred.unemployment[1]), 
                                                                one_percent_sensitivity(VMT.pred[1], VMT.pred.lanemiles[1]),
                                                                one_percent_sensitivity(VMT.pred[1], VMT.pred.Transit[1]),
                                                                one_percent_sensitivity(VMT.pred[1], VMT.pred.EVcharging[1]),
                                                                one_percent_sensitivity(VMT.pred[1], VMT.pred.telework[1])))
#Table 13
VMT.sensitivity.percent.change

##GHG###############################################################################################################

GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-GHG.dat
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
GHG.dat.sub<-GHG.dat.sub[,c(1,2,4,7)]


#no coefficent / variable change
#GHG function from best model - lasso with VMT(t-1)
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 1.358508e+03-1.408007e+01*Unemployment.Rate.1-6.909597e-09*TransitPMT+ 5.089045e-01*GHG.1
  return(GHG)
}

GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1)

#unemployment
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 1.358508e+03-1.408007e+01*(Unemployment.Rate.1+1)-6.909597e-09*TransitPMT+ 5.089045e-01*GHG.1
  return(GHG)
}

GHG.pred.unemployment<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1)

#TransitPMT
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 1.358508e+03-1.408007e+01*Unemployment.Rate.1-6.909597e-09*(TransitPMT*1.01)+ 5.089045e-01*GHG.1
  return(GHG)
}

GHG.pred.transit<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1)


GHG.sensitivity.percent.change <- data.frame(Indicator = c("Unemployment.Rate", "TransitPMT"),
                                             percent_change = c(one_percent_sensitivity(GHG.pred[2], GHG.pred.unemployment[2]), 
                                                                one_percent_sensitivity(GHG.pred[2], GHG.pred.transit[2])))
#Table 14
GHG.sensitivity.percent.change
                
##TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-TMS.dat
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])


#TMS function from best model - lasso with VMT(t-1)
TMS<-function(TransitUPT, TMS.1){
  TMS<-  1.773351e-02+1.965458e-12*TransitUPT + 2.481566e-01*TMS.1
  return(TMS)
}

TMS.pred<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1)


#TransitUPT
TMS<-function(TransitUPT, TMS.1){
  TMS<-  1.773351e-02+1.965458e-12*(TransitUPT * 1.01) + 2.481566e-01*TMS.1
  return(TMS)
}

TMS.pred.transit<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1)


TMS.sensitivity.percent.change <- data.frame(Indicator = c("TransitUPT"),
                                             percent_change = c(one_percent_sensitivity(TMS.pred[2], TMS.pred.transit[2])))
#Table 15
TMS.sensitivity.percent.change





                             