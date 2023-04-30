#This script creates the sensitivity analysis within Appendix F

#read the dataset
dat<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data20221210.csv", header=TRUE)

library(glmnet) 
#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[-c(19:20),c(1,2,3,7,10,11)]

#No coefficent or variable change
#VMT function from best model - Ridge without VMT(t-1)
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-.376350428*Unemployment.Rate.1+.790475824*TotLaneMiles-.500441968*TransitPMT-.650238158*EVPrivateandPublicChargingStationLocations+1.136597306*populationTelework
  return(VMT)
}

VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
mean.VMT.none <- mean(VMT.pred)
low.vmt.none<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high.vmt.none<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
vmt.mean.diff <- abs(mean.VMT.none - mean.VMT.none)

#Unemployment Coef x 0
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-0*Unemployment.Rate.1+.790475824*TotLaneMiles-.500441968*TransitPMT-.650238158*EVPrivateandPublicChargingStationLocations+1.136597306*populationTelework
  return(VMT)
}

VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
mean.VMT.unemployment <- mean(VMT.pred)
low.vmt.unemployment<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high.vmt.unemployment<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
vmt.mean.diff.unemployment <- abs(mean.VMT.none - mean.VMT.unemployment)

#Total Lane Miles Coeff x 0
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-.376350428*Unemployment.Rate.1+0*TotLaneMiles-.500441968*TransitPMT-.650238158*EVPrivateandPublicChargingStationLocations+1.136597306*populationTelework
  return(VMT)
}


VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
mean.VMT.lanemiles <- mean(VMT.pred)
low.vmt.lanemiles<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high.vmt.lanemiles<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
vmt.mean.diff.lanemiles <- abs(mean.VMT.none - mean.VMT.lanemiles)


#TransitPMT coeff x 0
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-.376350428*Unemployment.Rate.1+.790475824*TotLaneMiles-0*TransitPMT-.650238158*EVPrivateandPublicChargingStationLocations+1.136597306*populationTelework
  return(VMT)
}


VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
mean.VMT.PMT <- mean(VMT.pred)
low.vmt.PMT<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high.vmt.PMT<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
vmt.mean.diff.PMT <- abs(mean.VMT.none - mean.VMT.PMT)

#Ev charging station coeff x 0
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-.376350428*Unemployment.Rate.1+.790475824*TotLaneMiles-.500441968*TransitPMT-0*EVPrivateandPublicChargingStationLocations+1.136597306*populationTelework
  return(VMT)
}


VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
mean.VMT.EV <- mean(VMT.pred)
low.vmt.EV<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high.vmt.EV<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
vmt.mean.diff.EV <- abs(mean.VMT.none - mean.VMT.EV)

#Population telework ceoff x 0
VMT<-function(Unemployment.Rate.1, TotLaneMiles,TransitPMT,EVPrivateandPublicChargingStationLocations,populationTelework){
  VMT<- 0.008979102-.376350428*Unemployment.Rate.1+.790475824*TotLaneMiles-.500441968*TransitPMT-.650238158*EVPrivateandPublicChargingStationLocations+0*populationTelework
  return(VMT)
}


VMT.pred<-VMT(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$TotLaneMiles, VMT.dat.sub$TransitPMT, VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$populationTelework)
mean.VMT.tele <- mean(VMT.pred)
low.vmt.tele<-mean(VMT.pred)-2*sd(VMT.pred)/length(VMT.pred)
high.vmt.tele<-mean(VMT.pred)+2*sd(VMT.pred)/length(VMT.pred)
vmt.mean.tele <- abs(mean.VMT.none - mean.VMT.tele)


#table F-1
VMT.Sensitvity <- data.frame(coefficent_variable_change = c('None', 'Unemployment Coeff x 0', 'Total Lane Miles Coeff x 0', 'TransitPMT Coeff x 0',
                                                            'EV charging station Coeff x 0', 'Population Telework Coeff x 0'),
                             Coefficent_estimate = c('None',-0.376350428, .790475824, -.500441968 , -.650238158, 1.136597306),
                             Predicted_mean = c(mean.VMT.none, mean.VMT.unemployment, mean.VMT.lanemiles, mean.VMT.PMT, mean.VMT.EV, mean.VMT.tele),
                             Predicted_lower_mean_plus_2sd = c(low.vmt.none, low.vmt.unemployment, low.vmt.lanemiles, low.vmt.PMT, low.vmt.EV, low.vmt.tele),
                             Predicted_upper_mean_plus_2sd = c(high.vmt.none, high.vmt.unemployment, high.vmt.lanemiles, high.vmt.PMT, high.vmt.EV, high.vmt.tele),
                             Abs_mean_difference_from_None = c(vmt.mean.diff, vmt.mean.diff.unemployment,vmt.mean.diff.lanemiles, vmt.mean.diff.PMT,
                                                               vmt.mean.diff.EV, vmt.mean.tele)
)

VMT.Sensitvity




#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])
GHG.dat.sub<-GHG.dat.sub[,c(1,2,4,7)]

#Coefficents from final model (Table 8 in report):
#unemployment.rate.1  = -0.32966013
#TransitPMT = -0.36775031
#ghg.1 = 0.50890281

#no coefficent / variable change
#GHG function from best model - lasso with VMT(t-1)
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 0.02571576-0.32966013*Unemployment.Rate.1-0.36775031*TransitPMT+ 0.50890281*GHG.1
  return(GHG)
}

GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1); GHG.pred<-GHG.pred[-1]
GHG.pred.mean.none <- mean(GHG.pred)
GHG.low.none<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
GHG.high.none<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
GHG.difference.in.mean.none <- abs(GHG.pred.mean.none - GHG.pred.mean.none)

#Unemployment Coef * 0
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 0.02571576-0*Unemployment.Rate.1-0.36775031*TransitPMT+ 0.50890281*GHG.1
  return(GHG)
}

GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1); GHG.pred<-GHG.pred[-1]
GHG.pred.mean.Unemployment <- mean(GHG.pred)
GHG.low.Unemployment<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
GHG.high.Unemployment<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
GHG.difference.in.mean.unemployment <- abs(GHG.pred.mean.Unemployment - GHG.pred.mean.none)

#transitPMT * 0
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 0.02571576-0.32966013*Unemployment.Rate.1-0*TransitPMT+ 0.50890281*GHG.1
  return(GHG)
}

GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1); GHG.pred<-GHG.pred[-1]
GHG.pred.mean.PMT <- mean(GHG.pred)
GHG.low.PMT<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
GHG.high.PMT<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
GHG.difference.in.mean.PMT <- abs(GHG.pred.mean.none - GHG.pred.mean.PMT)

#ghg(t-1) * 0 
GHG<-function(Unemployment.Rate.1, TransitPMT, GHG.1){
  GHG<- 0.02571576-0.32966013*Unemployment.Rate.1-0.36775031*TransitPMT+ 0*GHG.1
  return(GHG)
}

GHG.pred<-GHG(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransitPMT, GHG.dat.sub$GHG.1); GHG.pred<-GHG.pred[-1]
GHG.pred.mean.t1 <- mean(GHG.pred)
GHG.low.t1<-mean(GHG.pred)-2*sd(GHG.pred)/length(GHG.pred)
GHG.high.t1<-mean(GHG.pred)+2*sd(GHG.pred)/length(GHG.pred)
GHG.difference.in.mean.t1 <- abs(GHG.pred.mean.none - GHG.pred.mean.t1)

#table F-2
GHG.Sensitvity <- data.frame(coefficent_variable_change = c('None', 'Unemployment Coeff x 0', 'TransitPMT Coeff x 0', 'GHG(t-1) Coeff x 0'),
                             Coefficent_estimate = c('None', -0.32966013, -0.36775031, 0.50890281),
                             Predicted_mean = c(GHG.pred.mean.none, GHG.pred.mean.Unemployment, GHG.pred.mean.PMT, GHG.pred.mean.t1),
                             Predicted_lower_mean_plus_2sd = c(GHG.low.none, GHG.low.Unemployment, GHG.low.PMT, GHG.low.t1),
                             Predicted_upper_mean_plus_2sd = c(GHG.high.none, GHG.high.Unemployment, GHG.high.PMT, GHG.high.t1),
                             Abs_mean_difference_from_None = c(GHG.difference.in.mean.none, GHG.difference.in.mean.unemployment,  GHG.difference.in.mean.PMT, GHG.difference.in.mean.t1)
)
GHG.Sensitvity




#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])

#coefficents from final model (Table 11 of report):
#TransitUPT : 0.55820411
# TMS(t-1) : 0.24366100

#No Coefficent/variable change######################

#TMS function from best model - lasso with VMT(t-1)
TMS<-function(TransitUPT, TMS.1){
  TMS<- 0.06859487+0.55820411*TransitUPT + 0.24366100*TMS.1
  return(TMS)
}

TMS.pred<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1); TMS.pred<-TMS.pred[-1]
TMS.pred.mean.none <- mean(TMS.pred)
TMS.low.none<-mean(TMS.pred)-2*sd(TMS.pred)/length(TMS.pred)
TMS.high.none<-mean(TMS.pred)+2*sd(TMS.pred)/length(TMS.pred)
Change.in.mean.None <- abs(TMS.pred.mean.none - TMS.pred.mean.none)


#TransitUPT x coeff * 0#############################
TMS<-function(TransitUPT, TMS.1){
  TMS<- 0.06859487+0*TransitUPT + 0.24366100*TMS.1
  return(TMS)
}

TMS.pred<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1); TMS.pred<-TMS.pred[-1]
TMS.pred.mean.UPT <- mean(TMS.pred)
TMS.low.UPT<-mean(TMS.pred)-2*sd(TMS.pred)/length(TMS.pred)
TMS.high.UPT<-mean(TMS.pred)+2*sd(TMS.pred)/length(TMS.pred)
Change.in.mean.UPT <- abs(TMS.pred.mean.none - TMS.pred.mean.UPT)

#TMS(t-1) x coeff * 0#############################
TMS<-function(TransitUPT, TMS.1){
  TMS<- 0.06859487+0.55820411*TransitUPT + 0*TMS.1
  return(TMS)
}


TMS.pred<-TMS(TMS.dat.sub$TransitUPT, TMS.dat.sub$TMS.1); TMS.pred<-TMS.pred[-1]
TMS.pred.mean.TMS <- mean(TMS.pred)
TMS.low.TMS<-mean(TMS.pred)-2*sd(TMS.pred)/length(TMS.pred)
TMS.high.TMS<-mean(TMS.pred)+2*sd(TMS.pred)/length(TMS.pred)
Change.in.mean.TMS <- abs(TMS.pred.mean.none - TMS.pred.mean.TMS)

#Table F-3
TMS.Sensitvity <- data.frame(coefficent_variable_change = c('None', 'TransitUPT Coeff x 0', 'TMS(t-1) Coeff x 0'),
                             Coefficent_estimate = c('None', 0.55820411, 0.24366100),
                             Predicted_mean = c(TMS.pred.mean.none, TMS.pred.mean.UPT, TMS.pred.mean.TMS),
                             Predicted_lower_mean_plus_2sd = c(TMS.low.none, TMS.low.UPT, TMS.low.TMS),
                             Predicted_upper_mean_plus_2sd = c(TMS.high.none, TMS.high.UPT, TMS.high.TMS),
                             Abs_mean_difference_from_None = c(Change.in.mean.None, Change.in.mean.UPT,  Change.in.mean.TMS)
                             )
TMS.Sensitvity
