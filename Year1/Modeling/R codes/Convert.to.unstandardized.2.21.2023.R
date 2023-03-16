setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#VMT ##############################################################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,c(2,5,15,18:19,27:28,31,33,36,37)]
VMT.dat.s<-as.data.frame(VMT.dat.s)
VMT.dat.sub<-VMT.dat.s[,c(1,2,3,7,10,11)]

#1 unit change in unemployment
one<-abs( (5-mean(VMT.dat$Unemployment.Rate.1))/sd(VMT.dat$Unemployment.Rate.1)
- (4-mean(VMT.dat$Unemployment.Rate.1))/sd(VMT.dat$Unemployment.Rate.1))
delta.y<- -0.3763*one
y1<-VMT.dat.sub$VMT[1]
y2<-delta.y+y1
y1.back<-y1*sd(VMT.dat$VMT)+mean(VMT.dat$VMT)
y2.back<-y2*sd(VMT.dat$VMT)+mean(VMT.dat$VMT)
y2.back-y1.back

one<-abs( (5000-mean(VMT.dat$EVPrivateandPublicChargingStationLocations))/sd(VMT.dat$EVPrivateandPublicChargingStationLocations)
          - (4000-mean(VMT.dat$EVPrivateandPublicChargingStationLocations))/sd(VMT.dat$EVPrivateandPublicChargingStationLocations))
delta.y<- -.6502*one
y1<-VMT.dat.sub$VMT[1]
y2<-delta.y+y1
y1.back<-y1*sd(VMT.dat$VMT)+mean(VMT.dat$VMT)
y2.back<-y2*sd(VMT.dat$VMT)+mean(VMT.dat$VMT)
y2.back-y1.back


one<-abs( (.036-mean(VMT.dat$populationTelework))/sd(VMT.dat$populationTelework)
          - (.035-mean(VMT.dat$populationTelework))/sd(VMT.dat$populationTelework))
delta.y<- 1.1365*one
y1<-VMT.dat.sub$VMT[1]
y2<-delta.y+y1
y1.back<-y1*sd(VMT.dat$VMT)+mean(VMT.dat$VMT)
y2.back<-y2*sd(VMT.dat$VMT)+mean(VMT.dat$VMT)
y2.back-y1.back



par(mfrow=c(1,1))
plot(VMT.dat.sub$Unemployment.Rate.1, VMT.dat.sub$VMT)
plot(VMT.dat$Unemployment.Rate.1, VMT.dat$VMT)

plot(VMT.dat$TotLaneMiles, VMT.dat$VMT)
plot(VMT.dat.sub$TotLaneMiles, VMT.dat.sub$VMT)

plot(VMT.dat$TransitPMT, VMT.dat$VMT)
plot(VMT.dat.sub$TransitPMT, VMT.dat.sub$VMT)

plot(VMT.dat$EVPrivateandPublicChargingStationLocations, VMT.dat$VMT)
plot(VMT.dat.sub$EVPrivateandPublicChargingStationLocations, VMT.dat.sub$VMT)

plot(VMT.dat$populationTelework, VMT.dat$VMT)
plot(VMT.dat.sub$populationTelework, VMT.dat.sub$VMT)




#GHG###############################################################################################################
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,c(3,5,15,18:19,27:28,31,33,36,37)]
GHG.dat.s<-as.data.frame(GHG.dat.s)
GHG.dat.sub<-GHG.dat.s[,c(1,2,3,7,10,11)]
GHG.dat.sub$GHG.1<-c(NA,GHG.dat.sub[1:19,1])

#1 unit change in unemployment
one<-abs( (5-mean(GHG.dat$Unemployment.Rate.1))/sd(GHG.dat$Unemployment.Rate.1)
          - (4-mean(GHG.dat$Unemployment.Rate.1))/sd(GHG.dat$Unemployment.Rate.1))
delta.y<- -0.2535*one
y1<-GHG.dat.sub$TransportGHG[1]
y2<-delta.y+y1
y1.back<-y1*sd(GHG.dat$TransportGHG)+mean(GHG.dat$TransportGHG)
y2.back<-y2*sd(GHG.dat$TransportGHG)+mean(GHG.dat$TransportGHG)
y2.back-y1.back

one<-abs( (4.7e+10-mean(GHG.dat$TransitPMT))/sd(GHG.dat$TransitPMT)
          - (4.6e+10-mean(GHG.dat$TransitPMT))/sd(GHG.dat$TransitPMT))
delta.y<- 0.4881*one
y1<-GHG.dat.sub$TransportGHG[1]
y2<-delta.y+y1
y1.back<-y1*sd(GHG.dat$TransportGHG)+mean(GHG.dat$TransportGHG)
y2.back<-y2*sd(GHG.dat$TransportGHG)+mean(GHG.dat$TransportGHG)
y2.back-y1.back

par(mfrow=c(1,1))
plot(GHG.dat.sub$Unemployment.Rate.1, GHG.dat.sub$TransportGHG)
plot(GHG.dat$Unemployment.Rate.1, GHG.dat$TransportGHG)

plot(GHG.dat.sub$TransitPMT, GHG.dat.sub$TransportGHG)
plot(GHG.dat$TransitPMT, GHG.dat$TransportGHG)




#TMS###############################################################################################################
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-TMS.dat.s[,c(1:2,12,15,16,24,25,28,30,33,34)]
TMS.dat.s<-as.data.frame(TMS.dat.s)
TMS.dat.sub<-TMS.dat.s[,c(1,2,5,8,9,11)]
TMS.dat.sub$TMS.1<-c(NA,TMS.dat.sub[1:15,1])

one<-abs( (9.1e+09-mean(GHG.dat$TransitUPT))/sd(GHG.dat$TransitUPT)
          - (9.2e+09-mean(GHG.dat$TransitUPT))/sd(GHG.dat$TransitUPT))
delta.y<- 0.5582*one
y1<-TMS.dat.sub$TransitModeShare[1]
y2<-delta.y+y1
y1.back<-y1*sd(TMS.dat$TransitModeShare)+mean(TMS.dat$TransitModeShare)
y2.back<-y2*sd(TMS.dat$TransitModeShare)+mean(TMS.dat$TransitModeShare)
y2.back-y1.back


plot(TMS.dat.sub$TransitUPT, TMS.dat.sub$TransitModeShare)
plot(TMS.dat$TransitUPT, TMS.dat$TransitModeShare)
