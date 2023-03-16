#An example showing positive part #######################################
f<-function(x,b0,b1,b2,tau){
  b0+b1*x+b2*pmax(x-tau,0)
}

b0=0; b1=0.5; b2=1; tau=2
x1<-seq(0,tau,by=0.1)
y1<-f(x1,b0,b1,b2,tau)
x2<-seq(tau,5,by=0.1)
y2<-f(x2,b0,b1,b2,tau)

plot(x1,y1, type="l",xlim=c(0,max(x1,x2)), ylim=c(0,max(y1,y2)))
lines(x2,y2, type="l",xlim=c(0,max(x1,x2)), ylim=c(0,max(y1,y2)))

##########################################################################
setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

library(ggplot2)
library(segmented)

#VMT #####################################################################
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.sub<-VMT.dat[,c("year","VMT","Unemployment.Rate.1","TotLaneMiles","EVPrivateandPublicChargingStationLocations","populationTelework")]
VMT.dat.sub$VMT.1<-c(NA,VMT.dat.sub[1:19,"VMT"])

plot.unemp<-ggplot(VMT.dat.sub, aes(x = Unemployment.Rate.1, y=VMT))+geom_point()
lm.unemp<-lm(VMT ~ Unemployment.Rate.1, data=VMT.dat.sub)

seg.unemp<-segmented(lm.unemp, seg.Z = ~ Unemployment.Rate.1)
summary(seg.unemp)
slope(seg.unemp)

unemp.fitted<-fitted(seg.unemp)
unemp.model<-data.frame(Unemployment=VMT.dat.sub$Unemployment.Rate.1, VMT=unemp.fitted)
plot.unemp+geom_line(data = unemp.model, aes(x = Unemployment, y = VMT), colour="red")
