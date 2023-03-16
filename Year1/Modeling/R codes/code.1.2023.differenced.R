setwd("/Users/csuffaculty/Desktop/FHWA")

#read the dataset
dat<-read.csv("Data20221210.csv", header=TRUE)

#Separate data by each measure############################################################################# 
#VMT vs. others 
VMT.dat<-dat[,colSums(is.na(dat))==0]
VMT.dat.s<-scale(VMT.dat)
VMT.dat.s<-VMT.dat.s[,-c(1,3,4)]
VMT.dat.s<-as.data.frame(VMT.dat.s)

acf(VMT.dat.s[,1])
pacf(VMT.dat.s[,1])

#GHG vs. others 
GHG.dat<-dat[,colSums(is.na(dat))==0]
GHG.dat.s<-scale(GHG.dat)
GHG.dat.s<-GHG.dat.s[,-c(1,2,4)]
GHG.dat.s<-as.data.frame(GHG.dat.s)

acf(GHG.dat.s[,1])
pacf(GHG.dat.s[,1])

#TMS vs. others
TMS.dat<-dat[-c(1:4),-c(1,2,4,5)]
TMS.dat<-TMS.dat[,colSums(is.na(TMS.dat))==0]
TMS.dat.s<-scale(TMS.dat)
TMS.dat.s<-as.data.frame(TMS.dat.s)

acf(TMS.dat.s[,1])
pacf(TMS.dat.s[,1])
