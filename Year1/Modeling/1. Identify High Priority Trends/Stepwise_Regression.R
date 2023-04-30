
# 
# This script is used to run stepwise regression on the 15 trend indicators. This was done to narrow down the 
# list of trends from 15 to 5 in order to begin the task 4 modeling. 

#install.packages("openxlsx")
#install.packages("Hmisc")

library(openxlsx);library(Hmisc); library(olsrr);library(pls); library(MASS); library(tidyverse); library(caret); library(leaps)

#read data
data = read.xlsx("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/FiveTrendsDataT.xlsx")
datan = data
y = datan[2:4]
x = datan[5:19]

for (i in c(1:15)) {
  x[,i]=scale(x[,i])
}

for (i in c(1:3)) {
  y[,i]=scale(y[,i])
}

#=============================== 
#set up model for selected performance metric
full.model <- lm(y$VMT ~., data = x[,-11]); summary(full.model)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE); summary(step.model)
models <- regsubsets(y$VMT ~., data = x, nvmax = 3,method = "seqrep"); summary(models)

#===============================
#perform stepwise regression on performance metric
intercept_only <- lm(y$VMT ~ 1, data=x[,-11])
all <- lm(y$VMT ~ ., data=x[,-11])
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0); forward$anova; forward$coefficients

both <- step(intercept_only, direction='both', scope=formula(all), trace=0)
both$anova
both$coefficients

#=============================
#keep the coefficents layed out in line 39

model = glm(y$VMT ~ 
              x$UnemploymentRate +
              # x$GDP +
              x$PopDL +
              x$RegisterVeh +
              # x$SalesHybandElec +
              # x$CarSharingVeh +
              x$LaneMiles +
              x$TransitUPT ,
            # x$AdultsUseInternet +
            # x$PbPvEVStations +
            # x$WorkatHome +
            # x$TruckTonMi +
            # x$PercentUrban +
            # x$GasPrices,
            data=x, family=gaussian()); summary(model)

#=============================
#Ending coefficents of stepwise regresison. Related to Figure 1-2 in Trends Indiciator Memorandum
VMT = lm (y$VMT ~ x$UnemploymentRate + x$PopDL + x$RegisterVeh + x$LaneMiles + x$TransitUPT, data = x); summary(VMT) 
GHG = lm (y$GHG ~ x$CarSharingVeh + x$TransitUPT + x$UnemploymentRate, data = x); summary(GHG)
TMS = lm (y$TransitModeShare ~ x$TransitUPT + x$CarSharingVeh + x$UnemploymentRate, data = x); summary(TMS)
