#This script creates the performance metric 1.5 models wtihin Step 5 of the Trends Indicators Memorandum,
#as well as their sensitivity analysis

##### Setup #####

#clean space
remove(list=ls())

# Loading the library
library(glmnet)
library(magrittr)
library(tidyr)
library(dplyr)
library(tidyverse)


#load the data
dp<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data 20230307 - DataP.csv", header=TRUE)
df<-read.csv("C:/Users/zapate/Documents/FHWA-Mobility-Trends-main/FHWA-Mobility-Trends-main/Year1/Modeling/Data/Data 20230307 - DataF.csv", header=TRUE)

summary(dp)
summary(df)


##### 1) VMT ###### -table 13

# Variables: VMT and X's
y_var = dp[,2]
x_vars = dp[,-c(1:5,11)]; #x_vars=x_vars[,-5]

lambda_seq=10^seq(20,5,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

#Coef estimates of VMT forecast (Step 5: Table 16)
coef(lasso_best)
VMT_coef <- coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)


#Coefficents:
Intercept.coef <- coef(lasso_best)[1]
GDP.coef <- coef(lasso_best)[3]
Unemployment.coef <- coef(lasso_best)[4]
TransitPMT.coef <- coef(lasso_best)[6]
Populationtelework.coef <- coef(lasso_best)[7]

concat_df = rbind(dp[c('Year', 'GDP', 'Unemployment.Rate.1', 'TransitPMT', 'X.populationTelework' )],
                  df[c('Year', 'GDP', 'Unemployment.Rate.1', 'TransitPMT', 'X.populationTelework')])


#sensitivity of GDP
df_vmt <- concat_df[-c(2:100)]
df_vmt['GDP'] <- concat_df['GDP'] * GDP.coef
df_vmt['Unemployment'] <- concat_df['Unemployment.Rate.1'] * Unemployment.coef
df_vmt['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef
df_vmt['Population_telework'] <- concat_df['X.populationTelework'] * Populationtelework.coef
df_vmt['Intercept'] <- Intercept.coef

#updating 2020 - 2022 as 0 since we do not model with them but are past current date for forecasting
df_vmt[21, 6] <- 0
df_vmt[22, 6] <- 0
df_vmt[23, 6] <- 0


#figure 24 in Appendix G
df_vmt %>% 
  pivot_longer(-Year, names_to = "type") %>% 
  ggplot(aes(Year, value, fill = type)) + geom_col() + labs(x = 'Year', y = 'VMT')


#sensitivity of GDP
df_VMT_sensitivity <- concat_df[-c(2:100)]
df_VMT_sensitivity['GDP'] <- concat_df['GDP'] * GDP.coef * 1.01
df_VMT_sensitivity['Unemployment'] <- concat_df['Unemployment.Rate.1'] * Unemployment.coef
df_VMT_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef
df_VMT_sensitivity['Population_telework'] <- concat_df['X.populationTelework'] * Populationtelework.coef
df_VMT_sensitivity['Intercept'] <- Intercept.coef

GDP_sensitivity <- ((df_VMT_sensitivity[19,2] + df_VMT_sensitivity[19,3] + df_VMT_sensitivity[19,4] + df_VMT_sensitivity[19,5] + df_VMT_sensitivity[19,6]) - 
                          (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])) / (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])
GDP_sensitivity

#sensitivity of Unemployment
df_VMT_sensitivity <- concat_df[-c(2:100)]
df_VMT_sensitivity['GDP'] <- concat_df['GDP'] * GDP.coef 
df_VMT_sensitivity['Unemployment'] <- (concat_df['Unemployment.Rate.1'] + 1) * Unemployment.coef
df_VMT_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef
df_VMT_sensitivity['Population_telework'] <- concat_df['X.populationTelework'] * Populationtelework.coef
df_VMT_sensitivity['Intercept'] <- Intercept.coef

unemploy_sensitivity <- ((df_VMT_sensitivity[19,2] + df_VMT_sensitivity[19,3] + df_VMT_sensitivity[19,4] + df_VMT_sensitivity[19,5] + df_VMT_sensitivity[19,6]) - 
                      (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])) / (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])
unemploy_sensitivity

#sensitivity of transit
df_VMT_sensitivity <- concat_df[-c(2:100)]
df_VMT_sensitivity['GDP'] <- concat_df['GDP'] * GDP.coef 
df_VMT_sensitivity['Unemployment'] <- concat_df['Unemployment.Rate.1'] * Unemployment.coef 
df_VMT_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef * 1.01
df_VMT_sensitivity['Population_telework'] <- concat_df['X.populationTelework'] * Populationtelework.coef
df_VMT_sensitivity['Intercept'] <- Intercept.coef

transit_sensitivity <- ((df_VMT_sensitivity[19,2] + df_VMT_sensitivity[19,3] + df_VMT_sensitivity[19,4] + df_VMT_sensitivity[19,5] + df_VMT_sensitivity[19,6]) - 
                           (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])) / (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])
transit_sensitivity

#sensitivity of pop telework%
df_VMT_sensitivity <- concat_df[-c(2:100)]
df_VMT_sensitivity['GDP'] <- concat_df['GDP'] * GDP.coef 
df_VMT_sensitivity['Unemployment'] <- concat_df['Unemployment.Rate.1'] * Unemployment.coef 
df_VMT_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef
df_VMT_sensitivity['Population_telework'] <- (concat_df['X.populationTelework'] + .01) * Populationtelework.coef 
df_VMT_sensitivity['Intercept'] <- Intercept.coef

telework_sensitivity <- ((df_VMT_sensitivity[19,2] + df_VMT_sensitivity[19,3] + df_VMT_sensitivity[19,4] + df_VMT_sensitivity[19,5] + df_VMT_sensitivity[19,6]) - 
                           (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])) / (df_vmt[19,2] + df_vmt[19,3] + df_vmt[19,4] + df_vmt[19,5] + df_vmt[19,6])
telework_sensitivity

#vmt sensitivity results -- Table 17
data.frame(indicators = c('GDP', 'Unemployment', 'TransitPMT', 'PopulationTelework%'),
           sensitivity = c(GDP_sensitivity, unemploy_sensitivity, transit_sensitivity, telework_sensitivity))



##### 2) GHG ######

# Variables: GHG and X's
y_var = dp[,4]
x_vars = dp[,-c(1:5,11)]; #x_vars=x_vars[,-5]

lambda_seq=10^seq(10,-5,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)
GHG_coef <- coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#Coefficents:
Intercept.coef <- GHG_coef[1]
Unemployment.coef <- GHG_coef[4]
Transit.coef <- GHG_coef[6]


concat_df = rbind(dp[c('Year','Unemployment.Rate.1', 'TransitPMT' )],
                  df[c('Year','Unemployment.Rate.1', 'TransitPMT' )])

df_GHG <- concat_df[-c(2:100)]
df_GHG['TransitPMT'] <- concat_df['TransitPMT'] * Transit.coef
df_GHG['Unemployment.Rate.1'] <- concat_df['Unemployment.Rate.1'] * Unemployment.coef
df_GHG['Intercept'] <- Intercept.coef

df_GHG[21, 4] <- 0
df_GHG[22, 4] <- 0
df_GHG[23, 4] <- 0

#sensitivity of transitPMT
df_ghg_sensitivity <- concat_df[-c(2:100)]
df_ghg_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * Transit.coef * 1.01
df_ghg_sensitivity['Unemployment.Rate.1'] <- concat_df['Unemployment.Rate.1'] * Unemployment.coef
df_ghg_sensitivity['Intercept'] <- Intercept.coef

transit_sensitivity <- ((df_ghg_sensitivity[19,2] + df_ghg_sensitivity[19,3] + df_ghg_sensitivity[19,4]) - 
                           (df_GHG[19,2] + df_GHG[19,3] + df_GHG[19,4])) / (df_GHG[19,2] + df_GHG[19,3] + df_GHG[19,4])
transit_sensitivity

#sensitivity of Unemployemnt
df_ghg_sensitivity <- concat_df[-c(2:100)]
df_ghg_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * Transit.coef
df_ghg_sensitivity['Unemployment.Rate.1'] <- (concat_df['Unemployment.Rate.1'] + 1) * Unemployment.coef
df_ghg_sensitivity['Intercept'] <- Intercept.coef

unemploy_sensitivity <- ((df_ghg_sensitivity[19,2] + df_ghg_sensitivity[19,3] + df_ghg_sensitivity[19,4]) - 
                           (df_GHG[19,2] + df_GHG[19,3] + df_GHG[19,4])) / (df_GHG[19,2] + df_GHG[19,3] + df_GHG[19,4])
unemploy_sensitivity

#coefficent results - Table 18
GHG_coef

#sensitivity results table 19
data.frame(indicator = c('Unemployment', 'TransitPMT'),
           value = c(unemploy_sensitivity, transit_sensitivity))

#decomposition of GHG, Appendix G, Figure 25
df_GHG %>% 
  pivot_longer(-Year, names_to = "type") %>% 
  ggplot(aes(Year, value, fill = type)) + geom_col() + labs(x = 'Year', y = 'GHG - MMT CO2 eq.')





##### 3) TMS ######

# Variables: TMS and X's
y_var = dp[-c(1:4),3]
x_vars = dp[-c(1:4),-c(1:5)]; #x_vars=x_vars[,-5]

lambda_seq=10^seq(5,-10,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 14)
train = 1:14
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

TMS_coef <- coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)


#Coefficents:
Intercept.coef <- coef(lasso_best)[1]
TransitPMT.coef <- coef(lasso_best)[6]

concat_df = rbind(dp[c('Year', 'TransitPMT' )],
                  df[c('Year', 'TransitPMT' )])

df_TMS <- concat_df[-c(2:100)]
df_TMS['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef
df_TMS['Intercept'] <- Intercept.coef

df_TMS[21, 3] <- 0
df_TMS[22, 3] <- 0
df_TMS[23, 3] <- 0


df_tms_sensitivity <- concat_df[-c(2:100)]
df_tms_sensitivity['TransitPMT'] <- concat_df['TransitPMT'] * TransitPMT.coef * 1.01
df_tms_sensitivity['Intercept'] <- Intercept.coef

#df_tms_sensitivity[1, 2] <- df_tms_sensitivity[1, 2] * 1.01
GDP_sensitivity <- ((df_tms_sensitivity[19,2] + df_tms_sensitivity[19,3]) - (df_TMS[19,2] + df_TMS[19,3])) / (df_TMS[19,2] + df_TMS[19,3])

#decomposition chart, Appendix G Figure 26
df_TMS%>% 
  pivot_longer(-Year, names_to = "type") %>% 
  ggplot(aes(Year, value, fill = type)) + geom_col() + labs(x = 'Year', y = 'Transit Share %')


#TMS Coefficents table 20 of report
TMS_coef

#TMS sensitivity table 21 of report
data.frame(coefficent = c('TransitPMT'),
           sensitivity = c(GDP_sensitivity))


