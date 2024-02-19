library(car)
library(caret)
library(tidyr)
library(glmnet) #documentation at: https://glmnet.stanford.edu/articles/glmnet.html
library(corrplot)
library(ggplot2)
library(tidyverse)

#################################### Inputs to edit #################################
#clear workspace
rm(list = setdiff(ls(), lsf.str()))

setwd("C:/Users/hrowe/Documents/FHWA mobility trend report/T4 - Forecasting/Year 2/FHWA_Mobility_Trends_Updatey2_2_19_2024/Year 2/") 

#load in the data you want to use 
df <- read.csv(paste(dirname(getwd()),"Year 2/2. Modeling/Data/T6_county_2_19_24.csv", sep = '/'))

##### Final VMT Model (run on 1/8/24)
#set the list of indicators you want to work with
indicators <- c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES","TELEWORK","UPT_distr_commuters", "COURIER_NONEMP_RCPTOT_REAL", "DRIVER_NONEMP_RCPTOT_REAL", "POP_DENSITY")
non_indicator_var <- c("Full_FIPS_Code", "YEAR", "County_Type")
y <-  "VMT" #set your dependent variable #"TOTAL_EMISSIONS", "TMS"
train_percent <- 0.7 #set the percent of data you want to use in the train set
n = 1 #define number of model trials you want to run
remove_y_zeros <- TRUE #(set to FLASE if preferred to keep zeros in)

##### VMT Model for Task 6 (ondemand) (run on 2/19/24)
#set the list of indicators you want to work with
indicators <- c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES","TELEWORK","UPT_distr_commuters", "COURIER_NONEMP_RCPTOT_REAL", "DRIVER_NONEMP_RCPTOT_REAL", "POP_DENSITY")
non_indicator_var <- c("Full_FIPS_Code", "YEAR", "County_Type")
y <-  "VMT" #set your dependent variable #"TOTAL_EMISSIONS", "TMS"
train_percent <- 0.7 #set the percent of data you want to use in the train set
n = 1 #define number of model trials you want to run
remove_y_zeros <- TRUE #(set to FLASE if preferred to keep zeros in)

#################################### Structure #################################

#setting seed value
set.seed(102)

#grab data and drop all NA 
df <- drop_na(df[,c(indicators, y, non_indicator_var)])

#remove cases where y is zero, depending on parameter given in the inputs section
if(remove_y_zeros){
  df <- df %>% 
    filter(!!sym(y) != 0) #replace VMT with y if we want this to work for all performance measures
}

# make copy of year and for later
df_year <- subset(df, select=c(YEAR))
df_county_type <- subset(df, select=c(County_Type))

#one_hot encode data
df$Full_FIPS_Code <- as.factor(df$Full_FIPS_Code)
dum <- dummyVars(" ~ Full_FIPS_Code", data = df)
counties_onehot <- data.frame(predict(dum, newdata = df))
df <- cbind(df, counties_onehot)

#log convert all data (even rates, as it helps with skew and uniformity of data)
all_var <- c(y, indicators)
log_var <- paste("LOG", all_var, sep ="")
log_y <- log_var[1]
for (a in all_var){
  temp <- log(df[[a]] + 1)
  title <- paste("LOG", a, sep ="")
  df[title] <- temp
}

#removing non log transformed indicators and performance measure
df <- df[ , -which(names(df) %in% c(indicators, y))]

# Min-max scaling the data 
process <- preProcess(as.data.frame(df), method=c("range"))
norm_scale <- predict(process, as.data.frame(df))

#adding back year, county type, and pre-min max scaled performance measure
norm_scale[["YEAR"]] <- as.numeric(df[["YEAR"]])
norm_scale[["County_Type"]] <- df[["County_Type"]]
norm_scale[[log_y]] <- df[[log_y]]

#initializing df to hold results from n trials
var_and_eval <- c(log_var[-1], "r2_test", "rmse_norm_test")
df_n_trials <- data.frame(matrix(ncol = length(var_and_eval), nrow = 0))
colnames(df_n_trials) <- var_and_eval

smp_size <- floor(train_percent * nrow(norm_scale))
train_ind <- sample(seq_len(nrow(norm_scale)), size = smp_size)
train <- norm_scale[train_ind, ]
test <- norm_scale[-train_ind, ]

lasso_x1 <- as.matrix(subset(train, select = -c(LOGVMT,Full_FIPS_Code, YEAR, County_Type)))
#lasso_x <- as.matrix(select(train, -one_of(c("LOGVMT", non_indicator_var))))
lasso_x <- as.matrix(select(train, -one_of(c(log_y, non_indicator_var))))

#lasso_y <- as.matrix(subset(train, select = c(LOGVMT)))
lasso_y <- as.matrix(select(train, log_y))

#read best model pick correct model
best_model <- read_rds(paste(dirname(getwd()),"/Modeling/champion_VMT_y2_2_16_24.rds", sep = ''))


#sensitivity analysis before and after log values
df <- read.csv(paste(dirname(getwd()),"/Data/County_Year2_2_16_2024.csv", sep = ''))
df <- drop_na(df[,c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES",
                    "TELEWORK","UPT_distr_commuters", "COURIER_NONEMP_RCPTOT_REAL", "DRIVER_NONEMP_RCPTOT_REAL", "POP_DENSITY", "VMT", "Full_FIPS_Code", "YEAR")]) #removed year and gems geotype
df <- df[!(df$VMT == 0),]
df_year <- subset(df, select=c(YEAR))
df$Full_FIPS_Code <- as.factor(df$Full_FIPS_Code)
counties_onehot <- data.frame(predict(dum, newdata = df))
df <- cbind(df, counties_onehot)


#processing function
sensitivty_processs <- function(data_point_sensitivity){
  
  #log convert all data (even unemploymet and telework, as it helps with skew and uniformity of data)
  data_point_sensitivity$LOGVMT <- log(data_point_sensitivity$VMT + 1)
  data_point_sensitivity$LOGPOPULATION <- log(data_point_sensitivity$POPULATION + 1)
  data_point_sensitivity$LOGTrue_GDP <- log(data_point_sensitivity$True_GDP + 1)
  data_point_sensitivity$LOGLNMILES <- log(data_point_sensitivity$LNMILES + 1)
  data_point_sensitivity$LOGCharging_Stations <- log(data_point_sensitivity$Charging_Stations+1)
  data_point_sensitivity$LOGUPT_distr_commuters <-log(data_point_sensitivity$UPT_distr_commuters + 1)
  data_point_sensitivity$LOGUnemployment_Rate <- log(data_point_sensitivity$Unemployment_Rate + 1)
  data_point_sensitivity$LOGTELEWORK <- log(data_point_sensitivity$TELEWORK + 1)
  data_point_sensitivity$LOGCOURIER_NONEMP_RCPTOT_REAL  <- log(data_point_sensitivity$COURIER_NONEMP_RCPTOT_REAL + 1)
  data_point_sensitivity$LOGDRIVER_NONEMP_RCPTOT_REAL  <- log(data_point_sensitivity$DRIVER_NONEMP_RCPTOT_REAL + 1)
  data_point_sensitivity$LOGPOP_DENSITY <- log(data_point_sensitivity$POP_DENSITY + 1)
  
  data_point_sensitivity <- subset(data_point_sensitivity, select = -c(VMT, POPULATION, True_GDP, LNMILES, Charging_Stations, UPT_distr_commuters, Unemployment_Rate, TELEWORK, COURIER_NONEMP_RCPTOT_REAL, DRIVER_NONEMP_RCPTOT_REAL, POP_DENSITY))
  
  #data_point_sensitivity <- predict(process, select(data_point_sensitivity, -c(LOGVMT, Full_FIPS_Code, YEAR )))
  data_point_sensitivity <- predict(process, data_point_sensitivity)
  
  #data_point_sensitivity <- subset(data_point_sensitivity, select = -c(VMT, POPULATION, True_GDP, LNMILES, Charging_Stations, UPT, Unemployment_Rate, TELEWORK))
  data_point_sensitivity <- subset(data_point_sensitivity, select = -c(LOGVMT, YEAR, Full_FIPS_Code))
  
  preds <- predict(best_model, as.matrix(data_point_sensitivity[colnames(lasso_x1)]))
  data_point_sensitivity$logpreds <- preds
  data_point_sensitivity$Predictions <- exp(data_point_sensitivity$logpreds)
  sensitivity100 <-  100 *(data_point_sensitivity$Predictions[2] - data_point_sensitivity$Predictions[1]) / data_point_sensitivity$Predictions[1]
  
  return(sensitivity100)
}
  
  
#Pop
quantile_values <- quantile(df$POPULATION) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$POPULATION)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$POPULATION <- percent_100_data_point_adjusted$POPULATION * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
Pop_sensitivity <- sensitivty_processs(data_point_sensitivity)


#GDP
quantile_values <- quantile(df$True_GDP) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$True_GDP)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$True_GDP <- percent_100_data_point_adjusted$True_GDP * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
GDP_sensitivity <- sensitivty_processs(data_point_sensitivity)


#Unemployment
quantile_values <- quantile(df$Unemployment_Rate) 
percent_100_data_point <- df[which.min(abs(quantile_values[2] - df$Unemployment_Rate)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$Unemployment_Rate <- percent_100_data_point_adjusted$Unemployment_Rate + .01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
Unemp_sensitivity <- sensitivty_processs(data_point_sensitivity)

#Charging Station
quantile_values <- quantile(df$Charging_Stations) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$Charging_Stations)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$Charging_Stations <- percent_100_data_point_adjusted$Charging_Stations * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
charg_sensitivity <- sensitivty_processs(data_point_sensitivity)

#Lane Miles
quantile_values <- quantile(df$LNMILES) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$LNMILES)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$LNMILES <- percent_100_data_point_adjusted$LNMILES * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
lnmiles_sensitivity <- sensitivty_processs(data_point_sensitivity)

#Telework
quantile_values <- quantile(df$TELEWORK) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$TELEWORK)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$TELEWORK <- percent_100_data_point_adjusted$TELEWORK + .01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
tele_sensitivity <- sensitivty_processs(data_point_sensitivity)

#UPT
quantile_values <- quantile(df$UPT_distr_commuters) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$UPT_distr_commuters)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$UPT_distr_commuters <- percent_100_data_point_adjusted$UPT_distr_commuters * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
upt_sensitivity <- sensitivty_processs(data_point_sensitivity)

#Courier
quantile_values <- quantile(df$COURIER_NONEMP_RCPTOT_REAL) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$COURIER_NONEMP_RCPTOT_REAL)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$COURIER_NONEMP_RCPTOT_REAL <- percent_100_data_point_adjusted$COURIER_NONEMP_RCPTOT_REAL * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
courier_sensitivity <- sensitivty_processs(data_point_sensitivity)

#Driver rev
quantile_values <- quantile(df$DRIVER_NONEMP_RCPTOT_REAL) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$DRIVER_NONEMP_RCPTOT_REAL)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$DRIVER_NONEMP_RCPTOT_REAL <- percent_100_data_point_adjusted$DRIVER_NONEMP_RCPTOT_REAL * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
driver_sensitivity <- sensitivty_processs(data_point_sensitivity)

#Pop density
quantile_values <- quantile(df$POP_DENSITY) 
percent_100_data_point <- df[which.min(abs(quantile_values[5] - df$POP_DENSITY)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$POP_DENSITY <- percent_100_data_point_adjusted$POP_DENSITY * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
density_sensitivity <- sensitivty_processs(data_point_sensitivity)


sensitivity_analysis <- data.frame(indicators = c("Population","GDP", "Unemployment Rate", "Charging Stations", "LNMILES", "TELEWORK", "UPT", "Courier", "Driver", "POP density"),
                                   percent_change = c(Pop_sensitivity,
                                                      GDP_sensitivity,
                                                      Unemp_sensitivity,
                                                      charg_sensitivity,
                                                      lnmiles_sensitivity,
                                                      tele_sensitivity,
                                                      upt_sensitivity,
                                                      courier_sensitivity,
                                                      driver_sensitivity,
                                                      density_sensitivity
                                                      ))

sensitivity_analysis
write.csv(sensitivity_analysis, "sensitivity_analysis_mixed_effectsVMT.csv", row.names=FALSE)



