
# FHWA Mobility Trends Year 2 Sensitivity Analysis - TMS
# Date: March 20, 2024
# Software Version: 1.1.0
# Creator Name: Helena Rowe and Zach Pate
# Summary: This script takes the champion models built for each performance metric
#          and creates the sensitivity analysis (1% change effect on performance metric).
# History of Modification:
#         1.1.0: Improved variable naming and comments


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

##### Final TMS Model (run on 2/16/24)
#load in the data you want to use 
df_all_data <- read.csv(paste(dirname(getwd()),"/Data/County_Year2_2_16_2024.csv", sep = ''))
#set the list of indicators you want to work with
indicators <- c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES","TELEWORK","capital_transit_funding", "COURIER_NONEMP_RCPTOT_REAL", "DRIVER_NONEMP_RCPTOT_REAL", "POP_DENSITY")
non_indicator_var <- c("Full_FIPS_Code", "YEAR", "County_Type")
performance_metric <-  "TRANSIT_USER_COUNT" #set your dependent variable ("TRANSIT_USER_COUNT", "TMS")
train_percent <- 0.7 #set the percent of data you want to use in the train set
n = 1 #define number of model trials you want to run
remove_y_zeros <- TRUE #(set to FALSE if preferred to keep zeros in)
#df <- df[!(df$distributed_UPT == 0),] #optional to remove 0s for UPT (remove counties without transit systems, assume only counties with transit systems have a relationship with TMS)
df_all_data <- df_all_data %>% filter(County_Type == 'Metropolitan') #creating a model only for metro counties

#################################### Structure #################################

#setting seed value
set.seed(102)

#grab data and drop all NA 
df_na_droped <- drop_na(df_all_data[,c(indicators, performance_metric, non_indicator_var)])

#remove cases where y is zero, depending on parameter given in the inputs section
if(remove_y_zeros){
  df_na_droped <- df_na_droped %>% 
    filter(!!sym(performance_metric) != 0) #replace VMT with y if we want this to work for all performance measures
}

# make copy of year and for later
df_year_subset <- subset(df_na_droped, select=c(YEAR))
df_county_type_subset <- subset(df_na_droped, select=c(County_Type))

#one_hot encode data
df_na_droped$Full_FIPS_Code <- as.factor(df_na_droped$Full_FIPS_Code)
dum_variables <- dummyVars(" ~ Full_FIPS_Code", data = df_na_droped)
counties_onehot <- data.frame(predict(dum_variables, newdata = df_na_droped))
df_one_hot_encoded <- cbind(df_na_droped, counties_onehot)

#log convert all data (even rates, as it helps with skew and uniformity of data)
all_var <- c(performance_metric, indicators)
log_var <- paste("LOG", all_var, sep ="")
log_y <- log_var[1]
for (a in all_var){
  temp <- log(df_one_hot_encoded[[a]] + 1)
  title <- paste("LOG", a, sep ="")
  df_one_hot_encoded[title] <- temp
}

#removing non log transformed indicators and performance measure
df_modeling <- df_one_hot_encoded[ , -which(names(df_one_hot_encoded) %in% c(indicators, performance_metric))]

# Min-max scaling the data 
process <- preProcess(as.data.frame(df_modeling), method=c("range"))
norm_scale <- predict(process, as.data.frame(df_modeling))


#adding back year, county type, and pre-min max scaled performance measure
norm_scale[["YEAR"]] <- as.numeric(df_one_hot_encoded[["YEAR"]])
norm_scale[["County_Type"]] <- df_one_hot_encoded[["County_Type"]]
norm_scale[[log_y]] <- df_one_hot_encoded[[log_y]]

#initializing df to hold results from n trials
var_and_eval <- c(log_var[-1], "r2_test", "rmse_norm_test")
df_n_trials <- data.frame(matrix(ncol = length(var_and_eval), nrow = 0))
colnames(df_n_trials) <- var_and_eval

smp_size <- floor(train_percent * nrow(norm_scale))
train_ind <- sample(seq_len(nrow(norm_scale)), size = smp_size)
train <- norm_scale[train_ind, ]
test <- norm_scale[-train_ind, ]

lasso_x1 <- subset(train, select = -c(LOGTRANSIT_USER_COUNT,Full_FIPS_Code, YEAR, County_Type))
lasso_x <- as.matrix(select(train, -one_of(c("LOGTRANSIT_USER_COUNT", non_indicator_var))))
lasso_x <- as.matrix(select(train, -one_of(c(log_y, non_indicator_var))))

#lasso_y <- as.matrix(subset(train, select = c(LOGVMT)))
lasso_y <- as.matrix(select(train, log_y))

#read best model pick correct model
best_model <- read_rds(paste(dirname(getwd()),"/Modeling/champion_TMS_y2_2_16_24.rds", sep = ''))





#sensitivity analysis before and after log values
df_sensitivity <- read.csv(paste(dirname(getwd()),"/Data/County_Year2_2_16_2024.csv", sep = ''))
df_sensitivity <- df_sensitivity %>% filter(County_Type == 'Metropolitan')
df_sensitivity_dropna <- drop_na(df_sensitivity[,c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES",
                    "TELEWORK","capital_transit_funding", "COURIER_NONEMP_RCPTOT_REAL", "DRIVER_NONEMP_RCPTOT_REAL", "POP_DENSITY", "TRANSIT_USER_COUNT", "Full_FIPS_Code","County_Type", "YEAR")]) #removed year and gems geotype
df_sensitivity_dropna <- df_sensitivity_dropna[!(df_sensitivity_dropna$TRANSIT_USER_COUNT == 0),]
df_year <- subset(df_sensitivity_dropna, select=c(YEAR))
df_sensitivity_dropna$Full_FIPS_Code <- as.factor(df_sensitivity_dropna$Full_FIPS_Code)
counties_onehot <- data.frame(predict(dum_variables, newdata = df))
df_sensitivity_one_hot <- cbind(df_sensitivity_dropna, counties_onehot)


#processing function
sensitivty_processs <- function(data_point_sensitivity, best_model){
  
  #log convert all data (even unemploymet and telework, as it helps with skew and uniformity of data)
  data_point_sensitivity$LOGTRANSIT_USER_COUNT <- log(data_point_sensitivity$TRANSIT_USER_COUNT + 1)
  data_point_sensitivity$LOGPOPULATION <- log(data_point_sensitivity$POPULATION + 1)
  data_point_sensitivity$LOGTrue_GDP <- log(data_point_sensitivity$True_GDP + 1)
  data_point_sensitivity$LOGLNMILES <- log(data_point_sensitivity$LNMILES + 1)
  data_point_sensitivity$LOGCharging_Stations <- log(data_point_sensitivity$Charging_Stations+1)
  data_point_sensitivity$LOGcapital_transit_funding <-log(data_point_sensitivity$capital_transit_funding + 1)
  data_point_sensitivity$LOGUnemployment_Rate <- log(data_point_sensitivity$Unemployment_Rate + 1)
  data_point_sensitivity$LOGTELEWORK <- log(data_point_sensitivity$TELEWORK + 1)
  data_point_sensitivity$LOGCOURIER_NONEMP_RCPTOT_REAL  <- log(data_point_sensitivity$COURIER_NONEMP_RCPTOT_REAL + 1)
  data_point_sensitivity$LOGDRIVER_NONEMP_RCPTOT_REAL  <- log(data_point_sensitivity$DRIVER_NONEMP_RCPTOT_REAL + 1)
  data_point_sensitivity$LOGPOP_DENSITY <- log(data_point_sensitivity$POP_DENSITY + 1)
  
  data_point_sensitivity <- subset(data_point_sensitivity, select = -c(TRANSIT_USER_COUNT, POPULATION, True_GDP, LNMILES, Charging_Stations, capital_transit_funding, Unemployment_Rate, TELEWORK, COURIER_NONEMP_RCPTOT_REAL, DRIVER_NONEMP_RCPTOT_REAL, POP_DENSITY))
  
  #data_point_sensitivity <- predict(process, select(data_point_sensitivity, -c(LOGVMT, Full_FIPS_Code, YEAR )))
  data_point_sensitivity <- predict(process, data_point_sensitivity)
  
  #data_point_sensitivity <- subset(data_point_sensitivity, select = -c(VMT, POPULATION, True_GDP, LNMILES, Charging_Stations, UPT, Unemployment_Rate, TELEWORK))
  data_point_sensitivity <- subset(data_point_sensitivity, select = -c(LOGTRANSIT_USER_COUNT, YEAR, Full_FIPS_Code))
  data_point_sensitivity <- subset(data_point_sensitivity, select = -c(County_Type))
  
  preds <- predict(best_model, as.matrix(data_point_sensitivity[colnames(lasso_x1)]))
  data_point_sensitivity$logpreds <- preds
  data_point_sensitivity$Predictions <- exp(data_point_sensitivity$logpreds)
  sensitivity100 <-  100 *(data_point_sensitivity$Predictions[2] - data_point_sensitivity$Predictions[1]) / data_point_sensitivity$Predictions[1]
  
  return(sensitivity100)
}
  
  
#Pop
quantile_values <- quantile(df_sensitivity_one_hot$POPULATION) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$POPULATION)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$POPULATION <- percent_100_data_point_adjusted$POPULATION * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
Pop_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)


#GDP
quantile_values <- quantile(df_sensitivity_one_hot$True_GDP) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$True_GDP)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$True_GDP <- percent_100_data_point_adjusted$True_GDP * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
GDP_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Unemployment
quantile_values <- quantile(df_sensitivity_one_hot$Unemployment_Rate) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$Unemployment_Rate)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$Unemployment_Rate <- percent_100_data_point_adjusted$Unemployment_Rate + .01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
Unemp_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Charging Station
quantile_values <- quantile(df_sensitivity_one_hot$Charging_Stations) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$Charging_Stations)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$Charging_Stations <- percent_100_data_point_adjusted$Charging_Stations * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
charg_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Lane Miles
quantile_values <- quantile(df_sensitivity_one_hot$LNMILES) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$LNMILES)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$LNMILES <- percent_100_data_point_adjusted$LNMILES * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
lnmiles_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Telework
quantile_values <- quantile(df_sensitivity_one_hot$TELEWORK) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$TELEWORK)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$TELEWORK <- percent_100_data_point_adjusted$TELEWORK + .01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
tele_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#UPT
quantile_values <- quantile(df_sensitivity_one_hot$capital_transit_funding) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$capital_transit_funding)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$capital_transit_funding <- percent_100_data_point_adjusted$capital_transit_funding * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
upt_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Courier
quantile_values <- quantile(df_sensitivity_one_hot$COURIER_NONEMP_RCPTOT_REAL) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$COURIER_NONEMP_RCPTOT_REAL)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$COURIER_NONEMP_RCPTOT_REAL <- percent_100_data_point_adjusted$COURIER_NONEMP_RCPTOT_REAL * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
courier_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Driver rev
quantile_values <- quantile(df_sensitivity_one_hot$DRIVER_NONEMP_RCPTOT_REAL) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$DRIVER_NONEMP_RCPTOT_REAL)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$DRIVER_NONEMP_RCPTOT_REAL <- percent_100_data_point_adjusted$DRIVER_NONEMP_RCPTOT_REAL * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
driver_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)

#Pop density
quantile_values <- quantile(df_sensitivity_one_hot$POP_DENSITY) 
percent_100_data_point <- df_sensitivity_one_hot[which.min(abs(quantile_values[5] - df_sensitivity_one_hot$POP_DENSITY)),]
percent_100_data_point_adjusted <- percent_100_data_point
percent_100_data_point_adjusted$POP_DENSITY <- percent_100_data_point_adjusted$POP_DENSITY * 1.01
data_point_sensitivity <- rbind(percent_100_data_point, percent_100_data_point_adjusted)
density_sensitivity <- sensitivty_processs(data_point_sensitivity, best_model)


sensitivity_analysis <- data.frame(indicators = c("Population","GDP", "Unemployment Rate", "Charging Stations", "LNMILES", "TELEWORK", "Capital Expenditures", "Courier", "Driver", "POP density"),
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
write.csv(sensitivity_analysis, "sensitivity_analysis_mixed_effectsTMS.csv", row.names=FALSE)




