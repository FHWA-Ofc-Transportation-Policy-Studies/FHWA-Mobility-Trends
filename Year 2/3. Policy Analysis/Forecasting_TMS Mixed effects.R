
# FHWA Mobility Trends Year 2 Forecasting Script - VMT
# Date: March 20, 2024
# Software Version: 1.1.0
# Creator Name: Helena Rowe and Zach Pate
# Summary: This script takes the champion models for respective performance 
#          metrics and utilizes indicator forecast to build county level forecast
#          through 2050. 
#         1.1.0: Improved variable naming and comments


library(car)
library(caret)
library(tidyr)
library(glmnet) #documentation at: https://glmnet.stanford.edu/articles/glmnet.html
library(corrplot)
library(ggplot2)
library(tidyverse)
library(omnibus) 

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

best_model <- readRDS(paste(dirname(getwd()),"/Modeling/champion_TMS_y2_2_16_24.rds", sep = ''))

round(coef(best_model), digits = 4)
df_all_data <- df_all_data[df_all_data$TRANSIT_USER_COUNT  > 0,]

#################################### Structure #################################

#setting seed value
set.seed(102)

#grab data and drop all NA 
df_na_droped <- drop_na(df_all_data[,c(indicators, performance_metric, non_indicator_var)])

#remove cases where y is zero, depending on parameter given in the inputs section
if(remove_y_zeros){
  df_na_droped <- subset(df_na_droped, performance_metric > 0)#replace VMT with y if we want this to work for all performance measures
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


#train test split
smp_size <- floor(train_percent * nrow(norm_scale))
train_ind <- sample(seq_len(nrow(norm_scale)), size = smp_size)
train <- norm_scale[train_ind, ]
test <- norm_scale[-train_ind, ]


#test predictions
#remove - lasso_pred_test <- predict(best_model,  as.matrix(subset(test, select = -c(LOGVMT, Full_FIPS_Code, YEAR, County_Type))))
lasso_pred_test <- predict(best_model,  as.matrix(select(test, -one_of(c(log_y, non_indicator_var)))))
test$preds = lasso_pred_test
test$resids =test[[log_y]] - test$preds

#test analysis
test_rmse <- sqrt(mean(test$resids ^ 2))
test_rmse_norm <- test_rmse / (max(test[[log_y]]) - min(test[[log_y]]) )
test_mae <- mean(abs(test$resids))
test_mae_norm <- test_mae / (max(test[[log_y]]) - min(test[[log_y]]) )
test_mse <- mean(test$resids ^ 2)
test_r2 <- cor(test[[log_y]], lasso_pred_test)^2

#train predictions
#remove - lasso_pred_train <- predict(best_model,  as.matrix(subset(train, select = -c(LOGVMT, Full_FIPS_Code, YEAR, County_Type))))
lasso_pred_train <- predict(best_model,  as.matrix(select(train, -one_of(c(log_y, non_indicator_var)))))
train$preds = lasso_pred_train
train$resids = train[[log_y]] - train$preds

#train analysis
train_rmse <- sqrt(mean(train$resids ^ 2))
train_rmse_norm <- train_rmse / (max(train[[log_y]]) - min(train[[log_y]]) )
train_mae <- mean(abs(train$resids))
train_mae_norm <- train_mae / (max(train[[log_y]]) - min(train[[log_y]]) )
train_mse <- mean(train$resids ^ 2)
train_r2 <- cor(train[[log_y]], lasso_pred_train)^2

#evaluation metrics
metrics_df <- data.frame(performance_measure = c("RMSE","RMSE Normalized", "MAE","MAE Normalized", "MSE", "R2"),
                         test = round(c(test_rmse, test_rmse_norm, test_mae, test_mae_norm, test_mse, test_r2  ), digits = 4),
                         train = round(c(train_rmse, train_rmse_norm, train_mae, train_mae_norm, train_mse, train_r2  ), digits = 4))

metrics_df

all_preds <- predict(best_model, as.matrix(select(norm_scale, -one_of(c(log_y, non_indicator_var)))))
norm_scale$Log_predictions <- all_preds
norm_scale$Predictions <- exp(norm_scale$Log_predictions)
write.csv(subset(norm_scale, select = c(Full_FIPS_Code, YEAR, County_Type, Log_predictions, Predictions,
                                        LOGTRANSIT_USER_COUNT,LOGPOPULATION,LOGTrue_GDP,LOGUnemployment_Rate,LOGCharging_Stations,
                                        LOGLNMILES, LOGTELEWORK, LOGcapital_transit_funding, LOGCOURIER_NONEMP_RCPTOT_REAL, LOGDRIVER_NONEMP_RCPTOT_REAL,
                                        LOGPOP_DENSITY)), 'Transit Users Modeling Dataset and results.csv')



#############FORECASTING


#transforing forecast data for prediction
#baseline
forecast_data_raw <- read.csv(paste(getwd(),"/Model Data/Forecast_Data 1_16_2024.csv", sep = ''))
forecast_data_raw <- forecast_data_raw[forecast_data_raw$FIPS %in% unique_FIPS,]

#forecast_data1 <- forecast_data1[forecast_data1$YEAR < 2020,]
#forecast_data1 <- forecast_data1[forecast_data1$YEAR > 2005,]

forecast_data = forecast_data_raw[,c("Pop.Linear",
                                  "Real.GDP", 
                                  "Unemployment.MA",
                                  "EV.1",
                                  "Lane.Miles.1",
                                  "Telework.1",
                                  "capital_transit_funding",
                                  "Courier.Baseline",
                                  "Driver.Rev.Baseline",
                                  "POPDENSITY",
                                  "YEAR",
                                  "FIPS")]

colnames(forecast_data) <- c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES"
                             ,"TELEWORK","capital_transit_funding","COURIER_NONEMP_RCPTOT_REAL","DRIVER_NONEMP_RCPTOT_REAL","POP_DENSITY", "YEAR", "Full_FIPS_Code")
forecast_data[is.na(forecast_data)] <- 0



####
forecast_data <- drop_na(forecast_data[,c("POPULATION","True_GDP","Unemployment_Rate", "Charging_Stations","LNMILES"
                                          ,"TELEWORK","capital_transit_funding","COURIER_NONEMP_RCPTOT_REAL","DRIVER_NONEMP_RCPTOT_REAL","POP_DENSITY", "Full_FIPS_Code", "YEAR")]) #removed year and gems geotype

forecast_data$Unemployment_Rate <- forecast_data$Unemployment_Rate / 100
forecast_data$DRIVER_NONEMP_RCPTOT_REAL <- forecast_data$DRIVER_NONEMP_RCPTOT_REAL/100

# make copy of year and for later
forecast_data_year <- subset(forecast_data, select=c(YEAR))

#one_hot encode data
forecast_data$Full_FIPS_Code <- as.factor(forecast_data$Full_FIPS_Code)
dum_variables <- dummyVars(" ~ Full_FIPS_Code", data = forecast_data)
counties_onehot <- data.frame(predict(dum_variables, newdata = forecast_data))
forecast_data <- cbind(forecast_data, counties_onehot)

#log convert all data (even rates, as it helps with skew and uniformity of data)
all_var <- c(y, indicators)
log_var <- paste("LOG", all_var, sep ="")
log_y <- log_var[1]
for (a in all_var){
  temp <- log(forecast_data[[a]] + 1)
  title <- paste("LOG", a, sep ="")
  forecast_data[title] <- temp
}

#removing non log transformed indicators and performance measure
forecast_data <- forecast_data[ , -which(names(forecast_data) %in% c(indicators, y))]



# Min-max scaling the data 
norm_scale <- predict(process, as.data.frame(forecast_data))

#adding back year, county type, and pre-min max scaled performance measure
norm_scale[["YEAR"]] <- as.numeric(forecast_data[["YEAR"]])
norm_scale$County_Type <- 0
norm_scale <- norm_scale[,norm_scale_colnames]
norm_scale

all_forecast_preds <- predict(best_model, as.matrix(subset(norm_scale, select = -c(County_Type, Full_FIPS_Code, YEAR, LOGTRANSIT_USER_COUNT))))

norm_scale$Log_predictions <- all_forecast_preds[,1]
norm_scale$Predictions <- exp(norm_scale$Log_predictions)
norm_scale$YEAR <- forecast_data$YEAR


norm_scale$fips_str <-  prefix(norm_scale$Full_FIPS_Code, 5, pad = "0") 
norm_scale$fips_year <- paste(norm_scale$fips_str, norm_scale$YEAR, sep = "-")


write.csv(subset(norm_scale, select = c( YEAR, Full_FIPS_Code,fips_year, Log_predictions, Predictions)), 'Transit Users Forecast Dataset and results.csv')
