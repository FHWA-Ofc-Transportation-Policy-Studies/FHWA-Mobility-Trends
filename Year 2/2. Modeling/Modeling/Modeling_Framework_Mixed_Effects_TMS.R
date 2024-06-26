
# FHWA Mobility Trends Year 2 Modeling Script - TMS
# Date: March 20, 2024
# Software Version: 1.1.0
# Creator Name: Helena Rowe and Zach Pate
# Summary: This script takes the county level dataset built by the Mobility Trends
#          team and creates the LASSO regression model for the specific performance
#          metric. It includes model creation, performance metrics, and analysis.
# History of Modification:
#         1.1.0: Improved variable naming and comments

#To use this script, run a section of the inputs for the performance metric you are interested in, and then everything below structure.
#The results include the coefficients to the champion model, residual analysis, model performance metrics, and number of trial runs.

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

#run this at the end if you want to save this model coeff
#saveRDS(best_model, "Champion_TMS_y2_2_16_24.rds")

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


for (x in 1:n) {
  #train test split
  smp_size <- floor(train_percent * nrow(norm_scale))
  train_ind <- sample(seq_len(nrow(norm_scale)), size = smp_size)
  train <- norm_scale[train_ind, ]
  test <- norm_scale[-train_ind, ]
  
  #lasso_x1 <- as.matrix(subset(train, select = -c(LOGVMT,Full_FIPS_Code, YEAR, County_Type)))
  #lasso_x <- as.matrix(select(train, -one_of(c("LOGVMT", non_indicator_var))))
  lasso_x <- as.matrix(select(train, -one_of(c(log_y, non_indicator_var))))
  
  #lasso_y <- as.matrix(subset(train, select = c(LOGVMT)))
  lasso_y <- as.matrix(select(train, log_y))
  
  #train first lasso model (cross validated) to find optimal lambda
  cv_model <- cv.glmnet(lasso_x, lasso_y, alpha = 1)
  best_lambda <- cv_model$lambda.1se
  best_lambda
  
  #Rerun lasso model using glmnet with the best lambda found above
  best_model <- glmnet(lasso_x, lasso_y, alpha = 1, lambda = best_lambda)
  round(coef(best_model), digits = 4)
  
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
  
  
  #store coeff values from trial n in a list
  results_list = list()
  for (i in 1:length(indicators) ) {
    index = length(indicators) - i
    results_list <- append(results_list, coef(best_model)[ length(coef(best_model)) - index, 1] )
  }
  
  #append 
  results_list <- append(results_list, metrics_df[6, 2])
  results_list <- append(results_list, metrics_df[2, 2])
  
  #bind results from trial n to the results df
  df_n_trials <- rbind(df_n_trials, results_list)
  colnames(df_n_trials) <- var_and_eval
  
  
  
}

#Writing results of each row to csv
all_preds <- predict(best_model, as.matrix(select(norm_scale, -one_of(c(log_y, non_indicator_var)))))
norm_scale$Log_predictions <- all_preds
norm_scale$Predictions <- exp(norm_scale$Log_predictions)
write.csv(subset(norm_scale, select = c(Full_FIPS_Code, YEAR, County_Type, Log_predictions, Predictions,
                                        LOGTRANSIT_USER_COUNT,LOGPOPULATION,LOGTrue_GDP,LOGUnemployment_Rate,LOGCharging_Stations,
                                        LOGLNMILES, LOGTELEWORK, LOGcapital_transit_funding, LOGCOURIER_NONEMP_RCPTOT_REAL, LOGDRIVER_NONEMP_RCPTOT_REAL,
                                        LOGPOP_DENSITY)), 'Transit_Users_Modeling_results.csv')



#visualize the coefficient values and evaluation metrics from n trails 
colnames(df_n_trials) <- var_and_eval
for (c in var_and_eval){
  hist(df_n_trials[[c]], main = paste("Histogram of" , c), xlab = c)
}

medians <- sapply(df_n_trials, median)

#write.csv(df_n_trials, 'ghg50trials.csv')

#test residual visuals
symbol_log_y <- sym(log_y)

ggplot(test,aes(x=!!symbol_log_y,y=preds,group=County_Type))+
  geom_point(aes(color=County_Type)) +
  geom_abline(slope=1, intercept= 0) + 
  ggtitle(paste('LOGTransitUsers Mixed-effects Pred vs Test'))

ggplot(test,aes(x=!!symbol_log_y,y=preds,group=County_Type))+
  geom_point(aes(color=County_Type)) +
  geom_abline(slope=1, intercept= 0) + 
  ggtitle(paste('LOGTransitUsers Mixed-effects Pred vs Test')) +
  facet_wrap(~County_Type)

ggplot(test, aes(x = resids, colour = County_Type)) +
  geom_density()+
  ggtitle(paste('LogTransitUsers Test Residual Distribution', sep = "" ))+
  theme_classic()

test1 <- test %>% 
  select(symbol_log_y, preds, County_Type) %>% 
  mutate(geo_type = ifelse(County_Type == 'Metropolitan', 'Urban', 'Rural')) %>% 
  select(symbol_log_y, preds, geo_type)

ggplot(test1,aes(x=!!symbol_log_y,y=preds,group=geo_type))+
  geom_point(aes(shape=geo_type, color = geo_type)) +
  geom_abline(slope=1, intercept= 0) + 
  ggtitle(paste('TransitUsers Mixed-effects Pred vs Test'))

qqPlot(test$resids, main = paste('Log(TransitUsers) Mixed-effects qqplot', sep = "" ))
hist(test$resids, main = paste("Distribution of Test Residuals for Log(TransitUsers)", sep = ""))


#train residual visuals
ggplot(test,aes(x=!!symbol_log_y,y=resids, group=County_Type))+  
  geom_point(aes(color=County_Type)) +
  geom_abline(slope=0, intercept= 0) + 
  ggtitle(paste('TransitUsers Test Residual Analysis')) 

ggplot(test,aes(x=!!symbol_log_y,y=resids, group=County_Type))+  
  geom_point(aes(color=County_Type)) +
  geom_abline(slope=0, intercept= 0) + 
  ggtitle(paste('TransitUsers Test Residual Analysis')) +
  facet_wrap(~County_Type)

hist(train$resids, main = paste("Distribution of Training Residuals for Log(TransitUsers)", sep = ""))


#additional model evaluation metrics, especially helpful for comparing multiple models against one another 
tLL <- best_model$nulldev - deviance(best_model)
k <- best_model$df
n <- best_model$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
BIC<-log(n)*k - tLL
r2_adj <- round(1 - (( ( 1-test_r2) * (n-1) )/ (n - k -1 )), digits = 4)

#comparing coefficients estimated vs. number of columns added through mixed effects 
num_county_coeff = best_model$df - length(indicators) #non zero coefficients (df) - number of indicators (this is an approximate estimate because not all indicators will get a coefficient estimate)
num_counties = length(counties_onehot) 
est_perc_var_w_ceoff <- round(num_county_coeff/num_counties, digits = 4)

comparisson_metrics_df <- data.frame(performance_measure = c("Degrees of Freedom","AICc", "BIC","R2 Adjusted", "Est. Percent Var w Coeff"),
                                     value = (c(k, AICc, BIC, r2_adj,est_perc_var_w_ceoff)) )
comparisson_metrics_df


#checking for outliers 
df_outliers <- test %>% 
  select(Full_FIPS_Code, YEAR, County_Type, resids, preds) %>% 
  filter((resids > 2) | (resids < -2) )