
# FHWA Mobility Trends Year 2 Combine Results to Final Dataset
# Date: March 25, 2024
# Software Version: 1.0.0
# Creator Name: Zach Pate
# Summary: This script takes the modeling results from the three champion 
#           model scripts and combines them into the final dataset
# History of Modification:
#         1.1.0: Improved variable naming and comments
library(car)
library(caret)
library(tidyr)
library(glmnet) #documentation at: https://glmnet.stanford.edu/articles/glmnet.html
library(corrplot)
library(ggplot2)
library(tidyverse)
library(omnibus) 

#clear workspace
rm(list = setdiff(ls(), lsf.str()))

#read in data
df_all_data <- read.csv(paste(dirname(getwd()),"/Data/County_Year2_2_16_2024.csv", sep = ''))

vmt_results <- read.csv('VMT_Modeling_Results.csv')
vmt_results <- subset(vmt_results, select = c("Full_FIPS_Code", "YEAR", "Log_predictions", "Predictions", "LOGVMT"))
names(vmt_results)[names(vmt_results) == "Log_predictions"] <- "VMT_LOG_Predictions"
names(vmt_results)[names(vmt_results) == "Predictions"] <- "VMT_Predictions"

ghg_results <- read.csv('Total_EMISSIONS_Modeling_Results.csv')
ghg_results <- subset(ghg_results, select = c("Full_FIPS_Code", "YEAR", "Log_predictions", "Predictions", "LOGTOTAL_EMISSIONS"))
names(ghg_results)[names(ghg_results) == "Log_predictions"] <- "Total_Emissions_LOG_Predictions"
names(ghg_results)[names(ghg_results) == "Predictions"] <- "Total_Emissions_Predictions"

tms_results <- read.csv('Transit_Users_Modeling_Results.csv')
tms_results <- subset(tms_results, select = c("Full_FIPS_Code", "YEAR", "Log_predictions", "Predictions", "LOGTRANSIT_USER_COUNT"))
names(tms_results)[names(tms_results) == "Log_predictions"] <- "TRANSIT_USER_COUNT_LOG_Predictions"
names(tms_results)[names(tms_results) == "Predictions"] <- "TRANSIT_USER_COUNT_Predictions"


#combine to main dataset
df_all_data_plus_vmt <- merge(x=df_all_data, y=vmt_results, by = c("Full_FIPS_Code", "YEAR"), all.x = TRUE)
df_all_data_plus_vmt_ghg <- merge(x=df_all_data_plus_vmt, y=ghg_results, by = c("Full_FIPS_Code", "YEAR"), all.x = TRUE)
df_all_data_plus_performance_metrics <- merge(x=df_all_data_plus_vmt_ghg, y=tms_results, by = c("Full_FIPS_Code", "YEAR"), all.x = TRUE)
df_all_data_plus_performance_metrics <- df_all_data_plus_performance_metrics[!duplicated(df_all_data_plus_performance_metrics), ]

write.csv(df_all_data_plus_performance_metrics, "Year_2_County_Level_Dataset_W_Modeling_Results.csv")
