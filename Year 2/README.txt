# FHWA_Mobility_Trends
This repo for FHWA Mobility Trends is to store ETL, Modeling, and Forecasting code for the Year 2 task 3, 4, and 6 work. 

There are multiple README files throughout this repo that explain the purpose of the current folder. These will provide
instructions to run the code and how to use outputs. If a README file is not available, you can find instructions within the 
'transform.ipynb' script. 

In the Year 2 County Level work, you will be able to recreate the project by doing the following in order:
1. Running the ETL scripts (some data is pulled by CSV from online, as shown in data dictionary) and joining them together
2. Using the Modeling template code to build RDS files for each of the champion models
3. Running the forecast results and imputting them into the excel template to create the final forecast.

Within each of the folders are additional instructions on how to use this code.


Folder structure:

Year 2 County Level - all year 2 code and data for Mobility Trends project

    1. ETL_Pipelines- Code and data required to create the county modeling data for year 2
        *Trend Folders - Each trend resprented within the models has it's own folder where the data was tranformed into the county level data
        County Level Dataset Documentation.docx - Word doc with information describing the transformation process on each of the trend folders
        Transform_Combine_datasets.ipynb - Combining the trend folders into a single dataset
        VMT_MODELING_DATASET12_18.csv - The final dataset used for modeling


    2. Modeling- Where we train, test, and validate our Champion models. This code includes templates to run your own version of champion models and check model performance
        Feature Development - Our team creates additional features as part of the study
        Data - Modeling data created from the Data Engineering code, with transformations added.
            County_Year2 Dataset.csv - Final Modeling Dataset used within code
        Modeling - Template to train, test, and measure performance of models
            Modeling Framework Mixed Effects.R - The template code used to run and adjust model builds based on different contraints. 
            champion_VMT_y2_2_16_24.rds - Champion VMT model as shown in the Task 4 Report
            champion_GHG_y2_2_16_24.rds - Champion GHG model as shown in the Task 4 Report
            champion_TMS_y2_2_16_24.rds - Champion Transit Users model as shown in the Task 4 Report
        Sensitivity analysis - Measuring how a 1% change in an indicator effects the performance metric within a model
            Sensitivity_Analysis VMT.R - Sensivity Analysis code used to create 1% change effect for VMT
            Sensitivity_Analysis GHG.R - Sensivity Analysis code used to create 1% change effect for GHG
            Sensitivity_Analysis TMS.R - Sensivity Analysis code used to create 1% change effect for TMS
            sensitivity_analysis_mixed_effectsVMT.csv - Sensivity analysis results for VMT
            sensitivity_analysis_mixed_effectsVMT.csv - Sensivity analysis results for GHG
            sensitivity_analysis_mixed_effectsVMT.csv - Sensivity analysis results for TMS


    3. Policy Analysis- Where we use our champion models within the modeling folder to predict performance metrics through 2050
        Raw Scenario Data - Indicator Forecast data for multiple scenarios the Mobility Trends team envisions
        Model Data - Raw scenario data is transformed to be used within the models. (ie moving to one row county per year)
	Forecasting_Analysis - Excel spreadsheets to create and analyze forecasts
		VMT Forecast.xlsx - VMT forecast, which takes the result of the "VMT Forecast Dataset and results.csv"
		GHG Forecast.xlsx - GHG forecast, which takes the result of the "TOTAL_EMISSIONS Forecast Dataset and results.csv"
		Transit_Users_Forecast.xlsx - VMT forecast, which takes the result of the "Transit Users Forecast Dataset and results.csv"
        Forecasting_VMT Mixed Effects.R - Code used to create the prediction outputs that can be fed into forecast for VMT
        Forecasting_GHG Mixed Effects.R - Code used to create the prediction outputs that can be fed into forecast for GHG
        Forecasting_TMS Mixed Effects.R - Code used to create the prediction outputs that can be fed into forecast for TMS
        VMT Modeling Dataset and results.csv - VMT results for modeling dataset
        VMT Forecast Dataset and results.csv - VMT results for forecasting dataset
        TOTAL_EMISSIONS Modeling Dataset and results.csv - GHG results for modeling dataset
        TOTAL_EMISSIONS Forecast Dataset and results.csv - GHG results for forecasting dataset
        Transit_USERS Modeling Dataset and results.csv - Transit User results for modeling dataset
        Transit_USERS Forecast Dataset and results.csv - Transit User results for forecasting dataset

