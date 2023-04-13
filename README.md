# FHWA-Mobility-Trends year 1
The Mobility Trends year 1 repo contains two sets of code: ETL and Modeling. 
ETL (Extraction, Transformation, and Loading of the data) scripts were used by the Mobility Trends team to import the following five datasets into a snowflake database for use by FHWA and for Modeling:

 	American Community Survey
	American Time Use Survey
	Consumer Expenditure Survey 
	Current Population Survey
	National Household Travel Survey
	
Each of these Survey have their own folder, which will include either separate Extract, Transform, and Load scripts, or a single script that combines these processes. It is not recommended that these scripts are run on a local computer, due to the large amount of RAM required to bring in the data. Moreover, the ETL scripts leverage internal Deloitte Python libraries to move the data to Snowflake. If you would like to move this data internally, it is advised to reach out to the Mobility Trends team and someone can work to help load the data. 

The second part of this repo is the Modeling Dataset. The folders are broken up to mirror the steps taken within the Trends Indicator Memorandum.  Each script has comments containing the relevant figure/table of the Final Trends Indicator Memorandum it relates to. The order of folders, subfolders, and files are as follows:

	1. Identify High Priority Trends 
		Stepwise_Regression.R – Used to run stepwise regression on the 15 trend indicators to reduce them down to 5 for modeling in task 4.
		Indicator_data_exploration.ipynb – Python Markdown (ie jupyter notebook) Used to create the correlation plot of all indicators.
	2. Develop and Run Models
		Lasso and Ridge – All Indicators
			Adaptive_LASSO.R – Used to create Adaptive LASSO models for performance Metrics
			Nonlinear_LASSO.R – Used to create Nonlinear LASSO models for performance metrics
		Lasso and Ridge – General Indicators
			General_Indicators.R – Creates Lasso and Ridge models with a subset of indicator variables. Was performed to get to the final models.
		Final Models
			Scaled.VMT.Model.Comparison.R – Compares Linear, Lasso, And Ridge models for VMT
			Scaled.GHG.Model.Comparison.R – Compares Linear, Lasso, And Ridge models for GHG
			Scaled.TMS.Model.Comparison.R – Compares Linear, Lasso, And Ridge models for TMS
			Final_VMT_Model.R – Creates the 1.0 Scaled and Unscaled model for VMT
			Final_GHG_Model.R – Creates the 1.0 Scaled and Unscaled model for GHG
			Final_TMS_Model.R – Creates the 1.0 Scaled and Unscaled model for TMS
			Second_Best_VMT_Model.R – Creates the model with second best out-of-sample testing for VMT
			Second_Best_GHG_Model.R – Creates the model with second best out-of-sample testing for VMT
			Second_Best_TMS_Model.R – Creates the model with second best out-of-sample testing for VMT
		Changepoint analysis
			Changepoint.GHG.Rmd – Changepoint of each indicator used for GHG Model
			Changepoint.TMS.Rmd – Changepoint of each indicator used for TMS Model
			Changepoint.VMT.Rmd – Changepoint of each indicator used for VMT Model
	3. Sensitivity Analysis
		Sensitivity_Analysis.R – Creates the sensitivity analysis described in Step 4 of report
		Appendix_F_Sensitivity_Analysis.R – Creates additional analysis included in Appendix F
	4. Future Forecasts
		Forecasting – Models_and_Sensitivity.R – Used to create the forecast of performance metrics using coefficients derived from final models and expert forecasts of the indicators.
		
		
The R scripts will require the following R packages to be installed:
•	openxlsx 
•	Hmisc
•	olsrr
•	pls
•	MASS
•	tidyverse
•	caret
•	leaps
•	glmnet
•	glmnetSE
•	magrittr
•	tidyr
•	dplyr
