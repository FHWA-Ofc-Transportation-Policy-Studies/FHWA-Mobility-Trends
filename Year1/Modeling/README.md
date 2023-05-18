This part of the repo represents the modeling code for Year 1. The folders are broken up to mirror the steps taken within the Trends Indicator Memorandum.  Each script has comments containing the relevant figure/table of the Final Trends Indicator Memorandum it relates to. The order of folders, subfolders, and files are as follows:

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
	task 6b
		Task 6b Models.R - Models and visualizations pertaining to Task 6b forecasting and quantitative effects. 
		
		
		
The R scripts will require the following R packages to be installed:
  openxlsx 
  Hmisc
  olsrr
  pls
  MASS
  tidyverse
  caret
  leaps
  glmnet
  glmnetSE
  magrittr
  tidyr
  dplyr
  
  
