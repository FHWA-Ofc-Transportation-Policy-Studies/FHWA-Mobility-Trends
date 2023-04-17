The ETL_Pipelines folder provides the ability to Extract, Transform, and Load (abbreviated as ETL) the survey data requested in Task 3. The following datasets are represented:

 	American Community Survey
	American Time Use Survey
	Consumer Expenditure Survey 
	Current Population Survey
	National Household Travel Survey
  
Each of these Survey have their own folder, which will include either separate Extract, Transform, and Load scripts, or a single script that combines these processes. It is not recommended that these scripts are run on a local computer, due to the large amount of RAM required to bring in the data. Moreover, the ETL scripts leverage internal Deloitte Python libraries to move the data to Snowflake. If you would like to move this data internally, it is advised to reach out to the Mobility Trends team and someone can work to help load the data. 

To run these files, you need to run them in order of:
  1. Extract
  2. Transform
  3. Load

The extract file will download to the specified folder in your computer, the transform file will combine files into the appropriate larger tables, and the load file with push the data to its specified database. In some cases, these files will be combined into one single file, as seen within the CPS (Current Population Survey) script. Other times, because of the structure of the data, datasets like the CES (Consumer Expenditure Survey) might have a single download script, but multiple Transform and Load scripts for how data is broken up.

Feel free to reach out to Zach Pate at zapate@deloitte.com when running this code, and help can be provided. 
