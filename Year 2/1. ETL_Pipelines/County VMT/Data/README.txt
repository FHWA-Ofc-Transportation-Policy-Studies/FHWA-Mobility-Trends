VMT data is put together over 4 different segments of time, each having different needs to transform into the format we wanted
The below process is abbreviated for clarity. A larger dicussion of our transformation process is available as part of the Task 4 Report.
1. The 2000-2008 data is provided each year for Interstate and Prinpal Arterial data for urban and rural. With these designations, 
we can sum all cateogories to get County VMT
2. 2011-2015 data is also provided each year for Interstate and Prinpal Arterial data for urban and rural. It was provided in daily VMT, which means we 
had to multiply by 365 (366 for leap years) to get yearly VMT. 
3. 2016-2020 data is provided across five different forms of road types. We pull only the urban and rural Interstate and Prinipal Arterial road types to match
the previous year's data.
4. For 2009-2010, we have to impute data in order to determine VMT. We do this by taking the state totals for 2009 and 2010, a County's vmt as a percent of it's state vmt for 2008, 
and then multiply that percent by the states vmt in 2009 and 2010 to get the imputed VMT. 