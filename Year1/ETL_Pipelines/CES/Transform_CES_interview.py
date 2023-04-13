
'''
This 'transform' script of the CES interview survey achieves three goals:
1. Concatenating all files of a certain kind (see files types below for different interview files)
2. Ensuring that all columns in files are aligned. This is done through scanning all files to create a data dictionary of all columns, no matter how many occurences. Then, each file is given all columns, with blank entries
    added if the year does not include a certain column
3. Add survey year and quarter number to file, so when all years are combined a user can choose survey year/quarter
    they want to use. This is important for downstream transformations, as it is not normally included in these 
    survey files
    
It is important to note the differing structure of CES-interview survey files. Within the 
raw data, each year is surveyed over 5 quarters. For example, to survey 2012, CES will have
2012Q1x, 2012Q2, 2012Q3, 2012Q4, and 2013Q1. All five of these quarters are under the same
weighting structure and survey methodology. Much of this script is set up exceptions to appropriatly bring in YYYYQ1x and (YYYY + 1)Q1 files
'''

import pandas as pd
import numpy as np
import json
import os
import sys
import re
from datetime import datetime
import yaml
import time
import requests
import urllib
import zipfile
from munch import munchify
import pprint
from tqdm import tqdm
from glob import glob
from platforms.connect.snowpy import SnowPy
from sqlalchemy.dialects import registry
registry.register('snowflake', 'snowflake.sqlalchemy', 'dialect')


print('start')
data_path = "/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/interview/"


#list of years for diary and interview:
#These years can be updated for different ranges. Since Python range() stops at number before 'stop' argument, need to add 1 to last year of survey. 
#below diary years are 1980-1981, 1990-2021, interview years 1980-1981, 1984-2021
interview_years = list(range(1991, 2023)) 


def get_file_types(path):
    'finds all file paths associated with the path of folder given'
    file_paths = []
    for root, dirs, files, in os.walk(path):
        for file in files:
            file_paths.append(os.path.join(root, file))

    return file_paths

file_paths = get_file_types(data_path)

#file types of interview survey. Will loop through these to create each file with entire year range
file_types = ['mtbi', "fmli", "memi", "itbi", "itii", "ntaxi"]

def find_file_interview(last_digits, file_type, quarter, includes_x):
    '''
    The structure of the zip files downloaded for CES change year to year. 
    This function checks for possible locations of the file, then creates the 
    dataframe for later processing.
    
    last_digits: the last two digits of the year requested (ex 2011 -> 11)
    file_type: the four letter file types within a diary survey folder
    quarter: must integer 1, 2, 3, or 4
    includes_x: does the file include an 'x' in file name? (True False only)
    
    returns -> dataframe of diary file
    '''
    intrvw_folder_name = "intrvw" + last_digits + "/"
    interview_folder_name = "interview" + last_digits + "/"
    if includes_x is False:
        file_paths = [
            interview_folder_name + file_type + last_digits + str(quarter) + ".csv",
            interview_folder_name + intrvw_folder_name + file_type + last_digits + str(quarter) + ".csv",
            interview_folder_name + intrvw_folder_name + intrvw_folder_name + file_type + last_digits + str(quarter) + ".csv",
        ]
    if includes_x is True:
        file_paths = [
            interview_folder_name + file_type + last_digits + str(quarter) + "x" + ".csv",
            interview_folder_name + intrvw_folder_name + file_type + last_digits + str(quarter) + "x" + ".csv",
            interview_folder_name + intrvw_folder_name + intrvw_folder_name + file_type + last_digits + str(quarter) + "x" + ".csv",
        ]        

    found_df, counter = False, 0
    while found_df is False:
        if counter != 3:
            try:
                df = pd.read_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/interview/" + file_paths[counter], low_memory = False)
                found_df = True
            except:
                counter += 1
        else:
            df = pd.DataFrame()
            found_df = True
    return df

def create_column_dict(file_types):
    '''
    function loops through each of the diary file types, and organizes them in a way that 
    documents all present columns, even if changing from year to year.
    '''
    data_dictionary = {}
    for file_type in file_types:
        interview_years = list(range(1980, 1982)) + list(range(1984, 2023)) 
        column_list = []
        #loop through years
        for year in reversed(interview_years):
            print(year, file_type)
            for quarter in list(range(1,6)):
                if quarter in [2,3,4]:
                    last_digits = str(year)[2:]
                    df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=quarter, includes_x = False)
                elif quarter == 5:
                    last_digits = str(year + 1)[2:]
                    df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=str(1), includes_x = False)
                elif quarter == 1:
                    last_digits = str(year)[2:]
                    df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=str(1), includes_x = True)
                    df_columns = list(df.columns.values.tolist())
                
                for column in df_columns:
                    if column not in column_list:
                        column_list.append(column)

        data_dictionary[file_type + "_columns"] = column_list
    return data_dictionary

def check_columns(stg_df, file_type):
    stg_df_columns = list(stg_df.columns.values.tolist())
    for column in data_dictionary[file_type + "_columns"]:
        if column in stg_df_columns:
            pass
        else:
            stg_df[column] = np.nan
    return stg_df

'''
if os.path.exists(data_path + 'CES_interview_data_dictionary.json') is True:
    f = open(data_path + 'CES_interview_data_dictionary.json')
    data_dictionary = json.load(f)
else:   
    data_dictionary = create_column_dict(file_types)
    with open(data_path + 'CES_interview_data_dictionary.json', 'w') as f:
        json.dump(data_dictionary, f)
'''
f = open(data_path + 'CES_interview_data_dictionary.json')
data_dictionary = json.load(f)

#loop thorugh and create files. RAM too low, needed to break into 10-year splits
for file_type in file_types:
    df = None
    years_counted = 0
    for year in reversed(interview_years):
        last_digits = str(year)[2:]
        print(file_type, year)

        for quarter in range(1,6):
            if df is None:
                if quarter in [2,3,4]:
                    includes_x = 0
                    last_digits = str(year)[2:]
                    df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=quarter, includes_x = False)
                elif quarter == 5:
                    includes_x = 0
                    last_digits = str(year + 1)[2:]
                    df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=str(1), includes_x = False)
                elif quarter == 1:
                    last_digits = str(year)[2:]
                    includes_x = 1
                    df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=str(1), includes_x = True)

                df = check_columns(df, file_type)
                df = df[data_dictionary[file_type + '_columns']]
                df['survey_year'] = year
                df['survey_quarter'] = quarter
                df['x_file_attachment'] = includes_x

            else:
                if quarter in [2,3,4]:
                    includes_x = 0
                    last_digits = str(year)[2:]
                    stg_df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=quarter, includes_x = False)
                elif quarter == 5:
                    includes_x = 0
                    last_digits = str(year + 1)[2:]
                    stg_df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=str(1), includes_x = False)
                elif quarter == 1:
                    last_digits = str(year)[2:]
                    includes_x = 1
                    stg_df = find_file_interview(last_digits = last_digits, file_type=file_type, quarter=str(1), includes_x = True)

                stg_df = check_columns(stg_df, file_type)
                stg_df = stg_df[data_dictionary[file_type + '_columns']]
                stg_df['survey_year'] = year
                stg_df['survey_quarter'] = quarter
                stg_df['x_file_attachment'] = includes_x  

                df = pd.concat([df, stg_df], axis=0, ignore_index = True)

        if year in [2013, 2003]:
            table_name = str("CES_interview_" + file_type + "_" + str(year) + "_" + str(year + 9))
            print(table_name)
            df.to_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_interview_data/" + table_name + '.csv')
            df = None

        elif year == 1991:
            table_name = str("CES_interview_" + file_type + "_" + str(year) + "_" + str(year + 11))
            print(table_name)
            df.to_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_interview_data/" + table_name + '.csv')
            df = None


            
            
            
            
#old code
'''
    table_name = str("CES_interview_" + file_type).upper()
    print(table_name)
    print(df.head())
    #snowpy.create_new_table(df = df, warehouse = warehouse, database = database, schema = schema, table = table_name, large_transfer= True, chunksize= 10000)  
    df.to_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_interview_data/" + file_type + '.csv')        
'''        
'''
#interview year from 1990 to 2021
for file_type in file_types:
    #select correct year ste for file
    interview_years = list(range(1980, 1982)) + list(range(1984, 2023)) 

    df = None
    #loop through years
    for year in reversed(interview_years):
        last_digits = str(year)[2:]
        print(file_type, last_digits)
        for quarter in range(1,5):
            #finding correct file
            if df is None:
                df = find_file_interview(last_digits = last_digits, file_type = file_type, quarter= quarter)
                df = check_columns(df, file_type)
                df = df[data_dictionary[file_type + "_columns"]]                  
                df['survey_year'] = year
                df['survey_quarter'] = quarter

                
            else:
                stg_df = find_file_interview(last_digits = last_digits, file_type = file_type, quarter= quarter)
                check_columns(stg_df, file_type)
                stg_df = stg_df[data_dictionary[file_type + "_columns"]]
                stg_df['survey_year'] = year
                stg_df['survey_quarter'] = quarter
                df = pd.concat([df, stg_df], ignore_index= True)
    
    table_name = str("CES_interview_" + file_type).upper()
    print(table_name)
    print(df.head())
    #snowpy.create_new_table(df = df, warehouse = warehouse, database = database, schema = schema, table = table_name, large_transfer= True, chunksize= 10000)  
    df.to_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_interview_data/" + file_type + '.csv')
'''