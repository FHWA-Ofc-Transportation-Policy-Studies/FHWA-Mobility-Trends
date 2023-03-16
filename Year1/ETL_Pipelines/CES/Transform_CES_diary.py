
'''
This 'transform' script of the CES Diary achieves three goals:
1. Concatenating all files of a certain kind (see files types below for different diary files)
2. Ensuring that all columns in files are aligned. This is done through scanning all files to create a data 
dictionary of all columns, no matter how many occurences. Then, each file is given all columns, with blank entries
    added if the year does not include a certain column
3. Add survey year and quarter number to file, so when all years are combined a user can choose survey year/quarter
    they want to use. This is important for downstream transformations, as it is not normally included in these 
    survey files
'''

import pandas as pd
import numpy as np
import os
import sys
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
data_path = "fhwa-dataengineering-zp-vol-1/Data/CES/"

#list of years for diary and interview:
#These years can be updated for different ranges. Since Python range() stops at number before 'stop' argument, need to add 1 to last year of survey. 
#below diary years are 1980-1981, 1990-2021, interview years 1980-1981, 1984-2021
diary_years = list(range(1980, 1982)) + list(range(1990, 2022))
interview_years = list(range(1980, 1982)) + list(range(1984, 2022)) 

#file types of diary survey. Will loop through these to create each file with entire year range
file_types = ['dtbd', 'expd', 'fmld', 'memd', 'dtid']

def find_file_diary(last_digits, file_type, quarter):
    '''
    The structure of the zip files downloaded for CES change year to year. 
    This function checks for two possible locations of the file, then creates the 
    dataframe for later processing.
    
    last_digits: the last two digits of the year requested (ex 2011 -> 11)
    file_type: the four letter file types within a diary survey folder
    quarter: must integer 1, 2, 3, or 4
    
    returns -> dataframe of diary file
    '''
    folder_name = "diary" + last_digits + "/"
    try:
        file_path = folder_name + folder_name + folder_name + file_type + last_digits + str(quarter) + ".csv"
        df = pd.read_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/" + file_path)
    except:
        file_path = folder_name + folder_name + file_type + last_digits + str(quarter) + ".csv"
        df = pd.read_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/" + file_path)  
    return df



def create_column_dict(file_types):
    '''
    function loops through each of the diary file types, and organizes them in a way that 
    documents all present columns, even if changing from year to year.
    '''
    data_dictionary = {}
    for file_type in file_types:
            if file_type != 'dtid':
                diary_years = list(range(1990, 2022))
            else:
                diary_years = list(range(2005,2022))
            column_list = []
            #loop through years
            for year in reversed(diary_years):
                last_digits = str(year)[2:]
                df = find_file_diary(last_digits = last_digits, file_type=file_type, quarter=str(1))
                df_columns = list(df.columns.values.tolist())
                for column in df_columns:
                    if column not in column_list:
                        column_list.append(column)
            data_dictionary[file_type + "_columns"] = column_list
    return data_dictionary

#run to create data dictionary of columns
data_dictionary = create_column_dict(file_types)

def check_columns(stg_df, file_type):
    '''
    check to ensure dataframe has all columns before being concatentated on
    
    stg_df: df to check
    file_type: the four letter file types within a diary survey folder
    
    returns -> dataframe filled in with all columns 
    '''
    stg_df_columns = list(stg_df.columns.values.tolist())
    for column in data_dictionary[file_type + "_columns"]:
        if column in stg_df_columns:
            pass
        else:
            stg_df[column] = np.nan
    return stg_df

#loop process for concatenating all years of a diary file, as well as add survey year and survey
#quarter to the data frame

#diary year from 1990 to 2021
for file_type in file_types:
    #select correct years for file
    if file_type != 'dtid':
        diary_years = list(range(1990, 2022))
    else:
        diary_years = list(range(2005,2022))

    df = None
    #loop through years
    for year in reversed(diary_years):
        last_digits = str(year)[2:]
        print(file_type, last_digits)
        for quarter in range(1,5):
            #finding correct file
            if df is None:
                df = find_file_diary(last_digits = last_digits, file_type = file_type, quarter= quarter)
                df = check_columns(df, file_type)
                df = df[data_dictionary[file_type + "_columns"]]                  
                df['survey_year'] = year
                df['survey_quarter'] = quarter

                
            else:
                stg_df = find_file_diary(last_digits = last_digits, file_type = file_type, quarter= quarter)
                stg_df = check_columns(stg_df, file_type)
                stg_df = stg_df[data_dictionary[file_type + "_columns"]]
                stg_df['survey_year'] = year
                stg_df['survey_quarter'] = quarter
                df = pd.concat([df, stg_df], ignore_index= True)
    
    table_name = str("CES_Diary_" + file_type).upper()
    print(table_name)
    print(df.head())
    #snowpy.create_new_table(df = df, warehouse = warehouse, database = database, schema = schema, table = table_name, large_transfer= True, chunksize= 10000)  
    df.to_csv("/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_diary_data/" + file_type + '.csv')

