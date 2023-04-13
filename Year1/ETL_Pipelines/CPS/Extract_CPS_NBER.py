'''
This script takes each of the CPS files from NBER, concats to a year, then pushes to snowflake.
Ram requirements prevented transfer of more than a year at a time.
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
from pathlib import Path
import csv
import gzip
import shutil
from sqlalchemy.dialects import registry
registry.register('snowflake', 'snowflake.sqlalchemy', 'dialect')


data_path = "fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CPS_NBER/"

# Define function to download URL to a file
def download_url(url, output_path):
    r = requests.get(url)
    with open(output_path, 'wb') as f:
        f.write(r.content)


def check_columns(stg_df, column_list):
    stg_df_columns = list(stg_df.columns.values.tolist())
    for column in column_list:
        if column in stg_df_columns:
            pass
        else:
            stg_df[column] = np.nan
    return stg_df    

def etl_year(year, data_path):
    month_list = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11' , '12']

    #adding manual adjustment in case months don't exist
    
    if year == 2022:
        month_list = month_list[:9]
    #extract / download
    
    print("start_download")
    for month in month_list:
        print("download " , month)
        print(data_path + "CPS_" + str(year) + str(month) + ".csv")
        if os.path.exists(data_path + "CPS_" + str(year) + str(month) + ".csv") is True:
            print('csv exists of ' + str(year) + str(month))
        else:
            print('downloading csv of' + str(year) + str(month))
            url = "https://data.nber.org/cps-basic2/csv/cpsb" + str(year) + str(month) + ".csv"
            output = data_path + "CPS_" + str(year) + str(month) + ".csv"
            download_url(url = url, output_path = output)        

    #check column consistency for year
    print('start column check')
    column_list = []
    for month in month_list:
        print("column check " + month)
        output = data_path + "CPS_" + str(year) + str(month) + ".csv"
        df = pd.read_csv(output)
        df_columns = list(df.columns.values.tolist())
        for column in df_columns:
            if column not in column_list:
                column_list.append(column)
    
    #transform to ensure all columns exist
    print('start transform')
    df = None
    for month in month_list:
        print("transform " + month)
        output = data_path + "CPS_" + str(year) + str(month) + '.csv'
        if df is None:
            df = pd.read_csv(output)
            df = check_columns(df, column_list)
            df = df[column_list]
            df['survey_year'] = year
            df['survey_month'] = month
        
        else:
            stg_df = pd.read_csv(output)
            stg_df = check_columns(stg_df, column_list)
            stg_df = stg_df[column_list]
            stg_df['survey_year'] = year
            stg_df['survey_month'] = month            
            df = pd.concat([df, stg_df], ignore_index=True)

    #df.to_csv(data_path + str("CPS_"+str(year)).upper())

    table_name = str("CPS_"+str(year)).upper()
    snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = table_name, large_transfer= True, chunksize=100000)

    #delete file in environment
    for month in month_list:
        print("deleting " + month)
        output = data_path + "CPS_" + str(year) + str(month) + ".csv"
        os.remove(output)

# Instantiate SnowPy
snowpy = SnowPy()


# login (return to add .env variabels)
account =#os.environ['ACCOUNT']
role =  #os.environ['ROLE']
ctx = snowpy.login(account, role)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV'  # For production use FHWA_DB
schema = 'RAW'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 

#example of create table
survey_years =  list(range(2000, 2008))#list(range(2000, 2023))
for year in tqdm(reversed(survey_years)):
    etl_year(year, data_path)
