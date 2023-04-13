import pandas as pd
import numpy as np
import os
import sys
from datetime import datetime
import yaml
import csv
import time
import requests
import urllib
import zipfile
import pprint
from tqdm import tqdm
from glob import glob
#from platforms.connect.snowpy import SnowPy
from snowpy import SnowPy

from sqlalchemy.dialects import registry
registry.register('snowflake', 'snowflake.sqlalchemy', 'dialect')

datapath = os.getcwd() + '/fhwa-dataengineering-master-largememory-vol-1/ACS/'
print(datapath)

def read_txt_to_list(file_name):
    column_list = []
    with open(file_name, 'r') as fd:
        reader = csv.reader(fd)
        for row in reader:
            column_list.append(row[0])
    fd.close()
    return column_list
        
house_columns = list(set(read_txt_to_list('ETL_Pipeline/ACS/house_columns.txt')))
person_columns = list(set(read_txt_to_list('ETL_Pipeline/ACS/person_columns.txt')))

def check_columns(stg_df, column_list):
    stg_df_columns = list(stg_df.columns.values.tolist())
    for column in column_list:
        if column in stg_df_columns:
            pass
        else:
            stg_df[column] = np.nan
    return stg_df  
    
snowpy = SnowPy()

# login (return to add .env variabels)
account = #os.environ['ACCOUNT']
role = #os.environ['ROLE']
ctx = snowpy.login(account, role, reauthenticate = True)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV'  # For production use FHWA_DB
schema = 'RAW'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 

#house data

main_df = None
for year in range(2000,2022):
        folder_path = datapath + 'pums_' + str(year) + '_csv_hus/'
        csv_files = []
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if file.endswith(".csv"):
                     csv_files.append(os.path.join(root, file))
        print(csv_files)
        for file_num in range(len(csv_files)):
            if year == 2025 and file_num == 0:
                pass
            else:
                df = pd.read_csv(csv_files[file_num], low_memory = False)
                df.columns = df.columns.str.upper()
                df = check_columns(df, house_columns)
                dtypes_list = list(df.dtypes)
                column_list = list(df.columns)
                for i in range(len(dtypes_list)):
                    if dtypes_list[i] == 'object':
                        df[column_list[i]] = df[column_list[i]].astype('string')
                df = df[house_columns]
                df['year'] = year
                print(list(df.columns))
                table_name = "ACS_HOUSE_" + str(year) + '_' + str(file_num + 1)
                snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = table_name, large_transfer= True, chunksize=100000)

#person
main_df = None
for year in range(2000,2022):
        folder_path = datapath + 'pums_' + str(year) + '_csv_pus/'
        csv_files = []
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if file.endswith(".csv"):
                     csv_files.append(os.path.join(root, file))
        print(csv_files)
        for file_num in range(len(csv_files)):
            if year == 2023 and file_num == 0:
                pass
            else:
                df = pd.read_csv(csv_files[file_num], low_memory = False)
                df.columns = df.columns.str.upper()
                #df = check_columns(df, person_columns)
                dtypes_list = list(df.dtypes)
                column_list = list(df.columns)
                for i in range(len(dtypes_list)):
                    if dtypes_list[i] == 'object':
                        df[column_list[i]] = df[column_list[i]].astype('string')
                #df = df[person_columns]
                df['year'] = year
                print(list(df.columns))
                table_name = "ACS_PERSON_" + str(year) + '_' + str(file_num + 1)
                snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = table_name, large_transfer= True, chunksize=100000)
              
