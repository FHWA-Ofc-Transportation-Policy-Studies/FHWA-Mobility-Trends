'''
This script represents the load process for each of the NHTS tables downloaded from the extract script.
As there are some differences between both the tables, it may be best to load seperately and then 
make adjustments within snowflake. 
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
import pprint
from tqdm import tqdm
from glob import glob
#from platforms.connect.snowpy import SnowPy
#from sqlalchemy.dialects import registry
#registry.register('snowflake', 'snowflake.sqlalchemy', 'dialect')

datapath = os.getcwd() + '/fhwa-dataengineering-master-vol-1/ACS/'
print(datapath)


    
def check_columns(stg_df, column_list):
    stg_df_columns = list(stg_df.columns.values.tolist())
    for column in column_list:
        if column in stg_df_columns:
            pass
        else:
            stg_df[column] = np.nan
    return stg_df    


def get_column_list():
    house_file_column_list = []
    person_file_column_list = []
    #house data
    for year in range(2000,2022):
        folder_path = datapath + 'pums_' + str(year) + '_csv_hus/'
        csv_files = []
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if file.endswith(".csv"):
                     csv_files.append(os.path.join(root, file))
        print(csv_files)
        for csv_path in csv_files:
            table_columns = pd.read_csv(csv_path, index_col=0, nrows=0).columns.tolist()
            for column in table_columns:
                if str(column).upper() not in house_file_column_list:
                    house_file_column_list.append(str(column).upper())
    
    for year in range(2000,2022):
        folder_path = datapath + 'pums_' + str(year) + '_csv_pus/'
        csv_files = []
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if file.endswith(".csv"):
                     csv_files.append(os.path.join(root, file))
        print(csv_files)
        for csv_path in csv_files:
            table_columns = pd.read_csv(csv_path, index_col=0, nrows=0).columns.tolist()
            #table_columns = list(df.columns)
            for column in table_columns:
                if str(column).upper() not in person_file_column_list:
                    person_file_column_list.append(str(column).upper())                   
    
    return [house_file_column_list, person_file_column_list]          
        
        
    #person data

house_columns, person_columns = get_column_list()

print(house_columns)
print(person_columns)

with open('ETL_Pipeline/ACS/house_columns.txt', 'w+') as f:
    for items in house_columns:
        f.write('%s\n' %items)
f.close()


with open('ETL_Pipeline/ACS/person_columns.txt', 'w+') as f:
    for items in person_columns:
        print(items)
        f.write('%s\n' %items)
f.close()

     
