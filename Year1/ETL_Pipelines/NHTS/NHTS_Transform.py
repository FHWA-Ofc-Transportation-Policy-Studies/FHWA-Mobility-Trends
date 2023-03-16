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

data_path = 'fhwa-dataengineering-zp-vol-1/Data/NHTS/'

file_dictionary = {
    "household": ['NHTS_2017/hhpub.csv', 'NHTS_2009/Ascii/HHV2PUB.CSV', 'NHTS_2001/HHPUB.csv'],
    "person": ['NHTS_2017/perpub.csv', 'NHTS_2009/Ascii/PERV2PUB.CSV', 'NHTS_2001/PERPUB.csv'],
    "trip": ['NHTS_2017/trippub.csv'],
    "vehicle": ['NHTS_2017/vehpub.csv', 'NHTS_2009/Ascii/VEHV2PUB.CSV', 'NHTS_2001/VEHPUB.csv'],
    "day": ['NHTS_2009/Ascii/DAYV2PUB.CSV', 'NHTS_2001/DAYPUB.csv'],
    "ldt": ['NHTS_2001/LDTPUB.csv']
}

table_name_dict= {
    "household": "NHTS_HHPUB",
    "person": "NHTS_PERPUB",
    "trip": "NHTS_TRIPPUB",
    "vehicle": "NHTS_VEHPUB",
    "day": "NHTS_DAYPUB",
    "ldt": "NHTS_LDTPUB"
}

print('here')

def check_columns(stg_df, column_list):
    stg_df_columns = list(stg_df.columns.values.tolist())
    for column in column_list:
        if column in stg_df_columns:
            pass
        else:
            stg_df[column] = np.nan
    return stg_df    


def create_combined_data():
    data_path_keys = file_dictionary.keys()
    
    for key in data_path_keys:
        print(key)
        print('column check')
        file_list = file_dictionary[key]
        column_list = []
        for file in file_list:
            df = pd.read_csv(data_path + file, low_memory = False)
            df_columns = list(df.columns.to_list())
            for column in df_columns:
                if column not in column_list:
                    column_list.append(column)
                    
        print('transform')
        df = None
        for file in file_list:
            if df is None:
                df = pd.read_csv(data_path + file,  low_memory = False)
                df = check_columns(df, column_list)
                df = df[column_list]
                if str(file)[5:9] == str(2017):
                    df['year'] = 2017
                elif str(file)[5:9] == str(2009):
                    df['year'] = 2009
                elif str(file)[5:9] == str(2001):
                    df['year'] = 2001
                else:
                    print('missed_year')
                    break

            else:
                stg_df = pd.read_csv(data_path + file,  low_memory = False)
                stg_df = check_columns(stg_df, column_list)
                stg_df = stg_df[column_list]
                if str(file)[5:9] == str(2017):
                    stg_df['year'] = 2017
                elif str(file)[5:9] == str(2009):
                    stg_df['year'] = 2009
                elif str(file)[5:9] == str(2001):
                    stg_df['year'] = 2001
                else:
                    print('missed_year')
                    break   

                df = pd.concat([df, stg_df], ignore_index=True)
        df.to_csv(data_path + 'Combined_data/' + str(table_name_dict[key] + '.csv'))

create_combined_data()