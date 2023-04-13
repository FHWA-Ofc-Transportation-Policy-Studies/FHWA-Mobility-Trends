import pandas as pd
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
from platforms.connect.snowpy import SnowPy
from sqlalchemy.dialects import registry
registry.register('snowflake', 'snowflake.sqlalchemy', 'dialect')

data_path = 'fhwa-dataengineering-zp-vol-1/Data/NHTS/Combined_data/'

# Instantiate SnowPy
snowpy = SnowPy()

# login (return to add .env variabels)
account =#os.environ['ACCOUNT']
role = #os.environ['ROLE']
ctx = snowpy.login(account, role)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV_ZP_TEST'  # For production use FHWA_DB
schema = 'RAW_TEST'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 

table_name_dict= {
    "household": "NHTS_HHPUB",
    "person": "NHTS_PERPUB",
    "trip": "NHTS_TRIPPUB",
    "vehicle": "NHTS_VEHPUB",
    "day": "NHTS_DAYPUB",
    "ldt": "NHTS_LDTPUB"
}

table_keys = table_name_dict.keys()

for key in table_keys:
    print(table_name_dict[key])
    df = pd.read_csv(data_path + table_name_dict[key] + '.csv', low_memory = True)
    dtypes_list = list(df.dtypes)
    column_list = list(df.columns)
    for i in range(len(dtypes_list)):
        if dtypes_list[i] == 'object':
            df[column_list[i]] = df[column_list[i]].astype('string')
    table_name = table_name_dict[key]
    snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = table_name)