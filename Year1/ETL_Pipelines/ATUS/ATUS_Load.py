'''
This script represents the load process for each of the NHTS tables downloaded from the extract script.
As there are some differences between both the tables, it may be best to load seperately and then 
make adjustments within snowflake. 
'''
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

data_path = 'fhwa-dataengineering-zp-vol-1/Data/ATUS/'


# Instantiate SnowPy
snowpy = SnowPy()

# login (return to add .env variabels)
account = #os.environ['ACCOUNT']
role = #os.environ['ROLE']
ctx = snowpy.login(account, role)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV_ZP_TEST'  # For production use FHWA_DB
schema = 'RAW_TEST'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 

#load activity file table
df = pd.read_csv(data_path + 'Activity_file.csv', low_memory = True)
table_name = 'ATUS_ACTIVITY_FILE_0320'
snowpy.create_new_table(df=df, warehouse=warehouse, database=database, schema=schema, table=table_name)

#load summary file
df = pd.read_csv(data_path + 'Activity_summary_file.csv', low_memory = True)
table_name = 'ATUS_ACTIVITY_SUMMARY_FILE_0320'
snowpy.create_new_table(df=df, warehouse=warehouse, database=database, schema=schema, table=table_name)



