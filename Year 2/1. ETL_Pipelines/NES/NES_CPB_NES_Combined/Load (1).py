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


datapath = 'vol-1/NES_CPB_files/'

snowpy = SnowPy()

# login (return to add .env variabels)
account = os.environ['Account']
role = os.environ['Role']
ctx = snowpy.login(account, role, reauthenticate = True)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV'  # For production use FHWA_DB
schema = 'RAW'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 

table_name = 'NES_CPB_COMBINED'

for year in range(2012,2020):
    print(year)
    df = pd.read_csv(datapath + 'NES_CPB' + str(year) + '/transformed_nonemp' + str(year)[2:] + 'co.csv',
                    dtype = str) 
    df['YEAR'] = year
    print(df.head)
    if year == 2012:
        snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = table_name, large_transfer= True, chunksize = 100000)  
    else:
        snowpy.modify_rows(df=df, warehouse=warehouse, database=database, operation = 'append', schema=schema, table=table_name, log=True, keep_alive=False)

        



