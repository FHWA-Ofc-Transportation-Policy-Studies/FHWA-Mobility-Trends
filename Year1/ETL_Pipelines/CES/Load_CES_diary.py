'''
This script pushes CES Diary files to snowflake database. 
It is easiest to run the file off your terminal, as you will need to write
username and password to snowflake.
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
from munch import munchify
import pprint
from tqdm import tqdm
from glob import glob
from platforms.connect.snowpy import SnowPy
from pathlib import Path
import csv
import gzip
import shutil

#data path of where date is located
data_path = "/home/jovyan/fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_diary_data/"

# Instantiate SnowPy
snowpy = SnowPy()

# login. 
account = #os.environ['ACCOUNT']
role = #os.environ['ROLE']
ctx = snowpy.login(account, role)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV'  # For production use FHWA_DB
schema = 'RAW'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 


#push files to snowflake warehouse
file_types = ['expd', 'fmld', 'memd', 'dtid' ,'dtbd',]
for name in file_types:
        print(name)
        table_name = str("CES_Diary_" + name).upper()
        df = pd.read_csv(data_path + name + ".csv")
        snowpy.create_new_table(df = df, warehouse = warehouse, database = database, schema = schema, table = table_name, large_transfer=True, chunksize=25000)


