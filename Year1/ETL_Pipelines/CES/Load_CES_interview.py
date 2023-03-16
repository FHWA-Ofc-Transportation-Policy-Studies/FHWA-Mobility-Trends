
'''
This script pushes CES interview files to snowflake database. 
It is easiest to run the file off your terminal, as you will need to write
username and password to snowflake.
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
data_path = "fhwa-dataengineering-zp-vol-1/ETL_Pipeline/Data/CES/final_interview_data/"
file_list = os.listdir(data_path)
print(file_list)


# Instantiate SnowPy
snowpy = SnowPy()

# login (return to add .env variabels)
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

mtbi_files =  ["CES_interview_mtbi_1991_2002.csv", "CES_interview_mtbi_2003_2012.csv", "CES_interview_mtbi_2013_2022.csv",]


for file in file_list:
    try:
        table_name = str(file[:-4]).upper()

        df = pd.read_csv(data_path + file)
        snowpy.create_new_table(df = df, warehouse = warehouse, 
                            database = database, schema = schema, 
                            table = table_name, large_transfer=True, chunksize=25000)
    except:
        pass

