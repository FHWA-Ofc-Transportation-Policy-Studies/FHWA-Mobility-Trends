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


#df = pd.read_csv('ETL_Pipeline/ACS/2010_Census_Tract_to_2010_PUMA.txt', sep=',')

                     

df = pd.read_csv('ETL_Pipeline/ACS/2010_Census_Tract_to_2010_PUMA.txt', dtype = {'STATEFP': 'string','COUNTYFP': 'string', 'TRACTCE': 'string', 'PUMA5CE':'string'})
print(df.head)
df['fips_code'] = df['STATEFP'] + df['COUNTYFP']


df1 = pd.read_csv('ETL_Pipeline/ACS/2020_Census_Tract_to_2020_PUMA.txt', dtype = {'STATEFP': 'string','COUNTYFP': 'string', 'TRACTCE': 'string', 'PUMA5CE':'string'})
print(df1.head)
df1['fips_code'] = df1['STATEFP'] + df1['COUNTYFP']


snowpy = SnowPy()

# login (return to add .env variabels)
account = #os.environ['ACCOUNT']
role = #os.environ['ROLE']
ctx = snowpy.login(account, role, reauthenticate = True)

# Establish SnowFlake connection
warehouse = 'COMPUTE_WH'
database = 'FHWA_DEV_ZP_TEST'  # For production use FHWA_DB
schema = 'RAW_TEST'  # Source data stored in Schema RAW, transformed (curated) data stored in CUR

ctx.execute(f'USE WAREHOUSE {warehouse}')
ctx.execute(f'USE DATABASE {database}')
ctx.execute(f'USE SCHEMA {schema}') 

snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = "CENSUS_TO_PUMAS_CROSSWALK_2010",large_transfer= True, chunksize=100000)
                            
snowpy.create_new_table(df = df, warehouse = warehouse, database = database, 
                            schema = schema, table = "CENSUS_TO_PUMAS_CROSSWALK_2020", large_transfer= True, chunksize=100000)
        


#snowpy.create_new_table(df = df, warehouse = warehouse, database = database,schema = schema, table = "USA_ERS_METRO_2013", large_transfer= True, chunksize=100000)
     