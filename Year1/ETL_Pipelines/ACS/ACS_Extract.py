"""
This script is used to extract the American Community Survey pums 1-year files. It represents collection of the data from 2000-2021, which 2020 ommitted (the 2020 file is released with experiemental weights in a different location, as it was not a fully completed survey).
There is a significant memory cost to pull this data.
"""
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
#from platforms.connect.snowpy import SnowPy

print(os.getcwd())
datapath = os.getcwd() + '/fhwa-dataengineering-master-largememory-vol-1/ACS/'

class DownloadProgressBar(tqdm):
    def update_to(self, b=1, bsize=1, tsize=None):
        if tsize is not None:
            self.total = tsize
        self.update(b * bsize - self.n)

'''
# Define function to download URL to a file
def download_url(url, output_path):
    r = requests.get(url)
    with open(output_path, 'wb') as f:
        f.write(r.content)
'''        
# Define function to download URL to a file
def download_url(url, output_path):
    with DownloadProgressBar(unit='B', unit_scale=True,
                             miniters=1, desc=url.split('/')[-1]) as t:
        urllib.request.urlretrieve(url, filename=output_path, reporthook=t.update_to)

def try_download(url, datapath, file):
    #makedir_if_needed(datapath)
    try:
        download_url(url, file)
        print(f"Downloaded {url} to {file}")
    except urllib.error.HTTPError as e:
        print(f"Couldn't find {url}, Exception: {e}")
        
def uncompress(filepath):
    # Uncompress if zip file
    if filepath[-4:].lower() == '.zip':
        zipfolder = filepath.split('/')[-1].split('.')[0]
        print(f'                Uncompressing zip file to folder {zipfolder}')
        with zipfile.ZipFile(filepath, 'r') as zip_ref:
            zip_ref.extractall(datapath + zipfolder)
            

        
            
states = pd.read_excel('ETL_Pipeline/ACS/State_Abbrev_FIPS.xlsx')
states.head() 

urlbase = 'https://www2.census.gov/programs-surveys/acs/data/pums/'

def download_acs_files():
    for year in range(2000, 2022, 1):
        print(year)
        file = f'{datapath}pums_{year}_csv_hus.zip'
        if os.path.exists(file) == False:
            url = f'{urlbase}{year}/1-Year/csv_hus.zip'
            try_download(url, datapath, file)
            url = f'{urlbase}{year}/csv_hus.zip'
            try_download(url, datapath, file)

        file = f'{datapath}pums_{year}_csv_pus.zip'   
        if os.path.exists(file) == False:
            url = f'{urlbase}{year}/1-Year/csv_pus.zip'
            try_download(url, datapath, file)
            url = f'{urlbase}{year}/csv_pus.zip'
            try_download(url, datapath, file)

#download_acs_files()

def uncompress_acs_files():
    for year in range(2000,2022,1):
        try:
            uncompress(datapath + 'pums_' + str(year) + '_csv_hus.zip')
            os.remove(datapath + 'pums_' + str(year) + '_csv_hus.zip')
        except:
            print('unable to uncompress')
        
        try:
            uncompress(datapath + 'pums_' + str(year) + '_csv_pus.zip')
            os.remove(datapath + 'pums_' + str(year) + '_csv_pus.zip')
        except:
            print('unable to uncompress')
        
#uncompress_acs_files()       

#download 2020 file

def download_acs_2020():
    house_file_2020_url = 'https://www2.census.gov/programs-surveys/acs/experimental/2020/data/pums/1-Year/csv_hus.zip'
    file = f'{datapath}pums_2020_csv_hus.zip'
    try_download(house_file_2020_url, datapath, file)
    uncompress(datapath+ 'pums_' + str(2020) + '_csv_hus.zip')

    person_file_2020_url = 'https://www2.census.gov/programs-surveys/acs/experimental/2020/data/pums/1-Year/csv_pus.zip'
    file = f'{datapath}pums_2020_csv_pus.zip'
    try_download(person_file_2020_url, datapath, file)
    uncompress(datapath+ 'pums_' + str(2020) + '_csv_pus.zip')
    
    
#download_acs_2020()

uncompress(datapath+ 'pums_' + str(2020) + '_csv_pus.zip')




#graveyard

'''
try:
    uncompress(datapath + 'pums_'+ str(year) + '_csv_hus.zip')
    os.remove(datapath + 'pums_'+ str(year) + '_csv_hus.zip')
except:
    file = f'{datapath}pums_{year}_csv_hus.zip'

    try:
        url = f'{urlbase}{year}/1-Year/csv_hus.zip'
        try_download(url, datapath, file)
    except:
        url = f'{urlbase}{year}/csv_hus.zip'
        try_download(url, datapath, file)
        print('download' + year)

    uncompress(datapath + 'pums_'+ str(year) + '_csv_hus.zip')
    os.remove(datapath + 'pums_'+ str(year) + '_csv_hus.zip')

try:
    uncompress(datapath + 'pums_'+ str(year) + '_csv_pus.zip')
    os.remove(datapath + 'pums_'+ str(year) + '_csv_pus.zip')
except: 
    file = f'{datapath}pums_{year}_csv_pus.zip'

    try:
        url = f'{urlbase}{year}/1-Year/csv_pus.zip'
        try_download(url, datapath, file)    
    except:
        url = f'{urlbase}{year}/csv_pus.zip'
        try_download(url, datapath, file)
    uncompress(datapath + 'pums_'+ str(year) + '_csv_pus.zip')
    os.remove(datapath + 'pums_'+ str(year) + '_csv_pus.zip')
'''
#for year in range(2000, 2022, 1):
#    uncompress('fhwa-dataengineering-zp-vol-1/Data/ACS/pums_' + str(year) + '_csv_hus.zip')
#uncompress('fhwa-dataengineering-zp-vol-1/Data/ACS/pums_2000_csv_hus.zip')

'''
for year in range(2001, 2020, 1):
    for abbr in states.Abbr:
        abbr = abbr.lower()
        file = f'{datapath}pums_{year}_csv_h{abbr}.zip'
        url = f'{urlbase}{year}/csv_h{abbr}.zip'
        try_download(url, datapath, file)
        url = f'{urlbase}{year}/1-Year/csv_h{abbr}.zip'
        try_download(url, datapath, file)
        file = f'{datapath}pums_{year}_csv_p{abbr}.zip'
        url = f'{urlbase}{year}/csv_p{abbr}.zip'
        try_download(url, datapath, file)
        url = f'{urlbase}{year}/1-Year/csv_p{abbr}.zip'
        try_download(url, datapath, file)
'''

