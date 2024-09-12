'''
This script extracts National Household Travel Survey data from the NHTS website. This will download the zip files, pull 
into the data lake, and uncomress those files for later usage. This is a realtively quick script as we are only
pulling in three files
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

data_path = 'C:/Users/zapate/Desktop/SNOWFLAKE_DATA/NHTS'

class DownloadProgressBar(tqdm):
    def update_to(self, b=1, bsize=1, tsize=None):
        if tsize is not None:
            self.total = tsize
        self.update(b * bsize - self.n)

# Define function to download URL to a file
def download_url(url, output_path):
    r = requests.get(url)
    with open(output_path, 'wb') as f:
        f.write(r.content)
        
def uncompress(filepath):
    # Uncompress if zip file
    if filepath[-4:].lower() == '.zip':
        zipfolder = filepath.split('/')[-1].split('.')[0]
        print(f'                Uncompressing zip file to folder {zipfolder}')
        with zipfile.ZipFile(filepath, 'r') as zip_ref:
            zip_ref.extractall(data_path + zipfolder)

def download_NHTS(data_path):
    urls = ['https://nhts.ornl.gov/assets/2016/download/csv.zip',
           'https://nhts.ornl.gov/2009/download/Ascii.zip',
           ' https://nhts.ornl.gov/2001/download/Ascii.zip']
    file_names = ['NHTS_2017.zip', 'NHTS_2009.zip','NHTS_2001.zip']
    
    for i in tqdm(range(len(urls))):
        url, file_name = urls[i], file_names[i]
        download_url(url, data_path + file_name)
        uncompress(data_path + file_name)
        
        
download_NHTS(data_path)   


