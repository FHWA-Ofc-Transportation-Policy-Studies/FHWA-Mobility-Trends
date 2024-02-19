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

datapath = 'vol-1/NES_county_files/'

class DownloadProgressBar(tqdm):
    def update_to(self, b=1, bsize=1, tsize=None):
        if tsize is not None:
            self.total = tsize
        self.update(b * bsize - self.n)
        
def download_url(url, output_path):
    with DownloadProgressBar(unit='B', unit_scale=True,
                             miniters=1, desc=url.split('/')[-1]) as t:
        urllib.request.urlretrieve(url, filename=output_path, reporthook=t.update_to)

def try_download(url, file):
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
            
            
#example url https://www2.census.gov/programs-surveys/nonemployer-statistics/datasets/2019/historical-datasets/nonemp19co.zip
#https://www2.census.gov/programs-surveys/nonemployer-statistics/datasets/2014/historical-datasets/nonemp14co.zip
#https://www2.census.gov/programs-surveys/nonemployer-statistics/datasets/2002/historical-datasets/nonemp02co.zip
            
            
def download_nes_county_files():
    for year in range(2000,2020):
        print(year)
        url = 'https://www2.census.gov/programs-surveys/nonemployer-statistics/datasets/' + str(year) + '/historical-datasets/nonemp' + str(year)[2:] + 'co.zip'
        file_name = 'NES_County_' + str(year) + '.zip'
        download_url(url, datapath + file_name)
        uncompress(datapath + file_name)
        os.remove(datapath + file_name)
    

download_nes_county_files()