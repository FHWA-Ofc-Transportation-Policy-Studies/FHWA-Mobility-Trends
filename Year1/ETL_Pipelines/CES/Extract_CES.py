
'''
This 'extract' file sets up the neccessary URL and download procedure to pull all Consumer expenditure survey zip files and uncompress them 
to data lake.
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

#Set path to where data will be stored (This path will be different for each user unless we transfer data volumes)
#Will need to make a folder if doesn't exist
data_path = "C:/Users/zapate/Desktop/SNOWFLAKE_DATA/CES/"

# Define class for Progress Bar
class DownloadProgressBar(tqdm):
    def update_to(self, b=1, bsize=1, tsize=None):
        if tsize is not None:
            self.total = tsize
        self.update(b * bsize - self.n)

# Define function to download URL to a file path
def download_url(url, output_path):
    r = requests.get(url)
    with open(output_path, 'wb') as f:
        f.write(r.content)


def diary_or_interview_path(diary_or_interview, last_two_digit_year, data_path):
    '''
    diary_or_interview = "diary" or "interview"
    last_two_digit_year = for year of diary from 1980-1981 or 1990-2021, choose last two digits for function
    data_path = path for where data should go, without file name
    file_name = files name at end of path
    
    returns:
        url can be used for download, path

    '''
    if diary_or_interview == 'interview':
        url = "https://www.bls.gov/cex/pumd/data/sas/intrvw" + str(last_two_digit_year) + ".zip"
    elif diary_or_interview == 'diary':
        url = "https://www.bls.gov/cex/pumd/data/sas/diary" + str(last_two_digit_year) + ".zip"

    path = data_path + diary_or_interview + str(last_two_digit_year) + ".zip"
    
    return url, path

#list of years for diary and interview:
#These years can be updated for different ranges. Since Python range() stops at number before 'stop' argument, need to add 1 to last year of survey. 
#below diary years are 1980-1981, 1990-2021, interview years 1980-1981, 1984-2021

diary_years = list(range(1980, 1982)) + list(range(1990, 2024))
interview_years = list(range(1980, 1982)) + list(range(1984, 2024)) 

#remove comment out to download

#download diary years:
for i in tqdm(range(len(diary_years))):
    last_digits = str(diary_years[i])[2:]
    url, path = diary_or_interview_path(diary_or_interview = 'diary', last_two_digit_year = last_digits, data_path = data_path)
    download_url(url = url, output_path = path)

#download interview years
for i in tqdm(range(len(interview_years))):
    last_digits = str(interview_years[i])[2:]
    url, path = diary_or_interview_path(diary_or_interview = 'interview', last_two_digit_year = last_digits, data_path = data_path)
    download_url(url = url, output_path = path)


# Define function to uncompress files if they are found to be in .zip format
def uncompress(filepath):
    # Uncompress if zip file
    if filepath[-4:].lower() == '.zip':
        zipfolder = filepath.split('/')[-1].split('.')[0]
        print(f'                Uncompressing zip file to folder {zipfolder}')
        with zipfile.ZipFile(filepath, 'r') as zip_ref:
            zip_ref.extractall(data_path + zipfolder)

            
#sets up compress and delete zip file then delete zip file. 
#This saves space in the overall data lake environment. This decreases overall storage costs, as this data will
#sit in the data lake not in use. 
for i in tqdm(range(len(diary_years))):
    last_digits = str(diary_years[i])[2:]
    print(data_path + "diary" + last_digits + ".zip" )
    uncompress(data_path + "diary" + last_digits + ".zip")
    os.remove(data_path + "diary" + last_digits + ".zip")

for i in tqdm(range(len(interview_years))):
    try:
        last_digits = str(interview_years[i])[2:]
        uncompress(data_path + "interview" + last_digits + ".zip")
        os.remove(data_path + "interview" + last_digits + ".zip")
    except:
        print(f'Can not uncompress interview{last_digits}')

        