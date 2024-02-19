import pandas as pd
import numpy as np 

datapath = 'vol-1/NES_county_files/'

column_check_list = []
for year in range(2000,2020):
    print(year)
    try:
        #with open(datapath + 'NES_County_' + str(year) + '/Nonemp' + str(year)[2:] + 'co.txt') as f:
        #first_line = f.readline()
        first_line = pd.read_csv(datapath + 'NES_County_' + str(year) + '/Nonemp' + str(year)[2:] + 'co.txt', nrows = 1)
        columns = list(first_line.columns)
        column_check = list(columns)
        column_check_list.append(column_check)
    except:
        first_line = pd.read_csv(datapath + 'NES_County_' + str(year) + '/nonemp' + str(year)[2:] + 'co.txt', nrows = 1)
        columns = list(first_line.columns)
        column_check = list(columns)
        column_check_list.append(column_check)   
        
for i in column_check_list:
    print(i)
        