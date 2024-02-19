import pandas as pd
import numpy as np 

#vol-1/NES_CPB_files/NES_CPB2014/combine14.txt

datapath = 'vol-1/NES_CPB_files/'

column_check_list = []
for year in range(2012,2020):
    print(year)
    try:
        #with open(datapath + 'NES_County_' + str(year) + '/Nonemp' + str(year)[2:] + 'co.txt') as f:
        #first_line = f.readline()
        first_line = pd.read_csv(datapath + 'NES_CPB' + str(year) + '/combine' + str(year)[2:] + '.txt', nrows = 1)
        columns = list(first_line.columns)
        column_check = list(columns)
        column_check_list.append(column_check)
    except:
        print('loop broken')
        break
        
for i in column_check_list:
    print(i)
for i in column_check_list:
    print(len(i))
               