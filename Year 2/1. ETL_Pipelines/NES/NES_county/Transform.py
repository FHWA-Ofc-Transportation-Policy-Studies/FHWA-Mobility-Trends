import pandas as pd
import numpy as np
import os


#pre 2007 ['ST', 'CTY', 'NAICS', 'ESTAB_F', 'ESTAB', 'RCPTOT_F', 'RCPTOT']
#2007 and post ['st', 'cty', 'naics', 'estab_f', 'estab', 'rcptot_n_f', 'rcptot_f', 'rcptot']

datapath = 'vol-1/NES_county_files/'

for year in range(2000,2020):
    print(year)
    try:
        df = pd.read_csv(datapath + 'NES_County_' + str(year) + '/Nonemp' + str(year)[2:] + 'co.txt')
    except:
        df = pd.read_csv(datapath + 'NES_County_' + str(year) + '/nonemp' + str(year)[2:] + 'co.txt')   
    df_columns = df.columns
    print('here')
    if year < 2007:
        df.columns = ['ST', 'CTY', 'NAICS', 'ESTAB_F', 'ESTAB', 'RCPTOT_F', 'RCPTOT']
        df['RCPTOT_N_F'] = np.NAN
        df = df[['ST', 'CTY', 'NAICS', 'ESTAB_F', 'ESTAB', 'RCPTOT_N_F', 'RCPTOT_F', 'RCPTOT']]
        df.to_csv(datapath + 'NES_County_' + str(year) + '/transformed_nonemp' + str(year)[2:] + 'co.csv')
        print(df.head)
    
    else:
        df.columns = ['ST', 'CTY', 'NAICS', 'ESTAB_F', 'ESTAB', 'RCPTOT_N_F', 'RCPTOT_F', 'RCPTOT']
        df.to_csv(datapath + 'NES_County_' + str(year) + '/transformed_nonemp' + str(year)[2:] + 'co.csv')
        print(df.head)
