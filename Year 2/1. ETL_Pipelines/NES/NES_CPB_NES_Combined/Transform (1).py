import pandas as pd
import numpy as np
import os

#vol-1/NES_CPB_files/NES_CPB2014/combine14.txt

datapath = 'vol-1/NES_CPB_files/'

for year in range(2012,2020):
    print(year)
    df = pd.read_csv(datapath + 'NES_CPB' + str(year) + '/combine' + str(year)[2:] + '.txt', 
                    dtype = str)
    df_columns = df.columns
    df.columns = ['STATE', 'SDSCR', 'COUNTY', 'CTYDSCR', 'NAICS', 'NCSDSCR', 'CESTAB', 'CBP_PCT',
                 'NES_PCT', 'EST', 'EMP', 'EMP_NF', 'QP1', 'QP1_NF', 'AP', 'AP_NF', 'ESTAB', 'RCPTOT', 'RCPTOT_N_F']
    df = df[['STATE', 'SDSCR', 'COUNTY', 'CTYDSCR', 'NAICS', 'NCSDSCR', 'CESTAB', 'CBP_PCT', 
             'NES_PCT', 'EST', 'EMP', 'EMP_NF', 'QP1', 'QP1_NF', 'AP', 'AP_NF', 'ESTAB', 'RCPTOT', 'RCPTOT_N_F']]
    df.to_csv(datapath + 'NES_CPB' + str(year) + '/transformed_nonemp' + str(year)[2:] + 'co.csv', index = False)
    print(df.head)
    

    