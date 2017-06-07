# Python version of DataIngestion.R 

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

d1 = pd.read_csv('data/vr_dates_2014_a_0912d_yr_fram.csv')
d2 = pd.read_csv('data/vr_fxrev_2012_0_0746d_yr_fram.csv')

indx = d2.groupby('PID')['fxdate'].transform(min)==d2.fxdate
first_fracture = d2[indx][['PID','YrFrac','fxdate']].drop_duplicates()

dat_attend = pd.concat([d1[['PID','age1','sex']], d1.filter(regex='^exam')], axis=1)
dat_attend = pd.melt(dat_attend, id_vars=['PID','age1','sex'])
dat_attend['variable']=dat_attend['variable'].str.replace('examyr','')
dat_attend = dat_attend.dropna()
bl=dat_attend[['PID','value']].groupby('PID')['value'].agg({'start_yr': np.min,'end_yr': np.max})
bl['PID'] = bl.index
dat_attend = pd.merge(dat_attend, bl)[['PID','age1','sex','start_yr','end_yr']].drop_duplicates()
dat_attend = dat_attend.merge(first_fracture, how = 'left', on=['PID'])
dat_attend['end_duration'] = pd.Series(np.where(np.isnan(dat_attend.YrFrac), dat_attend.end_yr, dat_attend.YrFrac))
dat_attend['frac_indic'] = pd.Series(np.where(np.isnan(dat_attend.YrFrac), 0, 1))
dat_attend['year1'] = dat_attend.start_yr