

```python
#Data munging
#using Dasgupta script to create risk factor ratios in OFFSPRING dataset
import numpy as np
import pandas as pd
import os
%matplotlib inline
os.chdir('P:\Framingham hip\Framingham Offsping Cohort datasets of Interest')
os.getcwd()
```




    'P:\\Framingham hip\\Framingham Offsping Cohort datasets of Interest'




```python
offspring = pd.read_csv('offspring_full_2000_redone_columns_bmi.csv')
```


```python
#rename columns to homogenize with 'original' dataset
offspring.rename(columns={'Period_STOP_AGE':'age_periods_stop'},inplace=True)

#Beta_1980 beta blockers in 1980 has a funny coding system.  3 = NO and 4 = YES
recode ={3:0,4:1,5:0}

```


```python
offspring['Beta_1980'].value_counts()
```




    3.0    3595
    4.0     152
    5.0      28
    Name: Beta_1980, dtype: int64




```python
offspring['Beta_1980'].replace(recode, inplace=True)
```


```python
offspring['Beta_1980'].value_counts()
```




    0.0    3623
    1.0     152
    Name: Beta_1980, dtype: int64




```python
offspring['Steroid_2000'].value_counts()   #Steroid is sparsely populated and has no zeros.  I think we will ignore this.
# if necessary, can recode steroid as 0 if person attended that visit.
```




    1.0    55
    Name: Steroid_2000, dtype: int64




```python
## Fix type of age columns
age_cols = [u for u in offspring.columns if u.find('age')>-1]
for col in age_cols:
    offspring[col] = pd.to_numeric(offspring[col], errors='coerce')

blah = ((offspring.age2 > offspring.age_periods_stop) & (offspring.age_periods_stop < 45)).astype(int)
blah[pd.isnull(offspring.age2) | pd.isnull(offspring.age_periods_stop)] = np.nan
offspring['menopause_1980'] = blah.copy()

blah = ((offspring.age4 > offspring.age_periods_stop) & (offspring.age_periods_stop < 45)).astype(int)
blah[pd.isnull(offspring.age4) | pd.isnull(offspring.age_periods_stop)] = np.nan
offspring['menopause_1990'] = blah.copy()

blah = ((offspring.age7 > offspring.age_periods_stop) & (offspring.age_periods_stop < 45)).astype(int)
blah[pd.isnull(offspring.age7) | pd.isnull(offspring.age_periods_stop)] = np.nan
offspring['menopause_2000'] = blah.copy()
```


```python
# Create tidy data set
offspring_long = offspring.melt(id_vars = ['PID'])

risk_factors = offspring_long.loc[offspring_long['variable'].str.contains('[0-9]{4}$')]
risk_factors['variable'] = risk_factors['variable'].str.replace('_wine','wine') # rationalize wine
bl = risk_factors['variable'].str.split('_', expand=True).iloc[:,:2] # Split variables and year
bl = bl.rename(columns = {0:'vars', 1:'year'})
risk_factors = risk_factors.join(bl)
risk_factors['value'] = pd.to_numeric(risk_factors['value'], errors='coerdce') # Make numeric
risk_factors['vars'] = risk_factors['vars'].str.lower() # Make lower case
risk_factors = risk_factors.drop('variable',1)


bl = risk_factors.pivot_table(index = 'PID', values = 'value',
                              columns = ['year','vars'])
```

    C:\ProgramData\Anaconda3\lib\site-packages\ipykernel_launcher.py:5: SettingWithCopyWarning: 
    A value is trying to be set on a copy of a slice from a DataFrame.
    Try using .loc[row_indexer,col_indexer] = value instead
    
    See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
      """



```python
# Aggregate wines for 2000
d = bl['2000'][['redwine','whitewine']]  #no otherwine in offspring dataset
d['wine'] = d.sum(axis = 1)

bl['2000','wine'] = d['wine']
bl = bl.sort_index(axis=1)
bl = bl.drop([('2000','redwine'),('2000','whitewine')],1)
```


```python
bl
```




<div>
<style>
    .dataframe thead tr:only-child th {
        text-align: right;
    }

    .dataframe thead th {
        text-align: left;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr>
      <th>year</th>
      <th colspan="8" halign="left">1980</th>
      <th colspan="2" halign="left">1990</th>
      <th>...</th>
      <th colspan="9" halign="left">2000</th>
      <th>conj</th>
    </tr>
    <tr>
      <th>vars</th>
      <th>beer</th>
      <th>beta</th>
      <th>bmi</th>
      <th>cocktail</th>
      <th>estrogen</th>
      <th>menopause</th>
      <th>smoke</th>
      <th>wine</th>
      <th>beer</th>
      <th>beta</th>
      <th>...</th>
      <th>beta</th>
      <th>bisphosphonates</th>
      <th>bmi</th>
      <th>cocktail</th>
      <th>estrogen</th>
      <th>menopause</th>
      <th>smoke</th>
      <th>steroid</th>
      <th>wine</th>
      <th>estrogen</th>
    </tr>
    <tr>
      <th>PID</th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>2924</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>22.648361</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>25.32</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>3297</th>
      <td>36.0</td>
      <td>0.0</td>
      <td>26.476584</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>4040</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>4061</th>
      <td>18.0</td>
      <td>1.0</td>
      <td>31.644676</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>16.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>33.33</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>4301</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>5350</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>6719</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>7248</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>32.03</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>8059</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>23.840380</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>8459</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>9458</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>18.033647</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>20.08</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9912</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>10791</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>11016</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>11651</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>14463</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>26.129085</td>
      <td>7.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>31.12</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>14764</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>15059</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>16.839087</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>22.96</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>15398</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>18098</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>18103</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>18844</th>
      <td>1.0</td>
      <td>0.0</td>
      <td>21.840750</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>19353</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>19892</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>20206</th>
      <td>1.0</td>
      <td>0.0</td>
      <td>24.598274</td>
      <td>4.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>26.25</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>6.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>22267</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>30.717181</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>30.15</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>22567</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>22996</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>24738</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>25240</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>9939621</th>
      <td>12.0</td>
      <td>0.0</td>
      <td>33.206243</td>
      <td>3.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>1.0</td>
      <td>5.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>30.99</td>
      <td>14.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>4.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9939888</th>
      <td>24.0</td>
      <td>0.0</td>
      <td>24.759029</td>
      <td>32.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>36.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9940525</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>9940870</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>25.220102</td>
      <td>7.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>7.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>28.85</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>4.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9941870</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>20.363903</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>28.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>23.36</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>NaN</td>
      <td>14.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9943257</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>6.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>26.63</td>
      <td>1.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>5.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9945338</th>
      <td>1.0</td>
      <td>0.0</td>
      <td>22.988938</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>27.57</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>9947765</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>22.119098</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>19.67</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9947948</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>22.137495</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>20.18</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9948725</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>37.379107</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>14.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>33.96</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9951579</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>23.007859</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9957447</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9958150</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>21.682869</td>
      <td>0.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>24.91</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9965528</th>
      <td>1.0</td>
      <td>0.0</td>
      <td>23.682465</td>
      <td>2.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>24.15</td>
      <td>2.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>2.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9967264</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>35.95</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9973742</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9975906</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>21.908598</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>19.91</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9977699</th>
      <td>14.0</td>
      <td>0.0</td>
      <td>19.911372</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>21.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>23.35</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>14.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9980291</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>22.648361</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>5.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>23.39</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9982799</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>21.461581</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>3.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>26.69</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9982997</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>24.181048</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>24.37</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9985882</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>30.578292</td>
      <td>2.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>...</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>37.82</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9986747</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>28.641696</td>
      <td>7.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>28.80</td>
      <td>7.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>7.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9987740</th>
      <td>7.0</td>
      <td>0.0</td>
      <td>22.163120</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>25.31</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9988354</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>21.791427</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>2.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>26.95</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9988615</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>25.74</td>
      <td>8.0</td>
      <td>1.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>4.0</td>
      <td>1.0</td>
    </tr>
    <tr>
      <th>9989304</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>32.695378</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>8.0</td>
    </tr>
    <tr>
      <th>9990078</th>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
    <tr>
      <th>9995187</th>
      <td>0.0</td>
      <td>0.0</td>
      <td>21.626670</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>1.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>0.0</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>0.0</td>
    </tr>
    <tr>
      <th>9995880</th>
      <td>35.0</td>
      <td>0.0</td>
      <td>18.731727</td>
      <td>0.0</td>
      <td>8.0</td>
      <td>NaN</td>
      <td>3.0</td>
      <td>0.0</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>...</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>0.0</td>
      <td>NaN</td>
    </tr>
  </tbody>
</table>
<p>5507 rows × 27 columns</p>
</div>




```python
#==============================================================================
# Fix smoking in 1980
#==============================================================================

bl['1980','smoke'] = np.where(bl['1980','smoke']>0, 1, bl['1980','smoke'])

```


```python
# Aggregate drinks to get total drinks, then create 
# RF_ETOH = 1 if drinks > 3
#==============================================================================
for u in ['1980','1990','2000']:
    d = bl[u]
    drinks = d[['beer','wine','cocktail']].sum(axis=1)
    drinks[drinks <= 3] = 0
    drinks[drinks > 3] = 1
    bl[u,'rf_etof']= drinks
```


```python
bl['1980','rf_etof']
#should be rf_etoh, but why quibble.
```




    PID
    2924       0.0
    3297       1.0
    4040       0.0
    4061       1.0
    4301       0.0
    5350       0.0
    6719       0.0
    7248       0.0
    8059       0.0
    8459       0.0
    9458       0.0
    9912       0.0
    10791      0.0
    11016      0.0
    11651      0.0
    14463      1.0
    14764      0.0
    15059      0.0
    15398      0.0
    18098      0.0
    18103      0.0
    18844      0.0
    19353      0.0
    19892      0.0
    20206      1.0
    22267      0.0
    22567      0.0
    22996      0.0
    24738      0.0
    25240      0.0
              ... 
    9939621    1.0
    9939888    1.0
    9940525    0.0
    9940870    1.0
    9941870    1.0
    9943257    0.0
    9945338    0.0
    9947765    0.0
    9947948    0.0
    9948725    1.0
    9951579    0.0
    9957447    0.0
    9958150    0.0
    9965528    1.0
    9967264    0.0
    9973742    0.0
    9975906    0.0
    9977699    1.0
    9980291    1.0
    9982799    0.0
    9982997    0.0
    9985882    0.0
    9986747    1.0
    9987740    1.0
    9988354    1.0
    9988615    0.0
    9989304    0.0
    9990078    0.0
    9995187    0.0
    9995880    1.0
    Name: (1980, rf_etof), Length: 5507, dtype: float64




```python
# Summary
#==============================================================================

bl['PID'] = bl.index
risk_factors = bl.melt(id_vars = 'PID')
pd.pivot_table(risk_factors, values ='value', index = 'year', 
               columns = 'vars', aggfunc=np.nanmean).to_excel('RiskFactors_offspring.xlsx')
```


```python
bl.to_csv('offspring_munging_done_10-6-17.csv')
```


```python

```
