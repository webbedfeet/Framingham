#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 28 19:13:25 2017

@author: abhijit
"""
%matplotlib inline
%cd NIAMS/Bhattacharyya/Framingham

import numpy as np
import pandas as pd

offspring = pd.read_csv('Datasets/offspring_full_2000_redone_columns_bmi.csv').iloc[:,2:]
offspring.columns=offspring.columns.str.lower()
offspring=offspring.rename(columns={'period_stop_age':'age_periods_stop'})
#==============================================================================
# Identify menopause status by year
#==============================================================================

## Fix type of age columns
age_cols = [u for u in offspring.columns if u.find('age')>-1]
for col in age_cols:
    offspring[col] = pd.to_numeric(offspring[col], errors='coerce')

blah = ((offspring.age4 > offspring.age_periods_stop) & (offspring.age_periods_stop < 45)).astype(int)
blah[pd.isnull(offspring.age4) | pd.isnull(offspring.age_periods_stop)] = np.nan
blah[offspring.examyr4 > '2010'] = np.nan
offspring['menopause_1990'] = blah.copy()

blah = ((offspring.age7 > offspring.age_periods_stop) & (offspring.age_periods_stop < 45)).astype(int)
blah[pd.isnull(offspring.age7) | pd.isnull(offspring.age_periods_stop)] = np.nan
offspring['menopause_1990'] = blah.copy()


#==============================================================================
# Create tidy data set
#==============================================================================

offspring_long = offspring.melt(id_vars = ['PID'])

risk_factors = offspring_long.loc[offspring_long['variable'].str.contains('[0-9]{4}$')]
risk_factors['variable'] = risk_factors['variable'].str.replace('_wine','wine') # rationalize wine
bl = risk_factors['variable'].str.split('_', expand=True).iloc[:,:2] # Split variables and year
bl = bl.rename(columns = {0:'vars', 1:'year'})
risk_factors = risk_factors.join(bl)
risk_factors['value'] = pd.to_numeric(risk_factors['value'], errors='coerce') # Make numeric
risk_factors['vars'] = risk_factors['vars'].str.lower() # Make lower case
risk_factors = risk_factors.drop('variable',1)


bl = risk_factors.pivot_table(index = 'PID', values = 'value',
                              columns = ['year','vars'])

#==============================================================================
# Aggregate wines for 2000
#==============================================================================

d = bl['2000'][['redwine','whitewine','otherwine']]
d['wine'] = d.sum(axis = 1)

bl['2000','wine'] = d['wine']
bl = bl.sort_index(axis=1)
bl = bl.drop([('2000','redwine'),('2000','whitewine'), ('2000','otherwine')],1)

#==============================================================================
# Fix smoking in 1980
#==============================================================================

bl['1980','smoke'] = np.where(bl['1980','smoke']>0, 1, bl['1980','smoke'])


#==============================================================================
# Aggregate drinks to get total drinks, then create 
# RF_ETOH = 1 if drinks > 3
#==============================================================================
for u in ['1980','1990','2000']:
    d = bl[u]
    drinks = d[['beer','wine','cocktail']].sum(axis=1)
    drinks[drinks <= 3] = 0
    drinks[drinks > 3] = 1
    bl[u,'rf_etof']= drinks

#==============================================================================
# Summary
#==============================================================================

bl['PID'] = bl.index
risk_factors = bl.melt(id_vars = 'PID')
pd.pivot_table(risk_factors, values ='value', index = 'year', 
               columns = 'vars', aggfunc=np.nanmean).to_excel('RiskFactors.xlsx')
