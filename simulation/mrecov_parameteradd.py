# -*- coding: utf-8 -*-
"""
Created on Thu Nov 10 14:08:31 2022

@author: Alex
"""

#run this to get the mrecov and precov files used for model and parameter recovery
#(it gets median parameter values across the crossvalidation runs and merges them with the simulated data)

import os
import re

def sorted_alphanumeric(data):
    convert = lambda text: int(text) if text.isdigit() else text.lower()
    alphanum_key = lambda key: [ convert(c) for c in re.split('([0-9]+)', key) ] 
    return sorted(data, key=alphanum_key)

import numpy as np
import pandas as pd
from fnmatch import fnmatch

data = pd.read_csv("./Data/GP_het_400_VSmem.csv") 
nRounds = np.max(data["round"]) + 1
nRounds = int(nRounds)
data = pd.concat([data['agent'],data['group'],data['model'],data['lambda'],data['beta'],data['tau'],
                  data['gamma'],data['alpha'],data['eps_soc']],axis=1)
data = data.drop_duplicates()


path = "./Data/recovery_data/mrecov_VSmem"

AS_files = [file for file in os.listdir(path) if fnmatch(file, 'AS*.npy')]
AS_files = sorted_alphanumeric(AS_files)
DB_files = [file for file in os.listdir(path) if fnmatch(file, 'DB*.npy')]
DB_files = sorted_alphanumeric(DB_files)
VS_files = [file for file in os.listdir(path) if fnmatch(file, 'VS*.npy')]
VS_files = sorted_alphanumeric(VS_files)
SG_files = [file for file in os.listdir(path) if fnmatch(file, 'SG*.npy')]
SG_files = sorted_alphanumeric(SG_files)

filler = np.ones((nRounds,1,5))*100
AS_pars = np.squeeze(np.array([np.load(path + "/" + file,allow_pickle=True) for file in AS_files]))
for i in range(len(AS_pars)):
    if AS_pars[i].size == 0:
        AS_pars[i]=filler
AS_pars = np.concatenate(AS_pars,axis=0)
AS_pars = np.squeeze(AS_pars)
AS_pars[:,-3:AS_pars.shape[1]] = np.exp(AS_pars[:,-3:AS_pars.shape[1]])
AS_med = np.median(np.split(AS_pars,len(AS_pars)/nRounds),axis=1)
AS_med = AS_med[np.all(AS_med<10000,axis=1)]

filler = np.ones((nRounds,1,6))*100
DB_pars = np.squeeze(np.array([np.load(path + "/" + file,allow_pickle=True) for file in DB_files]))
for i in range(len(DB_pars)):
    if DB_pars[i].size == 0:
        DB_pars[i]=filler
DB_pars = np.concatenate(DB_pars,axis=0)
DB_pars = np.squeeze(DB_pars)
DB_pars[:,-4:DB_pars.shape[1]] = np.exp(DB_pars[:,-4:DB_pars.shape[1]])
DB_med = np.median(np.split(DB_pars,len(DB_pars)/nRounds),axis=1)
DB_med = DB_med[np.all(DB_med<10000,axis=1)]

VS_pars = np.squeeze(np.array([np.load(path + "/" + file,allow_pickle=True) for file in VS_files]))
for i in range(len(VS_pars)):
    if VS_pars[i].size == 0:
        VS_pars[i]=filler
VS_pars = np.concatenate(VS_pars,axis=0)
VS_pars = np.squeeze(VS_pars)
VS_pars[:,-4:VS_pars.shape[1]] = np.exp(VS_pars[:,-4:VS_pars.shape[1]])
VS_med = np.median(np.split(VS_pars,len(VS_pars)/nRounds),axis=1)
VS_med = VS_med[np.all(VS_med<10000,axis=1)]

SG_pars = np.squeeze(np.array([np.load(path + "/" + file,allow_pickle=True) for file in SG_files]))
for i in range(len(SG_pars)):
    if SG_pars[i].size == 0:
        SG_pars[i]=filler
SG_pars = np.concatenate(SG_pars,axis=0)
SG_pars = np.squeeze(SG_pars)
SG_pars[:,-4:SG_pars.shape[1]] = np.exp(SG_pars[:,-4:SG_pars.shape[1]])
SG_med = np.median(np.split(SG_pars,len(SG_pars)/nRounds),axis=1)
SG_med = SG_med[np.all(SG_med<10000,axis=1)]



files = [path+"/"+file for file in os.listdir(path) if fnmatch(file, 'model*.csv')]
files = sorted_alphanumeric(files)
fit = pd.concat((pd.read_csv(f) for f in files), ignore_index=True)
#fit.loc[fit["model"]=="dummy","model"]="VS"
fit = fit.dropna()
fit = fit.reset_index(drop=True)


fit = pd.merge(fit,data)
fit['lambda_fit']=fit['beta_fit']=fit['tau_fit']=fit['par_fit']=np.nan

for i in range(len(fit)):
    if np.argmin(fit.iloc[i,4:8])==0:
        fit.loc[i,'lambda_fit':'tau_fit'] = AS_med[i,2:]
    elif np.argmin(fit.iloc[i,4:8])==1:
        fit.loc[i,'lambda_fit':'par_fit'] = DB_med[i,2:]
    elif np.argmin(fit.iloc[i,4:8])==2:
        fit.loc[i,'lambda_fit':'par_fit'] = VS_med[i,2:]
    elif np.argmin(fit.iloc[i,4:8])==3:
        fit.loc[i,'lambda_fit':'par_fit'] = SG_med[i,2:]
        
fit.to_csv(path+"/mrecov_VSmem.csv",index=False)

fit['lambda_fit']=fit['beta_fit']=fit['tau_fit']=fit['par_fit']=np.nan
for i in range(len(fit)):
    if fit.iloc[i,3]=="AS":
        fit.loc[i,'lambda_fit':'tau_fit'] = AS_med[i,2:]
    elif fit.iloc[i,3]=="DB":
        fit.loc[i,'lambda_fit':'par_fit'] = DB_med[i,2:]
    elif fit.iloc[i,3]=="VS":
        fit.loc[i,'lambda_fit':'par_fit'] = VS_med[i,2:]
    elif fit.iloc[i,3]=="SG":
        fit.loc[i,'lambda_fit':'par_fit'] = SG_med[i,2:]
        
fit.to_csv(path+"/precov_VSmem.csv",index=False)
