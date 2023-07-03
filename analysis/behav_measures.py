# -*- coding: utf-8 -*-
"""
Created on Sat Mar  5 15:11:24 2022
calculates additional behavioural measures 8because for loops in R suck9
@author: Alex
"""
import numpy as np
import pandas as pd


#import what you need
data = pd.read_csv("./Data/data_min_full.csv")
nAgent = 4
shor=15
nround=8
gridSize=121
Xstar = np.array([(x, y) for x in range(np.sqrt(gridSize).astype(int)) for y in range(np.sqrt(gridSize).astype(int))])

#check for incomplete groups (all members need to have completed all trials of all rounds)
blacklist = []
for gr in np.unique(data.group):
    if len(data.loc[(data['group']==gr)])!=nAgent*shor*nround:
        blacklist.append(gr)
        
data = data.loc[~data.group.isin(blacklist),:]

#Get individual search distance and previous reward
data["search_dist"] = np.nan
data["prev_rew"] = np.nan                      

for g in np.unique(data["group"]):
    for r in np.unique(data["round"]):
        for t in range(1,len(np.unique(data["trial"]))+1):
            for ag in np.unique(data.loc[(data['group']==g)]['agent']).astype(int):
                #search distance
                data.loc[(data["agent"]==ag) & (data["group"]==g) & (data["round"]==r) & (data["trial"]==t),"search_dist"] = np.linalg.norm(
                             Xstar[data.loc[(data["agent"]==ag) & (data["group"]==g) & (data["round"]==r) & (data["trial"]==t),"choice"].astype(int)] -
                             Xstar[data.loc[(data["agent"]==ag) & (data["group"]==g) & (data["round"]==r) & (data["trial"]==t-1),"choice"].astype(int)])
                #previous reward
                data.loc[(data["agent"]==ag) & (data["group"]==g) & (data["round"]==r) & 
                         (data["trial"]==t),"prev_rew"] = data.loc[(data["agent"]==ag) & (data["group"]==g) & (data["round"]==r) & 
                                                                   (data["trial"]==t-1),"reward"].values

#get social choices, rewards, and search distances                                                                       
data["soc_sd1"] = np.nan
data["soc_sd2"] = np.nan
data["soc_sd3"] = np.nan
data["soc_sd"] = np.nan   

data["soc_rew1"] = np.nan
data["soc_rew2"] = np.nan
data["soc_rew3"] = np.nan  

data["soc_choice1"] = np.nan
data["soc_choice2"] = np.nan
data["soc_choice3"] = np.nan                                                                          
                                                                            
for g in np.unique(data["group"]):
    for r in range(len(np.unique(data["round"]))):
        for t in range(1,len(np.unique(data["trial"]))+1):
            for ag in np.unique(data.loc[(data['group']==g)]['agent']).astype(int):
                data.loc[(data["agent"]==ag) & (data["group"]==g) &
                         (data["round"]==r) & (data["trial"]==t),"soc_sd1"] = np.linalg.norm(
                             Xstar[data.loc[(data["agent"]==ag) & (data["group"]==g) &
                         (data["round"]==r) & (data["trial"]==t),"choice"].astype(int)] -
                             Xstar[data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                         (data["round"]==r) & (data["trial"]==t-1),"choice"].astype(int)][0])
                data.loc[(data["agent"]==ag) & (data["group"]==g) &
                          (data["round"]==r) & (data["trial"]==t),"soc_rew1"] = np.array(data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                (data["round"]==r) & (data["trial"]==t-1),"reward"])[0]
                data.loc[(data["agent"]==ag) & (data["group"]==g) &
                          (data["round"]==r) & (data["trial"]==t),"soc_choice1"] = np.array(data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                (data["round"]==r) & (data["trial"]==t-1),"choice"])[0]                                                                                          
                                                                                                  
                try:     #depending on the remaining number of players, these measures might not be available        
                    data.loc[(data["agent"]==ag) & (data["group"]==g) &
                             (data["round"]==r) & (data["trial"]==t),"soc_sd2"] = np.linalg.norm(
                                 Xstar[data.loc[(data["agent"]==ag) & (data["group"]==g) &
                             (data["round"]==r) & (data["trial"]==t),"choice"].astype(int)] -
                                 Xstar[data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                             (data["round"]==r) & (data["trial"]==t-1),"choice"].astype(int)][1])
                    data.loc[(data["agent"]==ag) & (data["group"]==g) &
                              (data["round"]==r) & (data["trial"]==t),"soc_rew2"] = np.array(data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                    (data["round"]==r) & (data["trial"]==t-1),"reward"])[1]
                    data.loc[(data["agent"]==ag) & (data["group"]==g) &
                              (data["round"]==r) & (data["trial"]==t),"soc_choice2"] = np.array(data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                    (data["round"]==r) & (data["trial"]==t-1),"choice"])[1]                                                                                          
                                 
                    data.loc[(data["agent"]==ag) & (data["group"]==g) &
                             (data["round"]==r) & (data["trial"]==t),"soc_sd3"] = np.linalg.norm(
                                 Xstar[data.loc[(data["agent"]==ag) & (data["group"]==g) &
                             (data["round"]==r) & (data["trial"]==t),"choice"].astype(int)] -
                                 Xstar[data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                             (data["round"]==r) & (data["trial"]==t-1),"choice"].astype(int)][2])
                    data.loc[(data["agent"]==ag) & (data["group"]==g) &
                              (data["round"]==r) & (data["trial"]==t),"soc_rew3"] = np.array(data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                    (data["round"]==r) & (data["trial"]==t-1),"reward"])[2]
                    data.loc[(data["agent"]==ag) & (data["group"]==g) &
                              (data["round"]==r) & (data["trial"]==t),"soc_choice3"] = np.array(data.loc[(data["agent"]!=ag) & (data["group"]==g) &
                    (data["round"]==r) & (data["trial"]==t-1),"choice"])[2]                                                                                  
                except IndexError:
                    continue
                             
#average soc. sd and reward                            
data["soc_sd"]=data[["soc_sd1","soc_sd2","soc_sd3"]].mean(axis=1)
data["soc_rew"]=data[["soc_rew1","soc_rew2","soc_rew3"]].mean(axis=1)

                             
data.to_csv("./Data/data_min_full.csv",index=False)



#################################################################################
# adjust for search distance regression
#################################################################################

data = data.drop(columns=["soc_sd","soc_rew"])
data = data.rename(columns={'prev_rew':'soc_rew0','search_dist':'soc_sd0',"choice":"soc_choice0"})
data["prev_rew"] = data["soc_rew0"]
data = pd.wide_to_long(data,["soc_sd","soc_rew","soc_choice",],["agent","group","round","trial"],"ref_agent")

data = data.reset_index()
data["social"] = np.select([(data["ref_agent"]==0),(data["ref_agent"]!=0)],[False,True])

data.to_csv("./Data/data_full_regressable.csv",index=False)
