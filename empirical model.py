#############################################################################
#Assumption: Spread follows a decreasing exponential trend-empirical model 
#50000 data points were used and the corresponding alpha value was calculated
#############################################################################



import pandas as pd
import numpy as np
import datetime as dt
from sklearn.ensemble import RandomForestClassifier
from sklearn import linear_model
#import csv as csv
import os
import matplotlib.pyplot as plt
import math as mt
from sklearn.cross_validation import train_test_split

cwd = os.getcwd()
tset2 = pd.read_csv(os.path.join(cwd,'new_50k_subset.csv'))

nrows=len(tset2)

def mean_spread_func(train_set):
    spread_value = []
    for i in range(9,201,4):
        bid_price = train_set.iat[i]
        ask_price = train_set.iat[i+1]
        #print i
        #print i+1
        #print bid_price
        #print ask_price
        diff =long(ask_price - bid_price)
        #print diff
        spread_value.append(diff)
    spread_value_49 = np.array(spread_value)
    meanspread = np.mean(spread_value_49)
    return meanspread
    
    
##Calculating alpha
def alpha_func(train_set):
    def mean_spread_func(train_set):
        spread_value = []
        for i in range(9,201,4):
            bid_price = train_set.iat[i]
            ask_price = train_set.iat[i+1]
            #print i
            #print i+1
            #print bid_price
            #print ask_price
            diff = ask_price - bid_price
            #print diff
            spread_value.append(diff)
        spread_value_49 = np.array(spread_value)
        meanspread = np.mean(spread_value_49)
        return meanspread
    St0 = train_set.iat[202]-train_set.iat[201]
    St_values = []
    alpha_values = []
    k = 50
    mean_spread=mean_spread_func(train_set)
    for i in range(205,305,2):
        St = train_set[i+1]-train_set[i]
        #print (St)
        St_values.append(St)
        #print (len(St_values))
        num = (St-mean_spread)
        #print ("Num")
        #print (num)
        denom = (St0-mean_spread)
        #print (denom)
        ratio =abs(num/denom)
        #print ("ratio")
        #print (ratio)
        #print (len(ratio))
        log_value = np.log(ratio)
        #print ("log")
        #print (log_value)
        alpha =(log_value)/(49-k)
        #print ("alpha")
        #print (alpha)
        
        #print (len(alpha_values))
        k = k
    if alpha != 'nan':
        alpha_values.append(alpha)
        mean_alpha = np.mean(alpha_values)
    return mean_alpha
nrows=50
mean_spread=[0 for i in range(nrows)]
alpha=[0 for i in range(nrows)]
Spread_cal52=[0 for i in range(nrows)]
Spread_cal53=[0 for i in range(nrows)]
Spread_cal54=[0 for i in range(nrows)]
Spread_cal67=[0 for i in range(nrows)]
Spread_cal72=[0 for i in range(nrows)]
Spread_cal77=[0 for i in range(nrows)]
Spread_cal82=[0 for i in range(nrows)]
Spread_cal87=[0 for i in range(nrows)]
Spread_cal92=[0 for i in range(nrows)]
Spread_cal97=[0 for i in range(nrows)]

S0=[0 for i in range(nrows)]
S52=[0 for i in range(nrows)]
S53=[0 for i in range(nrows)]
S54=[0 for i in range(nrows)]
differ52=[0 for i in range(nrows)]
differ53=[0 for i in range(nrows)]
differ54=[0 for i in range(nrows)]
differ55=[0 for i in range(nrows)]
differ56=[0 for i in range(nrows)]


sum52 =0
sum53 =0
sum54 =0
for i in range(0,nrows,1):
    data_subset=tset2.loc[i,:]
    mean_spread[i]=mean_spread_func(data_subset)
    alpha[i]=alpha_func(data_subset)
    S0[i]= data_subset.iat[202]-data_subset.iat[201]
    S52[i]= data_subset.iat[210]-data_subset.iat[209]
    S53[i]= data_subset.iat[212]-data_subset.iat[211]
    S54[i]= data_subset.iat[214]-data_subset.iat[213]  
for i in range(0,nrows,1):
    Spread_cal52[i]= (mt.exp(alpha[i]*(50-52))*(S0[i]-mean_spread[i]))+mean_spread[i]
    Spread_cal53[i]= (mt.exp(alpha[i]*(50-57))*(S0[i]-mean_spread[i]))+mean_spread[i]
    Spread_cal54[i]= (mt.exp(alpha[i]*(50-62))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal67[i]= (mt.exp(alpha[i]*(50-67))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal72[i]= (mt.exp(alpha[i]*(50-72))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal77[i]= (mt.exp(alpha[i]*(50-77))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal82[i]= (mt.exp(alpha[i]*(50-82))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal87[i]= (mt.exp(alpha[i]*(50-87))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal92[i]= (mt.exp(alpha[i]*(50-92))*(S0[i]-mean_spread[i]))+mean_spread[i]
    #Spread_cal97[i]= (mt.exp(alpha[i]*(50-97))*(S0[i]-mean_spread[i]))+mean_spread[i]
    differ52[i]=(S52[i] - Spread_cal52[i]) ** 2
    differ53[i]=(S53[i] - Spread_cal53[i]) ** 2
    differ54[i]=(S54[i] - Spread_cal54[i]) ** 2    
    sum52 = sum52 + differ52[i]
    sum53 = sum53 + differ53[i]
    sum54 = sum54 + differ54[i]
    
sse52=sum52/nrows
sse57=sum53/nrows
sse62=sum54/nrows
rse52=mt.sqrt(sse52)
rse57=mt.sqrt(sse57)
rse62=mt.sqrt(sse62)
#print (St)
#print (Spread_cal52)
print('rmse52', rse52)
print('rmse57', rse57)
print('rmse62', rse62)

## The rmse values  show that error increases many fold as u try to predict further into the future using the empirical model

#x=[52,57,64]
#y=[rse52, rse57, rse62]
#plt.plot(x,y,'ro')
#plt.xlabel("T values")
#plt.ylabel("RMSE")
#plt.show()
#    