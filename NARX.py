##############################################################################
#ARNN model - Using NARX assumption
##############################################################################


from pyneurgen.recurrent import RecurrentConfig, ElmanSimpleRecurrent
from pyneurgen.recurrent import JordanRecurrent, NARXRecurrent
from pyneurgen.neuralnet import NeuralNet
from pyneurgen.nodes import Node, CopyNode, Connection
from pyneurgen.nodes import NODE_OUTPUT, NODE_HIDDEN, NODE_INPUT, NODE_COPY, NODE_BIAS
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

print "a";

input_nodes = 50
hidden_nodes = 4
output_nodes = 1

output_order = 10
incoming_weight_from_output = 0.2
input_order =10
incoming_weight_from_input = 0.8

net = NeuralNet()
net.init_layers(input_nodes, [hidden_nodes], output_nodes,
        NARXRecurrent(
            output_order,
            incoming_weight_from_output,
            input_order,
            incoming_weight_from_input))

net.randomize_network()

##defining inputs

cwd = os.getcwd()
tset = pd.read_csv(os.path.join(cwd,'new_50k_subset.csv'))
data_subset_first_row = tset.loc[0,:]
data=tset.loc[1:50,:]
nrows=len(data)
columns = list(range(50))
rows = list(range(nrows))
spread_ip= pd.DataFrame(columns=columns,index = rows)

for j in range(0,nrows,1):
	k=0
	for i in range(9,207,4):
		spread_ip.iloc[j,k]=data.iat[j,i+1]-data.iat[j,i]
		#print i ,j,k
		k=k+1
		



prev_vol=pd.DataFrame(data.iloc[:,2])
prev_val=pd.DataFrame(data.iloc[:,3])
trade_vwap=pd.DataFrame(data.iloc[:,4])
trade_vol=pd.DataFrame(data.iloc[:,5])
trade_type=pd.DataFrame(data.iloc[:,6])
all_inputs=pd.concat([prev_vol,prev_val,trade_vwap,trade_vol,trade_type,spread_ip],axis=1,ignore_index=True)   

all_inputs=(all_inputs)

spread_op= pd.DataFrame(columns=list(range(48)),index = rows)

for j in range(0,nrows,1):
	k=0
	for i in range(209,305,2):
		spread_op.iloc[j,k]=data.iat[j,i+1]-data.iat[j,i]
		#print i ,j,k
		k=k+1
		
#all_targets=(spread_op.loc[:,0])
all_targets=spread_op[0].values.tolist()
#   Set to constrain beginning weights to -.5 to .5
#       Just to show we can
net.set_random_constraint(.5)
net.set_learnrate(.1)
  
net.set_all_inputs(all_inputs)
net.set_all_targets(all_targets)
  
length = len(all_inputs)
learn_end_point = int(length * .8)
  
net.set_learn_range(0, learn_end_point)
net.set_test_range(learn_end_point + 1, length - 1)
  
#   Set the hidden layer activation type tanh
net.layers[1].set_activation_type('tanh')
net.layers[2].set_activation_type('sigmoid')
print "a";  
net.learn(epochs=75, show_epoch_results=True,random_testing=False)
  
