import os
import numpy as np  
import pandas as pd
import random

#Define parameters
    #Fixed diversification rate
div_rate = 0.01
    #Extinction probability extracted from binomial distribution
ext_prob=sum(np.random.binomial(100,0.05, 1000) == 0)/1000 

#Define names of individuals 
nind= ["ind" + str(x) for x in range(5)] 

#Define number of islands
nislands = 4

#Create a list with length nislands by nislands so we have kind of an x and y matrix but in list format
grid = ["NA" for x in range(nislands*nislands)]

#Generate a random number that can go from 0 to the total number of positions in the list
pos_init=np.random.randint(1,nislands*nislands)

#Substitute that position with the name of a random species for which we are simulating.
#We will need to generate a loop to iterate through the list of individuals
grid[pos_init] = random.choice(nind)

#Generate list of probabilities between cells
 #Generate empty matrix
matrix=np.zeros([nislands,nislands])
 #Fill matrix with numbers 
tmp_prob_values = np.random.normal(0, 0.01, nislands*nislands)
prob_values = np.reshape(tmp_prob_values, (nislands, nislands))

#Make list lists with grid and the probabilities of dispersal between islands
total_islands=[]
total_islands.append(grid)
total_islands.append(prob_values)

#Get position of your individual. nind[72] needs to be substituted with the individual that we are simulating
indices = [i for i, s in enumerate(grid) if nind[72] in s]

#Get probability of moving to other islands from the position where the species

prob_dispersal = total_islands[1][indices[0]]

#For every ind in the list that contains the species that we want to test, 
#Generate a new grid so it's empty and create an initial position where it'll start
#Give me the indices where the individual is currently located in each iteration.
#Now it's random but it will be product of a couple functions that calculate
#Prob of extinction, dispersal and diversification
tmax = 10
position_t = {'t' : [],'individual': [], 'tmp_indices': []}
for t in range(tmax):
    for individual in nind:
        pos_init=np.random.randint(1,nislands*nislands) #random number that will become position
        grid = ["NA" for x in range(nislands*nislands)] #create grid 
        grid[pos_init] = individual #add your individual to a random position in grid
        tmp_indices = [i for i, s in enumerate(grid) if individual in s] #return where is the individual
        #append results to a dictionary that stores the current position
        position_t['individual'].append(individual) #store individual info
        position_t['tmp_indices'].append(tmp_indices[0]) #store position info
        position_t['t'].append(t)
        print(position_t)

