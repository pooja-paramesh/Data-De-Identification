rm(list=ls())

# Libraries
library(markovchain)

# Initialize original users' IDs 
startUserId <- 0
endUserId <- 4

# Number of users to be simulated per seed (original user)
n <- 2 #

# Number of hours for each user
hours <- 720

# Noise (probability) injected during simulation
noiseperc <- 0.01 

# Sources
source("RFiles/Simulation_Functions.R")
source("RFiles/Converting_RawData_to_STMs.R")
source("RFiles/Simulation.R")