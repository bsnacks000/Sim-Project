setwd("~/Documents/CUNY/Simulation_604/Final_proj")

source("single_sim.R")  # import single_sim... 

# store results of 10 sims

sim_results = numeric(100)

for(i in 1:100){
    sim_results[i] = single_sim()   
}

sim_results
hist(sim_results)
