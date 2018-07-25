#Simulates one round of player who "hits" if he's below 15 with no ace. If he has an ace he hits below 5 (ace counted as 1) and above 11 but less than 10.

source("Simulator.R")

policy = t(matrix(c(rep(c(1,0), 3), rep(c(0,1), 7), rep(c(1,0), 3), rep(c(0,1), 6) ,rep(c(1,0), 13), rep(c(0,1),6)), nrow = 2))
rownames(policy) = Simple_simulator$states
colnames(policy) = Simple_simulator$actions
#print(policy)

Simple_simulator$simulator(policy)
