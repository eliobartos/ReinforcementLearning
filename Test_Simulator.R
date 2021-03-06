#Simulates one round of player who "hits" if he's below 15 with no ace. If he has an ace he hits below 5 (ace counted as 1) and above 11 but less than 10.

source("Simulator.R")

policy = t(matrix(c(rep(c(1,0), 16), rep(c(0,1), 4), rep(c(1,0), 14), rep(c(0,1), 6)), nrow = 2))
rownames(policy) = Simple_simulator$states
colnames(policy) = Simple_simulator$actions
print(policy)

Simple_simulator$simulator(policy)






### Measure time to simulate
library(microbenchmark)

microbenchmark(
  Simple_simulator$simulator(policy),
  times = 1000L
)
#time is in nanoseconds 10^-9

