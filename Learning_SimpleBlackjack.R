#setwd("C:/Users/Elio/Documents/R/Reinforcement Learning - David Silver/ReinforcementLearning")
source("Simulator.R")
source("ControlFunctions.R")
library(microbenchmark)
## Creating epsilon decay function
length(Simple_simulator$states)*length(Simple_simulator$actions)

epsilon_fun = function(k){
  if(k <= 8000) {
    return(1)
  } else {
    return(1/sqrt(sqrt(k-8000)))
  }
}

epsilon_fun2 = function(k){
  if(k <= 20000) {
    return(1)
  } else if (k <= 800000) {
    return(1/sqrt(sqrt(sqrt(k-20000))))
  } else {
    return(1/k)
  }
}

vec_epsilon_fun = Vectorize(epsilon_fun)
vec_epsilon_fun2 = Vectorize(epsilon_fun2)


x = 1:1000000
y = vec_epsilon_fun(1:1000000)
y2 = vec_epsilon_fun2(1:1000000)

quantile(y2)

plot(x,y, type = "l")
lines(x, y2, col = "blue")

### Learning
load("SimpleBlackjackResults_100k.RData")

microbenchmark(
  res <- GLIE_MC_control(states = Simple_simulator$states,
                        actions = Simple_simulator$actions,
                        simulator = Simple_simulator$simulator,
                        n_sim = 100000,
                        epsilon_fun = epsilon_fun),
  times = 1L
)

res_100k = res

microbenchmark(
  res <- GLIE_MC_control(states = Simple_simulator$states,
                         actions = Simple_simulator$actions,
                         simulator = Simple_simulator$simulator,
                         n_sim = 1000000,
                         epsilon_fun = epsilon_fun2),
  times = 1L
)

res_1m = res
save(res_100k, res_1m, file = "SimpleBlackjackResults.RData")



#### If we follow the best policy how many times will we win

win = 0
n_sim = 50000

for(i in 1:n_sim) {
  episode = Simple_simulator$simulator(res$policy)
  if(sum(episode$rewards) >= 1) {
    win = win + 1 
  }
}

print(win/n_sim)

