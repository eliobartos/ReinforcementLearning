library()
source("Simulator.R")
source("ControlFunctions.R")

## Creating epsilon decay function
length(Simple_simulator$states)*length(Simple_simulator$actions)

epsilon_fun = function(k){
  if(k <= 8000) {
    return(1)
  } else {
    return(1/sqrt(sqrt(k-8000)))
  }
}

vec_epsilon_fun = Vectorize(epsilon_fun)


x = 1:100000
y = vec_epsilon_fun(1:100000)

plot(x,y)

### Learning
microbenchmark(
  res <- GLIE_MC_control(states = Simple_simulator$states,
                        actions = Simple_simulator$actions,
                        simulator = Simple_simulator$simulator,
                        n_sim = 100000,
                        epsilon_fun = epsilon_fun),
  times = 1L
)

res_100k = res
save(res_100k, file = "SimpleBlackjackResults_100k.RData")



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

