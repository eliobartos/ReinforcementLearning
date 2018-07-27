source("ControlFunctions.R")

## Example

states = c("A")
actions = c("l", "r", "d")

simple_mdp = function(policy) {
  episode = list()
  episode$states = "A"
  episode$actions = sample(colnames(policy),1, prob = policy[1,])
  
  if(episode$actions == "r"){
    episode$rewards = 5
  } else if(episode$actions == "l") {
    episode$rewards = -5
  } else {
    episode$rewards = sample(c(0, 100),1, prob = c(0.9, 0.1))
  }
  return(episode)
}

simulator = simple_mdp

GLIE_MC_control(states, actions, simple_mdp, n_sim = 1000, epsilon_fun = function(k) {1/sqrt(k)})
GLIE_MC_control(states, actions, simple_mdp, n_sim = 1000, epsilon_fun = function(k) {1/k})