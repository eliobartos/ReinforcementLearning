library(tidyverse)

source("functions.R")
############################## Define MDP
n_s = 3  #Number of states (states are labeled as 1, 2, 3...)

n_a = 2 # Actions: 1 - Left, 2 - Right (actions are labeled as 1, 2, 3...)

P = list(2) # Transition matrices for action 1 - left, 2 - right
P[[1]] = matrix(c(1, 0, 0,
                  1, 0, 0,
                  0, 1, 0), nrow = n_s, byrow = TRUE)  # Transition matrix if we choose action 1

P[[2]] = matrix(c(0, 1, 0,
                  0, 0, 1,
                  0, 0, 1), nrow = n_s, byrow = TRUE) # Transition matrix if we choose action 2

R = list(2) #Rewards depending on action and state
R[[1]] = matrix(c(0, 1, 2), nrow = 3)  # For action 1
R[[2]] = matrix(c(1, 2, 0), nrow = 3)  # For action 2

gamma = 0.99 # Discount factor

MDP = list(n_s = n_s, n_a = n_a, P = P, R = R, gamma = gamma) # Fully Defined MPD

policy = matrix(c(1, 0,
                  1, 0,
                  1, 0), byrow = TRUE, nrow = 3) # For every state probability over actions, states are rows

######################################

############################## Define MDP 2
n_s = 4  #Number of states (states are labeled as 1, 2, 3...)

n_a = 2 # Actions: 1 - olovka, 2 - kemijska (actions are labeled as 1, 2, 3...)

P = list(2) # Transition matrices for action 1 - olovka, 2 - kemijska
P[[1]] = matrix(c(0, 0.8, 0.2, 0,
                  0, 0, 0, 1,
                  0, 0, 0, 1,
                  0, 0, 0, 1), nrow = n_s, byrow = TRUE)  # Transition matrix if we choose action 1

P[[2]] = matrix(c(0, 0.2, 0.8, 0,
                  0, 0, 0, 1,
                  0, 0.1, 0, 0.9,
                  0, 0, 0, 1), nrow = n_s, byrow = TRUE) # Transition matrix if we choose action 2

R = list(2) #Rewards depending on action and state
R[[1]] = matrix(c(1.2, 1, 1, 0), nrow = n_s)  # For action 1
R[[2]] = matrix(c(1.8, 1, 1.4, 0), nrow = n_s)  # For action 2

gamma = 0.99 # Discount factor

MDP = list(n_s = n_s, n_a = n_a, P = P, R = R, gamma = gamma) # Fully Defined MPD

policy = matrix(c(0, 1,
                  0, 1,
                  0, 1,
                  0, 1), byrow = TRUE, nrow = 4) # For every state probability over actions, states are rows

# From state to state transition reward (for simulations of episodes)
R_mat = matrix(c(0, 1, 2, 0,
                 0, 0, 0, 1,
                 0, 5, 0, 1,
                 0, 0, 0, 0), nrow = n_s, byrow = TRUE)

# Expected reward for action and state
R[[1]] = diag(P[[1]] %*% t(R_mat))
R[[2]] = diag(P[[2]] %*% t(R_mat))

MDP$R_mat = R_mat
######################################

policy_evaluation(MDP, policy)


every_visit_MC_policy_evaluation(MDP, policy, n_sim = 5000) %>% cbind

td_lambda_backward_online(MDP, policy, n_sim = 100000, alpha = 0.001)


x = matrix(rep(0, 6), nrow = 3, ncol = 2)
colnames(x) = c("left", "right")
rownames(x) = c("A", "B", "C")

