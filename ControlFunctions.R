GLIE_MC_control = function(states, actions, simulator, n_sim = 1000, epsilon_fun = function(k) {1/k}) {
  # states - list of strings representing states
  # actions - list of strings representing actions
  # simulator - for a given policy calculate one episode until termination. Returns list of 3 vectors: states, actions, rewards
  # n_sim - number of episodes to sample
  # epsilon_fun - function that controls how much we explore. It must converge to 0 when k -> Inf. Faster convergence means less exploration.
  print(epsilon_fun)
  n_S = length(states)
  n_A = length(actions)
  
  # Uniform random policy
  policy = matrix(rep(1/n_A, n_S*n_A), nrow = n_S, ncol = n_A)
  rownames(policy) = states
  colnames(policy) = actions
  
  # State-action value function
  Q = matrix(rep(0, n_S*n_A), nrow = n_S, ncol = n_A)
  rownames(Q) = states
  colnames(Q) = actions
  
  # Times we visited each state-action pair
  N = matrix(rep(0, n_S*n_A), nrow = n_S, ncol = n_A)
  rownames(N) = states
  colnames(N) = actions
  
  for(k in 1:n_sim) {
    # Sample episode
    episode = simulator(policy)
    
    # Evaluate policy
    MC_policy_evaluation = function(states, actions, policy, episode, Q, N) {
      
      # Actual returns
      G = matrix(rep(0, n_S*n_A), nrow = n_S, ncol = n_A)
      rownames(G) = states
      colnames(G) = actions
      
      # 1 if state-action is visited in this episode and we need to update it
      N_tmp = matrix(rep(0, n_S*n_A), nrow = n_S, ncol = n_A) #Tracks if we visited state, otherwise we dont update it
      rownames(N_tmp) = states
      colnames(N_tmp) = actions
      
      # Get the cummulative revard we got after each state-acion pair
      episode$cum_rewards = rev(cumsum(rev(episode$rewards)))
      
      # Update state value function Q
      for(i in 1:length(episode$actions)) {
        G[episode$states[[i]], episode$actions[[i]]] = G[episode$states[[i]], episode$actions[[i]]] + episode$cum_rewards[[i]]
        N[episode$states[[i]], episode$actions[[i]]] = N[episode$states[[i]], episode$actions[[i]]] + 1
        N_tmp[episode$states[[i]], episode$actions[[i]]] = 1
      }
      
      update = 1/N * (G - Q*N_tmp)
      update[is.na(update)] = 0
      Q = Q + update
      
      return(list(Q = Q, N = N))
    }
    out = MC_policy_evaluation(states, actions, policy, episode, Q, N)
    Q = out$Q
    N = out$N
    
    # Generate new epsilon greedy policy
    epsilon_greedy_policy = function(Q, policy, k, epsilon_fun = function(k) {1/k}) {
      # Create new epsilon greedy policy based on estimated state-action value function Q
      epsilon = epsilon_fun(k)

      if(epsilon <= 0 | epsilon >= 1) {
        print("Given epsilon not in range <0,1>!!!")
      }
      n_s = nrow(policy)
      n_a = ncol(policy)
      
      for(i in 1:n_s) {
        policy[i,which.max(Q[i,])] = epsilon/n_a + 1 - epsilon
        policy[i,-which.max(Q[i,])] = epsilon/n_a
      }
      
      return(policy)
    }
    policy = epsilon_greedy_policy(Q, policy, k, epsilon_fun)
  }
  
  #Round last policy to greedy policy
  for(i in 1:n_S)
  {
    policy[i,] = as.numeric(policy[i,] == max(policy[i,]))
  }
  return(list(policy = policy, 
              Q = Q,
              N = N))
}

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

