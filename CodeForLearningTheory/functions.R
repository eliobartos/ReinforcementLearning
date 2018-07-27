library(dplyr)

extract_MRP = function(MDP, policy) {
  # For a given MDP and policy extract underlying MRP
  MRP = list()
  
  P = matrix(rep(0, MDP$n_s*MDP$n_s), nrow = MDP$n_s)
  for(i in 1:MDP$n_s) {
    for(j in 1:MDP$n_a) {
      P[i,] = P[i,] + policy[i, j] * MDP$P[[j]][i,]
    }
  }
  
  MRP$P = P
  
  R = matrix(rep(0, MDP$n_s), ncol = 1)
  for(i in 1:MDP$n_s) {
    for(j in 1:MDP$n_a) {
      R[i,] = R[i,] + policy[i, j] * MDP$R[[j]][i,]
    }
  } 
  
  MRP$R = R
  MRP$gamma = MDP$gamma
  MRP$n_s = MDP$n_s
  MRP$n_a = MDP$n_a
  return(MRP)
}

policy_evaluation = function(MDP, policy, eps = 0.01, max_iter = 10000, print_ = FALSE) {
  # For given MDP calculate value-state function
  
  v = matrix(rep(0, MDP$n_s), ncol = 1)
  
  MRP = extract_MRP(MDP, policy)
  iter = 1
  while(1) {
    v_old = v
    v = MRP$R + MRP$gamma * MRP$P %*% v
    
    if(norm(v - v_old) < eps) {
      if(print_) print("Algorithm converged!")
      break
    }
    
    if(iter >= max_iter){
      if(print_) print("Algorithm did NOT converge!")
      break
    }
    iter = iter + 1
  }
  
  if(print_) cat("Number of iterations:", iter)
  return(v)
}

greedy_policy = function(MDP, v) {
  # For a given MDP and state value function v (regarding to some policy) calculate greedy policy
  
  Q = matrix(rep(0, MDP$n_s*MDP$n_a), nrow = MDP$n_s)
  
  for(i in 1:MDP$n_a)
  {
    Q[,i] = MDP$R[[i]] + MDP$gamma * MDP$P[[i]] %*% v
  }
  
  policy = Q
  for(i in 1:MDP$n_s)
  {
    policy[i,] = as.numeric(policy[i,] == max(Q[i,]))
  }
  return(policy)
}

policy_iteration = function(MDP, eps = 0.01) {
  
  # Starting policy is the one that always takes first action in any state
  policy = matrix(rep(0, MDP$n_s*MDP$n_a), nrow = MDP$n_s)
  policy[,1] = matrix(rep(1, MDP$n_s), nrow = MDP$n_s)
  
  while(1) {
    v = policy_evaluation(MDP, policy, eps = eps)
    new_policy = greedy_policy(MDP, v)
    
    if(all(new_policy == policy))
      break
    
    policy = new_policy
  }
  return(list(policy = policy, v = v))
}

value_iteration = function(MDP, eps = 0.001, max_iter = 10000, print_ = FALSE) {
  # Applies value iteration algorithm (Bellman optimality equation) to find optimal value-state function
  # Then calculates greedy policy based on that value-state function
  v = matrix(rep(0, MDP$n_s), ncol = 1)
  
  action_rewards = list()
  iter = 0
  
  while(1) {
    
    v_old = v
    
    for(i in 1:MDP$n_a) {
      action_rewards[[i]] = MDP$R[[i]] + MDP$gamma * MDP$P[[i]] %*% v
    }
    action_rewards_matrix = do.call(cbind, action_rewards)
    v = apply(action_rewards_matrix, 1, max) %>% cbind
    
    if(norm(v - v_old) < eps) {
      if(print_) print("Algorithm converged!")
      break
    }
    
    if(iter >= max_iter){
      if(print_) print("Algorithm did NOT converge!")
      break
    }
    iter = iter + 1
  }
  
  if(print_) cat("Number of iterations:", iter)
  policy = greedy_policy(MDP, v)
  
  return(list(policy = policy, v = v))
}

sample_one_episode = function(MDP, policy, start_state = 1) {
  # Sample one episode of states that agent is going following policy policy
  state_vector = vector()
  rewards_vector = vector()
  
  state_vector[1] = start_state
  
  current_state = start_state
  step = 1
  # Dok nismo u terminalnom stanju idi dalje
  while(1) {
    in_terminal_state = TRUE
    for(i in 1:MDP$n_a) {
      if(MDP$P[[i]][current_state, current_state]!=1)
        in_terminal_state = FALSE
    }
    
    if(in_terminal_state)
      break
  
    # Nismo u terminalnom stanju, idemo po iduci korak i nagradu
      # Choose action
  chosen_action = sample(1:MDP$n_a, 1, prob = policy[current_state,])
      # Move to next state
  next_state = sample(1:MDP$n_s, 1, prob = MDP$P[[chosen_action]][current_state,])
      # Get Reward
  rewards_vector[step] = MDP$R_mat[current_state, next_state]
  step = step + 1
  state_vector[step] = next_state
  current_state = next_state
  
  }
  
  return(
    list(state_vector = state_vector, rewards_vector = c(rewards_vector, 0))
    )
}

every_visit_MC_policy_evaluation = function(MDP, policy, n_sim = 100) {
  #Evaluates a policy (calculates state-value function) by taking sample episodes in MDP 
  # (Doesn't use knowledge od MDP, just states and rewards)
  state_rewards = vector(length = MDP$n_s)
  state_visits = vector(length = MDP$n_s)
  
   for(i in 1:n_sim) {
     start_state = sample(1:MDP$n_s, 1)
     
     episode = sample_one_episode(MDP, policy, start_state = start_state)
     episode$rewards_vector = rev(cumsum(rev(episode$rewards_vector)))
     
     episode = as.data.frame(episode)
     episode = episode %>% group_by(state_vector) %>% 
       summarise(rewards_vector = sum(rewards_vector),
                 visits = n())
     
     state_rewards[episode$state_vector] = state_rewards[episode$state_vector] + episode$rewards_vector
     state_visits[episode$state_vector] = state_visits[episode$state_vector] + episode$visits
   }
  return(state_values = state_rewards/state_visits)
}

td_lambda_backward_online = function(MDP, policy, lambda = 0,alpha = 0.1, n_sim = 100) {
  eligibility_traces = vector("double", length = MDP$n_s)
  state_value = vector("double", length = MDP$n_s)
  
  for(i in 1:n_sim) {
    start_state = sample(1:MDP$n_s, 1)
    episode = sample_one_episode(MDP, policy, start_state)
    
    # Skip episodes of length 1
    if(length(episode$state_vector) <= 1)
      next
    
    for(j in 1:(length(episode$state_vector)-1)) {
      # Update eligibility traces
      eligibility_traces = MDP$gamma*lambda*eligibility_traces
      eligibility_traces[[episode$state_vector[[j]]]] = eligibility_traces[[episode$state_vector[[j]]]] + 1
      
      # Calculate Error
      delta = episode$rewards_vector[[j]] + MDP$gamma*state_value[[episode$state_vector[[j+1]]]] - state_value[[episode$state_vector[[j]]]]
      state_value = state_value + alpha * delta * eligibility_traces
      
    }
  }
  return(list(state_value = state_value))
}
