# ReinforcementLearning
Blackjack project.

## GLIE Monte Carlo Control
### Algorithm
GLIE Monte Carlo Control is model-free algorithm for solving an episodic MDP. It works in theory but in practice it is not efficient when there are a lot of state-action pairs.

For a given MDP we assume we know how to sample episodes from that MDP and observe rewards. This algorithm finds the best policy to follow. 

### Implementation
Algorithm is implmented in function `GLIE_MC_control` in script `ControlFunctions.R` with arguments:

* **`states`** - list of strings representing states: `states = c("A")`
* **`actions`** - list of strings representing actions (in this implementation it is assumed that in each state we can do the same actions): `actions = c("l", "r", "d")`
* **`simulator`** - function that for a given policy samples one episode from the environment. Returns named list with 3 vectors. *states*, *actions*, *rewards*. See the example bellow.
* **`n_sim`** - number of episodes to simulate
* **`epsilon_fun`** - function which controls epsilon decay depending on the episode number k. Must converge to 0 when k goes to infinity: `function(k) {1/k}`. Must always be between $<0,1>$. In the function, k goes from 1 to `n_sim`.

```r
### Example

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

GLIE_MC_control(states, actions, simple_mdp, n_sim = 1000, epsilon_fun = function(k) {1/sqrt(k)})
GLIE_MC_control(states, actions, simple_mdp, n_sim = 1000, epsilon_fun = function(k) {1/k})

```

### `policy` argument of simulator function
`policy` is matrix with rows representing states and columns representing actions. Rows sum to 1. When in a given state simulator must sample action given the distribution provided with correspoding row. Easy way to do that in R is
```r
sample(colnames(policy),1, prob = policy[current_state,])
```

For example, in the first step algorithm creates random uniform policy:
```r
  # Uniform random policy
  policy = matrix(rep(1/n_A, n_S*n_A), nrow = n_S, ncol = n_A)
  rownames(policy) = states
  colnames(policy) = actions
```