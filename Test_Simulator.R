source("Simulator.R")

state <- deal_a_hand()
print(state)
state <- make_action("hit", state)
print(state)
state <- make_action("stand", state)
print(state)
