#A simulator which can only make decisions based on the cards in his hand. Content of the hand is encoded as a string consisting of a number and a word.
#The number gives the best value of the hand (with aces counted as 1 or 11) and the word can be either "soft" or "hard". A "soft" hand has an ace that
#is counted as 11 but can be counted as 1 in the future (thus always allowing one more card). All aces in a hard hand are counted as 1.

#This is a simple variant of blackjack which starts by player getting 2 cards and the dealer 1. The player may "hit" as many times
#as he wants. If the sum in player's hand is greater than 21 he loses the game immediately (aces are counted as 1 or 11 as is ussual in blackjack).
#After 0 or more hits the player may decide to "stand" after which the dealer takes cards until he gets higher score than the player
#or busts (goes over 21).
#
#------------------------------------------------------------INTERNAL SPECIFICATIONS------------------------------------------------------------
#A bot always gets the current state of the game returned as a list of attributes:
#    1. player - contains a list of values of the cards that player has. (i.e. (1, 5 ,10) means that player has an ace, a five 
#                                                                          and one face card or a ten)
#    2. dealer - ... -||- ... that dealer has.
#    3. outcome - "won", "lost" or "playing" classifying the state of the game.
#
#The simulator provides 2 methods:
#    1. deal_a_hand() - initializes the simulator and gives 2 cards to the player and one to the dealer
#    2. make_action(action, state) - action has to be eiter "hit" or "stand" and correspond to ussual blackjack.
#                                     (State describes the progres of the game and is of the format above)
#------------------------------------------------------------INTERNAL SPECIFICATIONS------------------------------------------------------------


Simple_simulator <- (function() {
    CARDS_IN_DECK = c(rep(4,9), 16)
    
    sample_cards = function(deck, number_of_cards){
      indices = sample(sum(deck), number_of_cards)
    
      cards = Vectorize(function(index){
        for(k in c(1:length(deck))){
          if (sum(deck[1:k-1]) < index & index <= sum(deck[1:k])){
            return(k)
          }
        }
      })(indices)
      return(cards)
    }
  
    hidden_cards = function(state){
      deck = CARDS_IN_DECK
      remove_card = function(x){
        deck[x] = max(deck[x] - 1, 0)
      }
      sapply(c(state$player,state$dealer), FUN = remove_card)
      return(deck)
    }
    
    deal_a_hand = function() {
      sample = sample_cards(CARDS_IN_DECK, 3)
      state = list(player=sample[1:2], dealer=sample[3], outcome="playing")
      return(state)
    }
    
    best_value = function(hand){
      value = sum(hand)
      aces = sum(hand == 1)
      while (value <= 11 & aces > 0) {
        value = value + 10
        aces = aces - 1
      }
      return(value)
    }
    
    make_action = function(action, state) {
      if(state$outcome != "playing"){
        return(state)
      }
      if (sum(state$player) > 21) {
        state$outcome = "lost"
        return(state)
      } else if(sum(state$dealer) > 21) {
        state$outcome = "won"
        return(state)
      }
      
      if (action == "hit"){
        state$player = c(state$player, sample_cards(hidden_cards(state), 1))
        if (best_value(state$player) == 21) {
          state$outcome = "won"
        }
        else if (sum(state$player) > 21) {
          state$outcome = "lost"
        } else {
          state$outcome = "playing"
        }
      } else if (action == "stand"){
        if (sum(state$player) > 21) {
          state$outcome = "lost"
        } else {
          while (best_value(state$dealer) <= best_value(state$player) & sum(state$dealer) <= 21 ) {
            state$dealer = c(state$dealer, sample_cards(hidden_cards(state), 1))
          }
          if(best_value(state$player) >= best_value(state$dealer) | sum(state$dealer) > 21){
            state$outcome = "won"
          } else {
            state$outcome ="lost"
          }
        }
      } else {
        stop("Unrecognized action commad: " + action + ".")
      }
      return(state)
    }
    
    simplified_hand = function(hand) {
      best = best_value(hand)
      return(sprintf("%d %s", best,  if (sum(hand) < best) "soft" else "hard"))
    }
    
    states = c(sapply(seq(2,21), function(x) sprintf("%d soft", x)), sapply(seq(2,21), function(x) sprintf("%d hard", x)))
    actions = c("hit", "stand")
    
    simulate_given_hand = function(policy, hand){
      current = hand
      list_of_states = c()
      list_of_actions = c()
      list_of_rewards = c()
      repeat{
        hand = simplified_hand(current$player)
        action = sample(colnames(policy), 1, prob=policy[hand, ])
        list_of_states = c(list_of_states, hand)
        list_of_actions = c(list_of_actions, action)
        current = make_action(action = action, state = current)

        list_of_rewards = c(list_of_rewards, if (current$outcome == "won") 1 else 0)
        if(current$outcome != "playing"){
          break
        }
      }
      return(list(states = list_of_states, actions = list_of_actions, rewards = list_of_rewards,
                  final_hand = current$player, dealer = current$dealer))
    }
    
    simulator = function(policy) {
      simulate_given_hand(policy, deal_a_hand())
    }
    
    return(list(states = states, actions = actions, simulator = simulator))
  })()
