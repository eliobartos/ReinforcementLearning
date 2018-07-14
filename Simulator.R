#A simple variant of blackjack which starts by player getting 2 cards and dealer 1. The player may "hit" as many times
#as he wants. If the sum in player's hand is greater than 21 he loses the game immediately (aces are counted as 1 or 11 as is common).
#After 0 or more hits the player may decide to "stand" after which the dealer takes cards until he gets higher score than the player
#or busts (goes over 21).
#
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

CARDS_IN_DECK <- c(rep(4,9), 16)

sample_cards <- function(deck, number_of_cards){
  indices <- sample(sum(deck), number_of_cards)
  
  cards <- Vectorize(function(index){
    for(k in c(1:length(deck))){
      if (sum(deck[1:k-1]) < index & index <= sum(deck[1:k])){
        return(k)
      }
    }
  })(indices)
  return(cards)
}

hidden_cards <- function(state){
  deck <- CARDS_IN_DECK
  remove_card <- function(x){
    deck[x] <- max(deck[x] - 1, 0)
  }
  sapply(state$player, FUN = remove_card)
  return(deck)
}

deal_a_hand <- function() {
  sample <- sample_cards(CARDS_IN_DECK, 3)
  state <- list(player=sample[1:2], dealer=sample[3], outcome="playing")
  return(state)
}

best_value <- function(hand){
  bottom <- sum(hand)
  if (sum(hand) <= 11 & 1 %in% hand) {
    return(bottom +10)
  } else {
    return(bottom)
  }
}

make_action <- function(action, state) {
  if(state$outcome != "playing"){
    return(state)
  }
  if (sum(state$player) > 21) {
    state$outcome <- "lost"
    return(state)
  } else if(sum(state$dealer) > 21) {
    state$outcome <- "won"
    return(state)
  }
  
  if (action == "hit"){
    state$player <- c(state$player, sample_cards(hidden_cards(state), 1))
    if (best_value(state$player) == 21) {
      state$outcome <- "won"
    }
    else if (sum(state$player) > 21) {
      state$outcome <- "lost"
    } else {
      state$outcome <- "playing"
    }
  } else if (action == "stand"){
    while (sum(state$player) <= 21 & best_value(state$dealer) <= best_value(state$player) & sum(state$dealer) <= 21 ) {
      state$dealer <- c(state$dealer, sample_cards(hidden_cards(state), 1))
    }
    if(sum(state$player) > 21 | best_value(state$player) >= best_value(state$dealer) | sum(state$dealer) > 21){
      state$outcome <- "won"
    } else {
      state$outcome <-"lost"
    }
  } else {
    stop("Unrecognized action commad: " + action + ".")
  }
  return(state)
}
