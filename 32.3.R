# ECON 511 PS03
# Kassia Gibbs, Madeline Kraft, Vanessa Lin
# 32.3 Network with New Road from X to Y

# This script creates a payoff matrix (payoff_matrix) from the perspective of an arbitrary player (all players are facing the same choices) for all possible strategy sets of the 3 opponents.
# From the payoff matrix, a discrete best response function is defined (payoff_matrix[5,]).
# 4-way intersections of the best response functions are taken as Nash equilibriums.

# Define parameters of the problem
strategy_set <- c("AXB","AYB","AXYB","AYXB")
routes <- c("AX","AY","XB","YB","XY","YX")
init_times <- c(6,20,20,6,7,7) # times each route takes if only one car travels on it
add_per_car <- c(3,1,0.9,3,1,1) # additional times for each route if one additional car travels on it

# Initialize payoff matrix
payoff_matrix <- matrix(0,5,choose(6,3))
rownames(payoff_matrix) <- c(strategy_set, "BR")
labels = vector("list",choose(6,3))

# For each possible strategy of our arbitrary player,
for (i in 1:length(strategy_set)) {
  a = strategy_set[i]
  counter = 1
  
  route.i <- unlist(lapply(routes, function(route) {grepl(route,a)})) # boolean vector indicating if our player chose a strategy that traverses a given route (in routes)
  travel_time = sum(route.i*init_times) # travel time of strategy given no other cars
  
  # Calculate payoffs given other players' strategies:
  # 3 opposing players play 3 unique strategies
  opponents <- combn(strategy_set,3)
  for (j in 1:ncol(opponents)) {
    opp <- opponents[,j] # a strategy set in which players choose 3 unique strategies
    # For each opposing player's strategy, add to travel time if they travel the same routes as our player
    for (s in opp) {
      travel_time = travel_time + sum(unlist(lapply(routes, function(route) {grepl(route,s)}))*add_per_car*route.i)
    }
    labels[counter] <- paste(opp, collapse = ",")
    payoff_matrix[i,counter] <- travel_time
    travel_time = sum(route.i*init_times)
    counter <- counter + 1
  }
  
  # 2 unique strategies
  opponents <- combn(strategy_set,2)
  for (j in 1:ncol(opponents)) {
    for (rep in 1:2) {
      opp <- opponents[,j]
      opp <- c(opp,opp[rep])
      for (s in opp) {
        travel_time = travel_time + sum(unlist(lapply(routes, function(route) {grepl(route,s)}))*add_per_car*route.i)
      }     
      labels[counter] <- paste(opp, collapse = ",")
      payoff_matrix[i,counter] <- travel_time
      travel_time = sum(route.i*init_times)
      counter <- counter + 1
    }
  }
  
  # 1 unique strategy
  for (strat in strategy_set) {
    opp <- rep(strat,3)
    for (s in opp) {
      travel_time = travel_time + sum(unlist(lapply(routes, function(route) {grepl(route,s)}))*add_per_car*route.i)
    }
    labels[counter] <- paste(opp, collapse = ",")
    payoff_matrix[i,counter] <- travel_time
    travel_time = sum(route.i*init_times)
    counter <- counter + 1
  }
}

# Calculate best responses
for (i in 1:ncol(payoff_matrix)) {
  payoff_matrix[5,i] <- strategy_set[which.min(payoff_matrix[1:length(strategy_set),i])]
}

# Export to csv
colnames(payoff_matrix) <- labels
write.csv(payoff_matrix, "./32.3payoffs.csv")
