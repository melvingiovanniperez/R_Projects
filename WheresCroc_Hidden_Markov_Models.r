# By: Hugo Swenson, Melvin Perez
# 2020-09-30

library(WheresCroc)

# Function for generating observational probabilities for each waterhole
observationalProbabilities <- function(readings, probs) {
  o_probs <- rep(0,40)
  for (i in 1:40) {
    salinity <- dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])
    phosphate <- dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2])
    nitrogen <- dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2])
    o_probs[i] <- salinity*phosphate*nitrogen
  }
  o_probs <- diag(o_probs)
  return(o_probs)
}

# Function for detecting the best possible next move based on a breadth first search
nextMove <- function(probabilities, positions, edges) {
  best_n <- which.max(probabilities)
  path <- breadthFirstSearch(positions, edges, best_n)
  if (length(path) == 0) {
    n_move <- c(positions[3], 0)
  }
  else if (length(path) == 1) {
    n_move <- c(path[1], 0)
  }
  else {
    n_move <- c(path[1], path[2])
  }
  return (n_move)
}

# Breadth-first search algorithm
breadthFirstSearch <- function(positions, edges, target) {
  known <- rep(0, 40)
  unknown <- rep(0, 40)
  dist <- rep(Inf, 40)
  goal <- positions[3]
  dist[goal] <- 0
  frontier <- c(goal)
  
  while (length(frontier) > 0) {
    closest <- frontier[1]
    frontier <- frontier[-1]
    if (closest %in% known){
      next
    }else if(closest == target){
      break
    }else{
      known[closest] <-  closest
    }
    neighbours <- getOptions(closest, edges)
    neighbours <- neighbours[-which(neighbours==closest)]
    for (n in neighbours) {
      alt_distance <- dist[closest] + 1
      if (alt_distance < dist[n]) {
        dist[n] <- alt_distance
        unknown[n] <- closest
        frontier <- c(frontier, n)
      }
    }
  }
  path <- vector()
  next_n <- target
  while (next_n != goal) {
    path <- c(next_n, path)
    next_n <- unknown[next_n]
  }
  return (path)
}

# Function for generating a transition probability matrix
transitionProbabilityMatrix <- function(edges) {
  trans_mat <- matrix(0, nrow = 40, ncol = 40)
  for (n in 1:40) {
    neighbours <- getOptions(n, edges)
    trans_prob <- (1/(length(neighbours)))
    for (neighbour in neighbours) {
      trans_mat[n, neighbour] <- trans_prob
    }
  }
  return (trans_mat)
}

# Funtion for calculating forward probability states
forwardProbabilityStates <- function(moveDat, readings, edges, probs) {
  x_probs <- moveDat$mem$states
  obs <- observationalProbabilities(readings, probs)
  trans_mat <- transitionProbabilityMatrix(edges)
  c_probs <- x_probs%*%trans_mat%*%obs
  c_probs <- c_probs/sum(c_probs)
  return (c_probs) 
}

# Hidden Markov Model function for WheresCroc
myHmm <- function(moveDat,readings,positions,edges,probs) {
  
  # Control function to avoid sending null-value in moveDat to the forwardProbabilityStates function
  if (moveDat$mem$status == 0) {
    moveDat$mem <- list(status = 1, states = rep(1/40, 40))
  }

  f_states <- forwardProbabilityStates(moveDat, readings, edges, probs)
  if (length(moveDat$moves) > 0) {
    if (moveDat$moves[2] == 0) {
      f_states[positions[3]] <- 0
      f_states <- f_states/sum(f_states)
    }
  }
  
  # If/else conditions for tourist 1&2 conditions: [ 1 alive, 2 dead | 1 dead, 2 alive | 1,2 alive ]
  if (positions[1] < 0 & !is.na(positions[1])) {
    f_states <- matrix(rep(0,40), ncol = 40)
    f_states[(-1)*positions[1]] <- 1
  }
  else if (positions[2] < 0 & !is.na(positions[2])) {
    f_states <- matrix(rep(0,40), ncol = 40)
    f_states[(-1)*positions[2]] <- 1
  }
  else  {
    f_states[positions[1]] <- 0
    f_states[positions[2]] <- 0
    f_states <- (f_states/sum(f_states))
  }
  n_move <- nextMove(f_states, positions, edges)
  moveDat$moves <- n_move
  moveDat$mem$states <- f_states
  return(moveDat)
}

testWC(myHmm, verbose = 1, returnVec = FALSE, n = 500, seed = 21, timeLimit = 300)
