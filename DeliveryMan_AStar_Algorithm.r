# By: Hugo Swenson, Melvin Perez
# 2020-09-30

library(DeliveryMan)

mov = c(4,6,8,2)
h_change = c(-1,1,0,0)
v_change = c(0,0,1,-1)

gCost = function(roads, x, y, next_x, next_y) {
  if ((x - next_x) == 1) return(roads$hroads[x-1, y])
  if ((x - next_x) == -1) return(roads$hroads[x, y])
  if ((y - next_y) == 1) return(roads$vroads[x, y-1])
  return(roads$vroads[x,y])
}
  
manhattanDistance = function(start_x,start_y,goal_x,goal_y) {
  return(abs(start_x-goal_x) + abs(start_y-goal_y))
}

packageFinder = function(road, car, package) {
  x = car$x
  y = car$y
  best_dist = Inf
  ind = 0 
  dist_vec = vector()
  for (i in 1:5) {
    if (package[i,5] != 0) next
    h_cost = manhattanDistance(x, y, package[i,1], package[i,2])
    dist_vec = c(dist_vec, h_cost)
    if (h_cost < best_dist) {
      best_dist = h_cost
      ind = i
    }
  }
  return(ind)
}

nextInd = function(next_x, next_y, xs, ys) {
  for (i in 1:length(xs)) {
    if (xs[i] == next_x & ys[i] == next_y)
      return(i)
  }
  return(0)
}

nextMove = function(roads, goal_x, goal_y, xs, ys, costs, used, path) {
  while(TRUE) {
    ind = 0
    best_f = Inf  
    
    for (i in 1:length(xs)) {
    if (used[i]) next
    if (xs[i] == goal_x & ys[i] == goal_y) {
      return(path[i])
    }
    
    f_score = manhattanDistance(xs[i], ys[i], goal_x, goal_y) + costs[i]
    if (f_score < best_f) {
      best_f = f_score
      ind = i
    }
  }
  
  used[ind] = TRUE
  for (i in 1:4) {
    next_x = xs[ind] + h_change[i]
    next_y = ys[ind] + v_change[i]
    
    if (next_x > 10 | next_x < 1 | next_y < 1 | next_y > 10) next
    
    next_i = nextInd(next_x, next_y, xs, ys)
    
    if (next_i == 0) {
      xs = c(next_x, xs)
      ys = c(next_y, ys)
      used = c(FALSE, used)
      costs = c(Inf, costs)
      path = c(5, path)
      next_i = 1
      ind = ind + 1
    }
    
    next_fs = costs[ind] + gCost(roads, xs[ind], ys[ind], next_x, next_y)
    if (next_fs >= costs[next_i]) next
    
    costs[next_i] = next_fs
    path[next_i] = path[ind]
    
    if (path[next_i] != 5) next
    path[next_i] = mov[i]
    }
  }
}

dmFun = function(roads, car, packages) {
  nextMove = 0
  toGo = 0
  offset = 0
  if (car$load == 0) {
    toGo = packageFinder(roads, car, packages)
  } else {
    toGo = car$load
    offset = 2
  }
  goal_x = packages[toGo, 1+offset]
  goal_y = packages[toGo, 2+offset]
  x = car$x
  y = car$y
  car$nextMove = nextMove(roads, goal_x, goal_y, c(x), c(y), c(0), c(FALSE), c(5))
  car$mem = list()
  return(car)
}

testDM(dmFun, verbose = 0, returnVec = FALSE, n = 500, seed = 21,
       timeLimit = 250)