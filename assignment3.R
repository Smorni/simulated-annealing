## Assignment 3
# Using simulated annealing

require(Rcpp)
require(RcppArmadillo)
require(adagio)

p <- c(15, 100, 90, 60, 40, 15, 10,  1)
w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
cap <- 102

p <- c(70, 20, 39, 37, 7, 5, 10)
w <- c(31, 10, 20, 19, 4, 3,  6)
cap <- 50

system.time(
  rescpp <- ska(w,
                p,
                cap,
                10000,
                1000000)
)

system.time(
  resada <- knapsack(w,
                     p,
                     cap)
)


