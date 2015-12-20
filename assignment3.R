## Assignment 3
# Using simulated annealing

require(Rcpp)
require(RcppArmadillo)

system.time(
  rescpp <- ska(objWeight = testW,
                testV,
                maxW,
                10000,
                1000000)
)
