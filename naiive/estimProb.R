library(testit)
library(lambda.tools)

# naiive implementation for discrete timetime-homogeneous HMM
# notation:
# delta: row vector of initial probabilities; should equal stationary distribution
# gamma: matrix of transition probabilities: \gamma_{ij} : prob . of transition 
# from state i to j
# P: row vector of functions; each function is the pdf or pmf of a state's 
# associated distribution
# observations: data frame consisting of obs (observations) and (the 
# offset in time of the associated observation with respect to its predecessor)
estimProb <- function(delta, gamma, P, obs){
  n <- length(obs)
  
  # create P-matrices
  P_v <- sapply(obs$obs, function(x){
    probs <- sapply(obs, P)
    id <- diag(probs)
    id
  })
  
  # take observation times into account
  gamma_probs <- apply(obs$time, 2, function(t){
    gamma^t
  })
  # 1^'
  gamma_probs <- c(gamma_probs, t(rep.int(1, n)))
  
  #multiply 
  tmp <- P_v %*% gamma_probs

  fold(tmp, function(a, b){ a %*% b}, delta)
  
}

# checks consistency of distributions (either initial/stationary distribution or
# rows in \gamma)
checkConsistency <- function(delta){
  # all rows sum to one
  assert(all(rowSums(delta) == 1))
  # value are within [0,1]
  assert(all(delta >= 0 & delta <= 1))
}