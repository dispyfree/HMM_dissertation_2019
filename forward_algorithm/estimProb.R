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
  
  alpha_0 <- delta
  alpha_t <- fold(1:n, function(t, acc){
    probs <- sapply(P, function(f){ f(obs$obs[t])})
    P_v <- diag(probs)
    
    acc %*% matPow(gamma, obs$time[t]) %*% P_v
  }, alpha_0)
  
  likelihood <- alpha_t %*% t(t(rep.int(1, length(delta))))
  likelihood
}

# checks consistency of distributions (either initial/stationary distribution or
# rows in \gamma)
checkConsistency <- function(delta){
  # all rows sum to one
  assert(all(rowSums(delta) == 1))
  # value are within [0,1]
  assert(all(delta >= 0 & delta <= 1))
}


matPow <- function(mat, p){
  if(p  == 0){
    mat
  }else{
    matPow(mat, p-1) %*% mat
  }
  
}
