library(testit)
library(lambda.tools)

# naiive implementation for discrete timetime-homogeneous HMM
# notation:
# delta: row vector of initial probabilities; should equal stationary distribution
# gamma: matrix of transition probabilities: \gamma_{ij} : prob . of transition 
# from state i to j
# P: row vector of functions; each function is the pdf or pmf of a state's 
# associated distribution
# observations: data frame consisting of obs (observations) and time (absolute times)
estimProb <- function(delta, gamma, P, obs){
  alpha_t <- estimAlpha(delta, gamma, P, obs,length(obs$obs))
  likelihood <- alpha_t %*% t(t(rep.int(1, length(delta))))
  likelihood
}

# calculates alpha up to timestep t
# alpha as defined in Zucchini
estimAlpha <- function(delta, gamma, P, obs, finalT){
  dims <- dim(obs)
  n <- dims[1]
  
  # convert observation times to differences
  obs$time <- c(obs$time[1], diff(obs$time))
  
  alpha_0 <- delta
  alpha_t <- fold(1:finalT, function(t, acc){
    probs <- sapply(P, function(f){ f(obs$obs[t])})
    P_v <- diag(probs)
    
    acc %*% matPow(gamma, obs$time[t]) %*% P_v
  }, alpha_0)
  alpha_t
}

# beta as defined in Zucchini
estimBeta <- function(delta, gamma, P, obs, startT){
  dims <- dim(obs)
  n <- dims[1]
  m <- length(delta)
  
  # convert observation times to differences
  obs$time <- c(obs$time[1], diff(obs$time))
  beta_0 <- t(t(rep.int(1, length(delta))))
  
  
  beta_t <- fold(startT:n, function(t, acc){
    probs <- sapply(P, function(f){ f(obs$obs[t])})
    P_v <- diag(probs)
    
     matPow(gamma, obs$time[t]) %*% P_v %*% acc
  }, beta_0);
  beta_t
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
  assert(p >= 0)
  if(p == 0){
    dims <- dim(mat)
    diag(dims[1])
  }
  else if(p  == 1){
    mat
  }else{
    matPow(mat, p-1) %*% mat
  }
  
}
