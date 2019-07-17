library(testit)
#todo: handle probability of zero explicitly! (if density is exactly zero for certain)
# values, this implementation breaks!

# log-space implementation for discrete timetime-homogeneous HMM
# notation:
# delta: row vector of initial probabilities; should equal stationary distribution
# gamma: matrix of transition probabilities: \gamma_{ij} : prob . of transition 
# from state i to j
# P: row vector of functions; each function is the pdf or pmf of a state's 
# associated distribution
# observations: data frame consisting of obs (observations) and time (absolute times)

# this algorithm runs in O(m^T); beware! 
estimLogProb <- function(delta, gamma, P, obs){
  dims <- dim(obs)
  n <- dims[1]
  m <- length(delta)
  
  #we work in differences
  obs$time <- c(obs$time[1], diff(obs$time))
  
  # initial distribution
  # \tilde{\alpha}
  alpha_t <- log(delta)
  
  # work around the corner case that the first observation is at t=0
  # this will break the log decomposition as log(0) is undefined and gamma^0 
  # is id(m).
  # this is achieved by manually contracting the first observation's probabilities
  # into alpha. 
  if(obs$time[1] == 0){
    probs <- sapply(P, function(f){ f(obs$obs[1])})
    P_v <- diag(probs)
    alpha_t <- log(delta %*% P_v)
    
    # remove first observation
    obs <- obs[2:n, ]
    dims <- dim(obs)
    n <- dims[1]
  }
  
  for (t in 1:n){
      probs <- sapply(P,
                      function(f){ f(obs$obs[t])})
      P_v <- diag(probs)
      beta <- matPow(gamma, obs$time[t]) %*% P_v
      
      # find maximum alpha to apply formula
      maxA_t <- which.max(alpha_t)
      
      # alpha_t(k)
      newAlpha_t <- rep.int(-Inf, m)
      for(k in 1:m){
        newAlpha_t[k] <- alpha_t[maxA_t]  + log(beta[maxA_t, k])
        
        # for log(1 + x), calculate x
        x <- -Inf
        for(j in 1:m){
          if(j != maxA_t){
            x <- addIfNotInfty(x, exp( alpha_t[j] + log(beta[j, k]) - alpha_t[maxA_t] 
                          - log(beta[maxA_t, k])))
          }
        }
        newAlpha_t[k] <- addIfNotInfty(newAlpha_t[k], suppressWarnings(log1p(x)))
      }
      alpha_t <- newAlpha_t
  }
  #alpha_t %*% t(t(rep.int(1, m)))
  logSum(selectIfFinite(alpha_t))
}

addIfNotInfty <- function(no1, no2){
  if(!is.finite(no2))
    no1
  else if(!is.finite(no1))
    no2
  else
    no1 + no2
}

# returns zero iff no is infinity/-infinity
# used to discard log(0) values by not adding them up
zeroIfInfty <- function(no){
  if(is.finite(no)){
    no
  }
  else{
    0
  }
}

selectIfFinite <- function(x){
  Filter(is.finite, x)
}

# source: 
# https://en.wikipedia.org/wiki/List_of_logarithmic_identities#Summation/subtraction
# transforms (log(a_1), log(a_2), ...) into log(sum(a_1, a_2, ...))
logSum <- function(log_a){
  maxA <- which.max(log_a)
  
  res <- log_a[maxA]
  if(length(log_a) == 1)
    res
  else{
    x <- sum(map(log_a[-maxA], function(log_a_i){
      exp(log_a_i - log_a[maxA])
    }))
    
    res + log1p(x)
  }
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