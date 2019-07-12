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
    alpha_t <- log(delta * P_v)
    
    # remove first observation
    obs <- obs[2:n, ]
  }
  
  for (t in 1:n){
      probs <- sapply(P, function(f){ f(obs$obs[t])})
      P_v <- diag(probs)
      beta <- matPow(gamma, obs$time[t]) %*% P_v
      
      # find maximum alpha to apply formula
      maxA_t <- which.max(alpha_t)
      
      # alpha_t(k)
      newAlpha_t <- rep.int(0, m)
      for(k in 1:m){
        newAlpha_t[k] <- alpha_t[maxA_t]  + log(beta[maxA_t, k])
        
        # for exp(1 + x), calculate x
        x <- 0
        for(j in 1:m){
          if(j != maxA_t){
            x <- x + exp( alpha_t[j] + log(beta[j, k]) - alpha_t[maxA_t] 
                          - log(beta[maxA_t, k]))
          }
        }
        newAlpha_t[k] <- newAlpha_t[k] + log1p(x)
      }
      alpha_t <- newAlpha_t
  }
  exp(alpha_t) %*% t(t(rep.int(1, m)))
}