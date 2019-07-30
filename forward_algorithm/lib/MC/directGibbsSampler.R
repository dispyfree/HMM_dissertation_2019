library('MCMCpack')
source('lib/estimProb.R')
source('lib/estimProbWithStates.R')

# implementation of a direct-Gibbs (DG) sampler for the state-space

directGibbsSampler <- function(delta, gamma, P_dens, obs){
  m <- length(delta)
  n <- length(obs$obs)
  origStates <- obs$states
  states <- getInitialStateEstimate(P_dens, obs);
  
  stateHistory <- matrix(data=states, ncol = length(states))
  
  gamma <- drawRandomGamma(m);
  runs <- 2000
  distances <- c()
  probs <- c()
  for(n in 1:runs){
    states <- sampleStates(delta, gamma, P_dens, obs, states)
    gamma <- gammaMHStep(delta, gamma, P_dens, obs, states)
    prob <- estimProbWithStates(delta, gamma, P_dens, obs, states)
    probs <- c(probs, prob)
    stateHistory <- rbind(stateHistory, states)
    print(prob)
    
    estimatedStates <- apply(stateHistory, 2, mean)
    dist <- sum((estimatedStates - origStates)^2)
    print(dist)
    distances <- c(distances, dist)
  }
  list("prob" = prob, "probs" = probs, "dists" = distances, 
       "stateHistory" = stateHistory, "estimGamma" = gamma)
}

extractMaxFromHistory <- function(stateHistory, noStates){
  dims <- dim(stateHistory)
  n    <- dims[2]
  states <- c()
  for(t in 1:n){
    counts <- rep.int(0, noStates)
    for(state in 1:noStates){
      counts[state] <- sum(stateHistory[, t] == state)
    }
    states <- c(states, which.max(counts))
  }
  states
}

# samples states from end to beginning, one run
sampleStates <- function(delta, gamma, P_dens, obs, states){
  n <- length(obs$obs)
  m <- length(delta)
  for(t in n:1){
    # alpha <- estimAlpha(delta, gamma, P_dens, obs, t)
    # beta <- estimBeta(delta, gamma, P_dens, obs, t)
    # 
    # combined <- alpha * t(beta)
    # #maxProbState <- which.max(combined)
    # maxProbState <- rdiscrete(1, combined)
    # states[t] <- maxProbState
    
    stateProbs <- c()
    for(s in 1:m){
      stateProb <- P_dens[[s]](obs$obs[t])
      
      if(t > 1)
        stateProb <- stateProb * gamma[states[t-1], s]
      if(t < n)
        stateProb <- stateProb * gamma[s, states[t+1]]
      
      stateProbs <- c(stateProbs, stateProb)
    }
    
    #states[t] <- which.max(stateProbs)
    states[t] <- rdiscrete(1, stateProbs)
  }
  states
}

# computes an initial estimate of the chain of states by 
# considering each observation independently, e.g.
# draw from P(H_t | X_t = x_t)
getInitialStateEstimate <- function(P_dens, obs){
  states <- c()
  for(t in 1:length(obs$obs)){
    probs <- sapply(P_dens, function(f){ f(obs$obs[t])})
    #states <- c(states, which.max(probs))
    states <- c(states, rdiscrete(1, c(0.5, 0.5)))
  }
  states
}


gammaMHStep <- function(delta, gamma, P_dens, obs, states){
  dims <- dim(gamma)
  n <- dims[1]
  sd <- 0.01
  oldGamma <- gamma
  
  rtu <- rdiscrete(1, rep.int(1, n) / n)
  
    gamma[rtu, ] <- gamma[rtu, ] + rnorm(n, mean=0, sd=sd)
    
    # if not valid, abort immediately and use only gamma
    if(any(gamma[rtu, ] < 0 | gamma[rtu, ] > 1)){
      print('rejected due to row')
      return(oldGamma)
    }
      
    # normalise so that it sums to one
    gamma[rtu, ] <- gamma[rtu, ] / sum(gamma[rtu, ])
  
  oldProb <- estimProbWithStates(delta, oldGamma, P_dens, obs, states)
  newProb <- estimProbWithStates(delta, gamma, P_dens, obs, states)
  
  alpha <- min(1, newProb / oldProb)
  
  #accept
  if(runif(1) <= alpha){
    print('accepted!');
    gamma
  }
  else{
    oldGamma
  }
}