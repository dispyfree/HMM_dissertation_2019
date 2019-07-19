
# implementation of a direct-Gibbs (DG) sampler for the state-space

directGibbsSampler <- function(delta, gamma, P_dens, obs){
  m <- length(delta)
  n <- length(obs$obs)
  origStates <- obs$states
  states <- getInitialStateEstimate(P_dens, obs);
  
  stateHistory <- matrix(data=states, ncol = length(states))
  
  runs <- 20
  distances <- c()
  for(n in 1:runs){
    states <- sampleStates(delta, gamma, P_dens, obs, states)
    prob <- estimProbWithStates(delta, gamma, P_dens, obs, states)
    stateHistory <- rbind(stateHistory, states)
    
    estimatedStates <- apply(stateHistory, 2, mean)
    dist <- sum((estimatedStates - origStates)^2)
    print(dist)
    distances <- c(distances, dist)
  }
  list("prob" = prob, "stateHistory" = stateHistory)
}

# samples states from end to beginning, one run
sampleStates <- function(delta, gamma, P_dens, obs, states){
  n <- length(obs$obs)
  for(t in n:1){
    alpha <- estimAlpha(delta, gamma, P_dens, obs, t)
    beta <- estimBeta(delta, gamma, P_dens, obs, t)
    
    combined <- alpha * t(beta)
    #maxProbState <- which.max(combined)
    maxProbState <- rdiscrete(1, combined)
    states[t] <- maxProbState
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
    states <- c(states, 1)
  }
  states
}