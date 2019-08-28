source('lib/MC/MetropolisHastings/MH.R')

#delta, gamma, P_dens, obs 
directGibbsSampler <- function(obs){
  m <- length(delta)
  n <- length(obs$obs)
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


getInitialStateEstimate <- function(P_dens, obs){
  
  
}