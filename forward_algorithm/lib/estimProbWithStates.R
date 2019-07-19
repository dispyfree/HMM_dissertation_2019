

estimProbWithStates <- function(delta, gamma, P_dens, obs, states){
  prob <- delta[states[1]]
  
  for(t in 2:length(obs$obs)){
    lastState <- states[t-1]
    currentOb <- obs$obs[t]
    newState <- states[t]
    
    prob <- prob * gamma[lastState, newState] * P_dens[[newState]](currentOb)
  }
  prob
} 