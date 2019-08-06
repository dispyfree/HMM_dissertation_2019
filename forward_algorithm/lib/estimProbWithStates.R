# calculates P(X_1 = x_1, C_1 = c_1, X_2 = x_2, C_2 = c_2, \dots )
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

# calculates log(P(X_1 = x_1, C_1 = c_1, X_2 = x_2, C_2 = c_2, \dots ))
estimLogProbWithStates <- function(delta, gamma, P_dens, obs, states){
  prob <- log(delta[states[1]])
  
  for(t in 2:length(obs$obs)){
    lastState <- states[t-1]
    currentOb <- obs$obs[t]
    newState <- states[t]
    
    prob <- prob + log(gamma[lastState, newState]) + log(P_dens[[newState]](currentOb))
  }
  prob
} 

