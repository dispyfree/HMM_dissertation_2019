library(e1071)

# generates a (Markov) chain of realisations for the given HMM
# u: row vector, probability distribution over initial state
# gamma: transition matrix, (i,j) => probability of moving from state i to state j
# P: row vector of functions drawing a random value for the respective state
# T: number of samples from HMM

# output: list(states = c(...), obs = c(...), prob = number)
# prob: overall probability of this sample
# where obs are observations
genMC <- function(u, gamma, P, P_density, T){
  
  prob <- 1.0
  currentState <- rdiscrete(1, u)
  states <- c(currentState)
  newObserv <- P[[currentState]]()
  obs <- c(newObserv)
  
  prob <- prob * ddiscrete(currentState, u)
  prob <- prob * P_density[[currentState]](newObserv)
  
  for (t in 1:T){
    oldState <- currentState
    oldProb <- tail(prob, n=1)
    currentState <- rdiscrete(1, gamma[currentState, ])
    states <- c(states, currentState)
    
    newObserv <- P[[currentState]]()
    obs <- c(obs, newObserv)

    newProb <- oldProb * ddiscrete(currentState, gamma[oldState, ])
    newProb <- oldProb * P_density[[currentState]](newObserv)
    prob <- c(prob, newProb)
  }
  
  list(states = states, obs = obs, prob = prob)
}


# generates dummy weather model
# state 1: high pressure, state 2: low pressure
# observation 1: sunshine, obs 2: rain

# use stationary distribution of transition matrix
u1 <- c(1.0, 0.5) / 1.5
gamma1 <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
P <- c(function(){
          rdiscrete(1, c(0.9, 0.1))
},
       function(){
         rdiscrete(1, c(0.3, 0.7))
       }
)

P_density <- c(function(x){
  ddiscrete(x, c(0.9, 0.1))
},
function(x){
  ddiscrete(x, c(0.3, 0.7))
}
)

sampleReal <- genMC(u1, gamma1, P, P_density, 100)
#write.csv(sampleReal, file='rainySample.csv')