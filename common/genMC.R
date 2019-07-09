library(e1071)

# generates a (Markov) chain of realisations for the given HMM
# u: row vector, probability distribution over initial state
# gamma: transition matrix, (i,j) => probability of moving from state i to state j
# P: row vector of functions drawing a random value for the respective state
# T: number of samples from HMM

# output: list(states = c(...), obs = c(...)) 
# where obs are observations
genMC <- function(u, gamma, P, T){
  
  currentState <- rdiscrete(1, u)
  states <- c(currentState)
  obs <- c(P[[currentState]]())
  
  for (t in 1:T){
    currentState <- rdiscrete(1, gamma[currentState, ])
    states <- c(states, currentState)
    obs <- c(obs, P[[currentState]]())
  }
  
  list(states = states, obs = obs)
}


# generates dummy weather model
# state 1: low pressure, state 2: high pressure
# observation 1: rain, obs 2: sunshine

# use stationary distribution of transition matrix
u1 <- c(0.5, 1) / 1.5
gamma1 <- matrix(c(0.8, 0.2, 0.1, 0.9), nrow = 2, byrow = TRUE)
P <- c(function(){
          rdiscrete(1, c(0.7, 0.3))
},
       function(){
         rdiscrete(1, c(0.1, 0.9))
       }
)

#sampleReal <- genMC(u1, gamma1, P, 1000)
#write.csv(sampleReal, file='rainySample.csv')