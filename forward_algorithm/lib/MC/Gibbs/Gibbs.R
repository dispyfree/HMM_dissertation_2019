source('lib/MC/common/utility.R')
source('lib/estimLogProb.R')
library(e1071)
library(purrr)
library(gtools)
library(testit)
library(ggplot2)

#gtools overwrites assert... what a bad practice. 
assert <- testit::assert

# samples states by drawing last state randomly using alpha_T
# then samples backwards by using 
# P(X^t = x^t, C_t) * P(C_{t+1} | C_t) = \alpha_t(i) * \gamma{i, C_{t+1}}
# method adopted from Zucchini
sampleHiddenStates <- function(theta, P_dens, obs){
  probs <- estimTLogProb(theta, P_dens, obs)
  alphas <- probs$alphas
  
  dims <- dim(alphas)
  # one more alphas than observations due to initial distribution
  n <- dims[1]
  
  # sample state from last alpha
  # working in log space, consider
  # log(p_1/p_2) = log(p_1) - log(p_2)
  # => exp(log(p_1) - p_2) = p_1/p_2
  # only consider ratio and draw from that
  lastState <- tail(alphas, 1)
  probs <- logProbToRatio(lastState)
  lastState <- rdiscrete(1, probs)
  
  # build in reverse order
  states <- c(lastState)
  
  for(i in (n - 1) : 1){
    probs <- getBackwardProbs(alphas[i,], theta$gamma, lastState)
    newState <- rdiscrete(1, logProbToRatio(probs))
    
    states <- c(newState, states)
    lastState <- newState
  }
  
  #print(sum(states == 1) / length(states))
  #print("----------------------------")
  
  states
}

# accepts vector of log(a), log(b), log(c), ... 
# returns a, 1-a 
logProbToRatio <- function(dist){
  rat <- exp(dist[2] - dist[1])
  p1Prob <- 1.0 / (1.0 + rat)
  c(p1Prob, 1 - p1Prob)
}


# returns: 
# P(X^t = x^t, C_t) * P(C_{t+1} | C_t) = \alpha_t(i) * \gamma{i, C_{t+1}}
# nextState is hence C_{t+1}
getBackwardProbs <- function(alpha, gamma, nextState){
  alpha + log(gamma[, nextState])
}


# estimates Gamma using the given estimate of hidden states
# then updates it by means of weighted Dirichlet distribution
sampleGamma <- function(m, hiddenStates, prior){
  eGamma <- estimGamma(m, hiddenStates)
  sample <- replicate(m, prior)
  
  # sample new transition probabilities
  gamma <- matrix(rep.int(0, m*m), nrow = m)
  for(i  in 1:m){
    print(sample[i, ])
    print(eGamma[i, ])
    gamma[i, ] <- rdirichlet(1, 10 * (sample[i, ] +  eGamma[i, ]))
  }
  gamma 
}


# estimates gamma naiively by given chain of hiddenStates
estimGamma <- function(m, hiddenStates){
  trans <- matrix(rep.int(0, m*m), nrow = m)
  
  for(i in 1: (length(hiddenStates) - 1)){
    j <- hiddenStates[i]
    k <- hiddenStates[i+1]
    trans[j, k] <- trans[j, k] + 1
  }
  # convert rows to probabilities
  for(i in 1:m){
    s <- sum(trans[i, ])
    # if we did not observe state, set transitions uniformly
    if(s == 0){
      trans[i, ] <- rep.int(1.0/m, m)
    }
    else{
      trans[i, ] <- trans[i, ] #/ sum(trans[i, ])
    }
  }
  
  trans
}


# simple estimates Bernoulli with the average of observations for each state
sampleBernoulli <- function(m, hiddenStates, obs){
  probs <- rep(0.0, m)
  # the initial distribution doesn't have an attached observation
  # and hence is disregarded when sampling Bernoulli variables 
  t <- tail(hiddenStates, -1)
  for(state in 1:m){
    # extract relevant observations and their probabilities
    regimes <- t == state

    estimP <- sum(obs$obs[regimes]) / sum(regimes)
    #using uniform prior, we can just use this estimate
    probs[state] <- estimP
  }
  # enforce increasing ps
  probs
}

# samples gamma, bernoulli and delta in one go
sampleTheta  <- function (m, hiddenStates, obs, oldDelta, noPriorRuns){
  # uniform over all distributions
  alphaPrior <- rep.int(1.0 / m, m)
  
  oldDelta <- oldDelta * noPriorRuns
  oldDelta[hiddenStates[1]] <- oldDelta[hiddenStates[1]] + 1
  oldDelta <- oldDelta / (noPriorRuns + 1)
  sampleBernoulli(m, hiddenStates, obs)
  list("gamma" = sampleGamma(m, hiddenStates, alphaPrior), 
       "statePara" = sampleBernoulli(m, hiddenStates, obs),  #c(0.9, 0.3), 
       "noRuns" = noPriorRuns + 1,
       "delta" = oldDelta #c(1.0, 0.5) / 1.5
       )
}
