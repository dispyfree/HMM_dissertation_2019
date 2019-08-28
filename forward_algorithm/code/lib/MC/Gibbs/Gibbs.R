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
sampleHiddenStates <- function(theta, P_dens, obs, hs){
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
    # todo: which one to choose?
    probs <- getBackwardProbs(alphas[i,], theta$gamma, lastState)
    # probs <- rep.int(0, length(alphas[1, ]))
    # if(i > 1 && !is.na(hs)){
    #   probs <- probs + log(theta$gamma[hs[i-1], ])
    # }
    # probs <- probs + log(theta$gamma[, lastState])
    # probs <- probs + log(sapply(P_dens, function(f){ f(obs$obs[i])}))
    # 
     newState <- rdiscrete(1, logProbToRatio(probs))
    
    states <- c(newState, states)
    lastState <- newState
  }
  
  #print(sum(states == 1) / length(states))
  #print("----------------------------")
  
  states
}

# accepts vector of log(a), log(b), log(c), ... 
# 
# TODO: check mathematically whether this makes sense!!!
logProbToRatio <- function(dist){
  tot <- logSum(dist)
  normalSpace <- exp(dist - tot)
  normalSpace / sum(normalSpace)
}


# returns: 
# P(X^t = x^t, C_t) * P(C_{t+1} | C_t) = \alpha_t(i) * \gamma{i, C_{t+1}}
# nextState is hence C_{t+1}
getBackwardProbs <- function(alpha, gamma, nextState){
  alpha + log(gamma[, nextState])
}


samplePoissonTheta  <- function (theta, m, hiddenStates, obs, oldDelta, noPriorRuns, taus){
  T <- length(obs$obs)
  # uniform over all distributions
  alphaPrior <- rep.int(1.0 / m, m)
  
  oldDelta <- oldDelta * noPriorRuns
  oldDelta[hiddenStates[1]] <- oldDelta[hiddenStates[1]] + 1
  oldDelta <- oldDelta / (noPriorRuns + 1)
  
  # according to scott:
  # 1) sample hidden states
  # 2) sample contributions 
  # 3) resample taus
  contrib <- sampleContributions(T, m, hiddenStates, obs, taus)
  taus <- sampleTaus(contrib, hiddenStates, m)
  newGamma <- sampleGamma(m, hiddenStates, alphaPrior)

  list("gamma" = newGamma, 
       "statePara" =  taus, # taus,  #c(0.9, 0.3)
       "noRuns" = noPriorRuns + 1,
       "delta" = oldDelta #c(1.0, 0.5) / 1.5
  )
}

sampleTaus <- function(contributions, hiddenStates, m){
  sampledTaus <- rep.int(0, m)
  for(hiddenState in 1:m){
    # sum of all contributions
    allContrib <- sum(contributions[, hiddenState])
    # no of activations of this tau
    noActivations <- sum(tail(hiddenStates, -1) >= hiddenState)
    
    sampledTaus[hiddenState] <- rgamma(1, 1 + allContrib, 1 + noActivations)
  }
  sampledTaus
}

sampleContributions <- function(T, s, hiddenStates, obs, taus){
  contrib <- matrix(rep.int(0, T*s), nrow = T, ncol = s)
  # first hidden state does not produce an observation
  obsHiddenStates <- tail(hiddenStates, -1)
  for(i in 1:T){
    h_t <- obsHiddenStates[i]
    activeTaus <- taus[1:h_t]
    contrib[i, 1:length(activeTaus)] <- rmultinom(1, obs$obs[i], activeTaus)
  }
  contrib
}



# delta, gamma, P_dens, obs 
# f: list of functions, namely:
#    getInitialTheta
#    buildDensity
#    sampleTheta
GibbsSampler <- function(m, obs, f, minConvLimit){
  n <- 0
  
  # initialize uniformly 
  theta <- f$getInitialTheta(m)
  progress <- createProgress(m)
  currentLimit <- 100
  minRuns <- 100
  deviations <- c()
  
  while(n < minRuns || (currentLimit > minConvLimit && n < f$maxRuns)){
    P_dens    <- f$buildDensity(theta$statePara)
    hiddenStates <- sampleHiddenStates(theta, P_dens, obs)
    theta <- f$sampleTheta(m, hiddenStates, obs, theta, n)
    
    progress <- thetaToProgress(progress, theta, 0)
    
    # R complains if is.na is applied to a closure
    if(suppressWarnings(!is.na(f$progressCallback))){
      f$progressCallback(n, theta, progress, hiddenStates, list())
    }
    
    # track progress of quantiles
    if(n > 0){
      q <- progressToQuantiles(progress, m)
      if(n == 1){
        quantileProgress <- c(q$secondQuant, q$thirdQuant)
      }
      else if( n > 1){
        quantileProgress <- rbind(quantileProgress, c(q$secondQuant, q$thirdQuant))
      }
      mDev <- getMaxQuantDeviation(q)
      deviations <- c(deviations, mDev)
      
      if(n > 1){
        currentLimit <- mDev
      }
      print(paste0(mDev, ' (<', minConvLimit, ')'))
    }
    
    n <- n + 1
  }
  
  list("theta" = theta, "progress" = progress, "deviations" = deviations)
}


# simple estimates Bernoulli with the average of observations for each state
gibbs.sampleBernoulli <- function(m, hiddenStates, obs, f){
  probs <- rep(0.0, m)
  # the initial distribution doesn't have an attached observation
  # and hence is disregarded when sampling Bernoulli variables 
  t <- tail(hiddenStates, -1)
  
  # m parameters are estimated in \Gamma before
  nfp <- f$noFixedParams - m
  for(state in 1:m){
    # extract relevant observations and their probabilities
    regimes <- t == state
    
    n <- sum(regimes)
    k <- sum(obs$obs[regimes])
    estimP <- sum(obs$obs[regimes]) / sum(regimes)
    #using uniform prior, we can just use this estimate
    if(state > nfp){
      probs[state] <- rbeta(1, k+1, n-k+1)
    }
    else{
      probs[state] <- f$origTheta$statePara[state]
    }
  }
  
  
  # enforce increasing ps
  probs
}



# samples gamma, bernoulli and delta in one go
gibbs.sampleBernoulliTheta  <- function (m, hiddenStates, obs, theta, noPriorRuns){
  # uniform over all distributions
  alphaPrior <- rep.int(1.0 / m, m)
  

  delta <- rep.int(0, m)
  delta[hiddenStates[1]] <- 1
  
  list("gamma" = gibbs.sampleGamma(m, hiddenStates, alphaPrior, f), 
       "statePara" = gibbs.sampleBernoulli(m, hiddenStates, obs, f), 
       "noRuns" = noPriorRuns + 1,
       "delta" = delta
  )
}

# estimates Gamma using the given estimate of hidden states
# then updates it by means of weighted Dirichlet distribution
gibbs.sampleGamma <- function(m, hiddenStates, prior, f){
  eGamma <- estimGamma(m, hiddenStates)
  sample <- replicate(m, prior)
  
  # sample new transition probabilities
  gamma <- matrix(rep.int(0, m*m), nrow = m)
  nfp <- f$noFixedParams
  
  for(i  in 1:m){
    if(i <= nfp){
      # fix ith parameter and only resample the remainder
      remainingWeight <- 1 - f$origTheta$gamma[i, i]
      gamma[i, ][-i] <- remainingWeight * rdirichlet(1, 10 * (sample[i, ][-i] +  eGamma[i, ][-i]))
      gamma[i,i] <- f$origTheta$gamma[i, i]
    }
    else{
      gamma[i, ] <- rdirichlet(1, 10 * (sample[i, ] +  eGamma[i, ]))
    }
  }
  
  gamma 
}
