library(tictoc)
library(testit)
source('lib/MC/common/common.R')
source('lib/estimLogProb.R')

# samples gamma by altering with sample drawn from normal distribution
# first draws _one_ row to alter; then alters _solely_ this row
# always returns valid distributions
mh.sampleGamma <- function(gamma){
  dims <- dim(gamma)
  n <- dims[1]
  sd <- 0.05
  oldGamma <- gamma
  
  rtu <- rdiscrete(1, rep.int(1, n) / n)
  
  # adding zeroSums retains the invariant \sum \gamma_{i,} = 1.0
  gamma[rtu, ] <- gamma[rtu, ] + normaliseToZeroSum(rnorm(n, mean=0, sd=sd))
  gamma[rtu, ] <- normaliseTo01Sum1(gamma[rtu, ])
  gamma
}


# samples gamma, bernoulli and delta in one go
mh.sampleBernoulliTheta  <- function (theta, obs, noPriorRuns){
  m <- length(theta$delta)
  # uniform over all distributions
  alphaPrior <- rep(1.0 / m, m)
  
  list("gamma" = mh.sampleGamma(theta$gamma), 
       "statePara" = mh.sampleBernoulli(theta$statePara), 
       "noRuns" = noPriorRuns + 1,
       "delta" = mh.sampleDelta(theta$delta) 
  )
}

# samples by altering probs with normal distribution
# always returns a valid distribution
mh.sampleBernoulli <- function(probs){
  sd <- 0.03
  testit::assert(all(probs >= 0 & probs <= 1))
  
  m <- length(probs)
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  probs <- sapply(probs, normaliseTo01)
  testit::assert(all(probs >= 0 & probs <= 1))
  probs
}


mh.sampleDelta <- function(probs){
  m <- length(probs)
  sd <- 0.03
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  probs <- sapply(probs, normaliseTo01)
  testit::assert(all(probs >= 0 & probs <= 1))
  probs / sum(probs)
}



# delta, gamma, P_dens, obs 
directMHSampler <- function(m, obs, f, runs){
  n <- length(obs$obs)
  
  theta <- f$getInitialTheta(m)
  progress <- createProgress(m)
  
  for(n in 1:runs){
    newTheta <- f$sampleTheta(theta, obs, n)
    newP_dens <- f$buildDensity(newTheta$statePara)
    P_dens    <- f$buildDensity(theta$statePara)
    
    newProb <- estimTLogProb(newTheta, newP_dens, obs)
    oldProb <- estimTLogProb(theta, P_dens, obs)
    
    alpha <- min(0, newProb$logSum - oldProb$logSum)
    
    #accept
    if(log(runif(1)) <= alpha){
      theta <- newTheta
      progress <- thetaToProgress(progress, newTheta, TRUE)
    }else{
      progress <- thetaToProgress(progress, theta, FALSE)
    }
    
    # R complains if is.na is applied to a closure
    if(suppressWarnings(!is.na(f$progressCallback))){
      extra = list("newLogSum" = newProb$logSum)
      f$progressCallback(n, theta, progress, hiddenStates, extra)
    }
    
    print(newProb$logSum)
    print(theta$statePara)
  }
  
  list("theta" = theta, "progress" = progress)
}