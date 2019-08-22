library(tictoc)
library(testit)
source('lib/MC/common/common.R')
source('lib/estimLogProb.R')

# samples gamma by altering with sample drawn from normal distribution
# first draws _one_ row to alter; then alters _solely_ this row
# always returns valid distributions
mh.sampleGamma <- function(gamma, f){
  dims <- dim(gamma)
  n <- dims[1]
  sd <- 0.08
  oldGamma <- gamma
  
  rtu <- rdiscrete(1, rep.int(1, n) / n)
  
  #noFixedParameters
  nfp <- min(n, f$noFixedParams)
  
  # adding zeroSums retains the invariant \sum \gamma_{i,} = 1.0
  # no fixing needed
  if(nfp < rtu){
    prop <- bumpTo01OrKeep(gamma[rtu, ] + rnorm(n, mean=0, sd=sd), gamma[rtu, ])
    gamma[rtu, ] <- normaliseToSum1(prop)
  }
  else{
    # draw all the other numbers
    step <- rnorm(n, mean=0, sd=sd)
    prop <- bumpTo01OrKeep(gamma[rtu, ] + step, gamma[rtu, ])
    prop[rtu] <- f$origTheta$gamma[rtu, rtu]
    # normalise the remainder so that they take up (1-prop[rtu]) of probability
    prop[-rtu] <- normaliseToSum1(prop[-rtu]) * (1 - prop[rtu])
    gamma[rtu, ] <- prop
  }
  diag(gamma)[1:nfp] <- diag(f$origTheta$gamma)[1:nfp]
    
  gamma
}


# samples gamma, bernoulli and delta in one go
mh.sampleBernoulliTheta  <- function (theta, obs, noPriorRuns, f){
  m <- length(theta$delta)
  # uniform over all distributions
  alphaPrior <- rep(1.0 / m, m)
  mh.sampleGamma(theta$gamma, f)
  list("gamma" = mh.sampleGamma(theta$gamma, f), 
       # sort to prevent label switching
       "statePara" = sort(mh.sampleBernoulli(theta$statePara, f)), 
       "noRuns" = noPriorRuns + 1,
       "delta" = mh.sampleDelta(theta$delta) 
  )
}

# samples by altering probs with normal distribution
# always returns a valid distribution
mh.sampleBernoulli <- function(probs, f){
  sd <- 0.1
  testit::assert(all(probs >= 0 & probs <= 1))
  
  m <- length(probs)
  oldProbs <- probs
  
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  #noFixedParameters
  nfp <- f$noFixedParams - m
  if(nfp > 0){
    probs[1:nfp] <- f$origTheta$statePara[1:nfp]
  }
  
  probs <- bumpTo01OrKeep(probs, oldProbs)
  testit::assert(all(probs >= 0 & probs <= 1))
  probs
}


mh.sampleDelta <- function(probs){
  m <- length(probs)
  sd <- 0.03
  oldProbs <- probs
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  probs <- normaliseToSum1(bumpTo01OrKeep(probs, oldProbs))
  testit::assert(all(probs >= 0 & probs <= 1))
  probs
}



# delta, gamma, P_dens, obs 
directMHSampler <- function(m, obs, f, convLimit){
  n <- length(obs$obs)
  
  theta <- f$getInitialTheta(m)
  progress <- createProgress(m)
  
  currentLimit <- 100
  # do not update the currentLimit if chain has not at least 200 elements
  minRuns <- 200
  n <- 0
  while(currentLimit > convLimit && n < f$maxRuns){
    newTheta <- f$sampleTheta(theta, obs, n, f)
    newP_dens <- f$buildDensity(newTheta$statePara)
    P_dens    <- f$buildDensity(theta$statePara)
    
    newProb <- estimTLogProb(newTheta, newP_dens, obs)
    oldProb <- estimTLogProb(theta, P_dens, obs)
    
    alpha <- min(0, newProb$logSum - oldProb$logSum)
    
    #accept
    if(log(runif(1)) <= alpha ){
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
    
    # track progress of quantiles
    if(n %% 10 == 0){
      q <- progressToQuantiles(progress)
      if(n == 10){
        quantileProgress <- cbind(q$secondQuant, q$thirdQuant)
      }
      else if( n >= 20){
        quantileProgress <- rbind(quantileProgress, cbind(q$secondQuant, q$thirdQuant))
      }
      mDev <- getMaxQuantDeviation(q)
      print(mDev)
      
      if(n > minRuns){
        currentLimit <- mDev
      }
      
    }
    n <- n + 1
  }
  
  list("theta" = theta, "progress" = progress, 
       "quantileProgress" = quantileProgress)
}