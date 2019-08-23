library(ramify)
library(testit)

source('lib/MC/common/Bernoulli.R')
source('lib/MC/common/utility.R')

# attaches current theta values to progress and returns progress
thetaToProgress <- function(progress, theta, d){
  
  progress <- rbind(progress, c(
    d,
    theta$delta,
    ramify::flatten(theta$gamma, across='rows'),
    theta$statePara
  ))
  progress
}


# maps teach progress parameter (ie. everything except the first)
# to its (0.25, 0.5, 0.75) quantiles.  
# cuts the progress in three parts of equal size (1-n, n - 2n, 2n - 3n)
# and computes the quantiles for each part, respectively 
progressToQuantiles <- function(progress){
  n <- dim(progress)[1]
  chainSize <- floor(n / 3)
  paramNo <- dim(progress)[2] - 1
  quantilesToMeasure <- c(0.25, 0.5, 0.75)
  
  #stores quantiles for second/third part for each component, respectively
  sq <- c()
  tq <- c()
  
  for(compIndex in 2:paramNo){
    #firs part discarded
    #firstPart <- progress[1: chainSize, compIndex]
    secondPart <- progress[chainSize: (2 * chainSize), compIndex]
    thirdPart <- progress[(2 * chainSize): (3 * chainSize), compIndex]
    
    sq <- c(sq, quantile(secondPart, quantilesToMeasure, names = FALSE))
    tq <- c(tq, quantile(thirdPart, quantilesToMeasure, names = FALSE))
  }
  
  list("secondQuant" = sq, "thirdQuant" = tq)
}


# for use with @progressToQuantiles
# extracts the maximum difference between respective components
getMaxQuantDeviation <- function(p){
  max(abs(p$secondQuant - p$thirdQuant))
}



# create a progress object for use with
# @thetaToProgress
# assumes that for each state , there is an entry in delta, m entries
# in gamma plus one single parameter parameterising the state itself.
# "d" is meant to store arbitrary data, for instance for tracking the 
# the progress of convergence. 
createProgress <- function(m){
  noColumns <- 1 + 2 * m + m*m
  dat <- data.frame(matrix(rep.int(0, noColumns), nrow=1))
  cnames <- c("d")
  
  for(i in 1:m){
    di <- paste("delta", i, sep ="")
    cnames <- c(cnames, di)
  }
  
  # double loop for gamma
  for(i in 1:m){
    for(j in 1:m){
      gi <- paste("gamma", i, j, sep="")
      cnames <- c(cnames, gi)
    }
  }
  
  # state params
  for(i in 1:m){
    di <- paste("state", i, sep ="")
    cnames <- c(cnames, di)
  }
  
  colnames(dat) <- cnames
  dat
}





# estimates Gamma using the given estimate of hidden states
# then updates it by means of weighted Dirichlet distribution
gibbs.sampleGamma <- function(m, hiddenStates, prior){
  eGamma <- estimGamma(m, hiddenStates)
  sample <- replicate(m, prior)
  
  # sample new transition probabilities
  gamma <- matrix(rep.int(0, m*m), nrow = m)
  for(i  in 1:m){
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


drawRandomGamma <- function(m){
  gamma <- matrix(rep.int(1/m, m*m), ncol=m)
  gamma
}


# gets model needed for the given numbers of parameters to be estimated

getRequiredModel <- function(paramsToEstimate){
  testit::assert(paramsToEstimate < 12)
  testit::assert(paramsToEstimate >=2)
  
  if(paramsToEstimate <= 6){
    list("m" = 2, "paramsToEstimate" = paramsToEstimate,
         "noFixedParams" = 6 - paramsToEstimate)
  }else{
    list("m" = 3, "paramsToEstimate" = paramsToEstimate,
         "noFixedParams" = 12 - paramsToEstimate)
  }
}


