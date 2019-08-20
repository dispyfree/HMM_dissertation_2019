library(ramify)

source('lib/MC/common/Bernoulli.R')

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


# resulting vector has values in [0, 1], obtained by shifting accordingly
normaliseTo01 <- function(vec){
  ret <- sapply(vec, function(entry){
    if(entry < 0){
      -entry;
    }
    else if(entry > 1){
      1.0 - (entry - 1.0)
    }
    else{
      entry
    }});
  ret
}

# resulting vector is shifted s.t. its components sum to zero. 
normaliseToZeroSum <- function(vec){
  vec <- vec - (sum(vec) / length(vec))
  vec
}

# resulting vector has sum 1 and has values in [0, 1]
normaliseTo01Sum1 <- function(vec){
  l <- sum(abs(vec))
  # for numerical stability, make sure |*| < 1
  vec <- vec / (l * 1.001)
  normaliseTo01(vec)
}
