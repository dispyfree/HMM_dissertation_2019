library(purrr)
library(testit)
library(lambda.tools)

# samples by altering delta with normal distribution
# always returns a valid distribution
sampleInitialDist <- function(delta){
  sd <- 0.05
  n <- length(delta)
  
  newDist <- delta + normaliseToZeroSum(rnorm(n, mean=0, sd=sd))
  
  # normalize so that all components are in [0, 1]
  newDist <- normaliseTo01Sum1(newDist)
  newDist
}



# tries to bump individual entries; if not possible, keep old entry
bumpTo01OrKeep <- function(newVec, oldVec){
  newVec <- bumpTo01(newVec)
  newVec[is.na(newVec)] <- oldVec[is.na(newVec)]
  newVec
}

# resulting vector has values in [0, 1], obtained by shifting accordingly
bumpTo01 <- function(vec){
  ret <- sapply(vec, function(entry){
    # if it can't be bumped back, return NA
    if(min(abs(entry - 0), abs(entry - 1)) > 1){
      NA
    }
    else{
      if(entry < 0){
        -entry;
      }
      else if(entry > 1){
        1.0 - (entry - 1.0)
      }
      else{
        entry
      }}});
}


# resulting vector has sum 1 and has values in [0, 1]
normaliseToSum1 <- function(vec){
  l <- sum(abs(vec))
  vec <- vec / l
  vec
}

# resulting vector is shifted s.t. its components sum to zero. 
normaliseToZeroSum <- function(vec){
  vec <- vec - (sum(vec) / length(vec))
  vec
}

# resulting vector has sum 1 and has values in [0, 1]
normaliseTo01Sum1 <- function(vec){
  l <- sum(abs(vec))
  vec <- vec / l
  normaliseTo01(vec)
}
# resulting vector has values in [0, 1], obtained by shifting accordingly
normaliseTo01 <- function(vec){
  if(min(vec) < 0){
    vec <- vec - min(vec)
  }
  else if(max(vec) > 1){
    vec <- vec - (max(vec) - 1)
  }
  vec
}



# all entries are (1/m)
drawRandomGamma <- function(m){
  gamma <- matrix(rep.int(1/m, m*m), ncol=m)
  gamma
}

# samples by altering probs with normal distribution
# always returns a valid distribution
sampleBernoulliP <- function(probs){
  sd <- 0.03
  assert(all(probs >= 0 & probs <= 1))
  
  m <- length(probs)
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  probs <- sapply(probs, bumpTo01OrKeep)
  assert(all(probs >= 0 & probs <= 1))
  probs
}



# maps vector of regime parameters to vector of Poisson pdfs
buildPoissonDensityFromTau <- function(taus){
  assert(all(taus > 0))
  
  P_density <- map(cumsum(taus), function(lambda){
    function(x){
      dpois(x, lambda)
    }})
  P_density
}


# maps vector of probabilities to vector of Bernoulli pdfs
buildPoissonDensity <- function(lambdas){
  assert(all(lambdas > 0))
  
  P_density <- map(lambdas, function(lambda){
    function(x){
      dpois(x, lambda)
    }})
  P_density
}

# attaches theta to progress and returns new progress
BernThetaToProgress <- function(progress, theta, accepted){
  progress <- rbind(progress, c(
    theta$delta[1:2],
    theta$gamma[1, 1:2], theta$gamma[2, 1:2],
    theta$statePara[1:2],
    accepted
  ))
  progress
}


# attaches theta to progress and returns new progress
PoissonThetaToProgress <- function(progress, theta, accepted){
  progress <- rbind(progress, c(
    theta$delta[1:2],
    theta$gamma[1, 1:2], theta$gamma[2, 1:2],
    theta$statePara[1:3],
    accepted
  ))
  progress
}