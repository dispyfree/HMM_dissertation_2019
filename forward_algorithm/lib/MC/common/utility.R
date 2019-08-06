library(purrr)
library(testit)

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

# resulting vector is shifted s.t. its components sum to zero. 
normaliseToZeroSum <- function(vec){
  vec <- vec - (sum(vec) / length(vec))
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
  sd <- 0.05
  assert(all(probs >= 0 & probs <= 1))
  m <- length(probs)
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  probs <- normaliseTo01(probs)
  assert(all(probs >= 0 & probs <= 1))
  probs
}



# maps vector of probabilities to vector of Bernoulli pdfs
buildBernDensity <- function(ps){
  assert(all(ps >= 0 & ps <= 1))
  
  P_density <- map(ps, function(p){
    function(x){
      ddiscrete(x, c(p, 1 - p), values = c(1, 0))
    }})
  P_density
}

# attaches theta to progress and returns new progress
thetaToProgress <- function(progress, theta){
  progress <- rbind(progress, c(
    theta$delta[1:2],
    theta$gamma[1, 1:2], theta$gamma[2, 1:2],
    theta$statePara[1:2]
  ))
  progress
}