library(purrr)

sampleInitialDist <- function(delta){
  sd <- 0.05
  n <- length(delta)
  
  newDist <- delta + normaliseToZeroSum(rnorm(n, mean=0, sd=sd))
  
  # normalize so that all components are in [0, 1]
  newDist <- normaliseTo01Sum1(newDist)
  newDist
}


sampleGamma <- function(gamma){
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
  vec <- vec - sum(vec)
}



sampleBernoulliP <- function(probs){
  sd <- 0.05
  assert(all(probs >= 0 & probs <= 1))
  m <- length(probs)
  probs <- probs + rnorm(m, mean = 0, sd=sd)
  
  probs <- normaliseTo01(probs)
  assert(all(probs >= 0 & probs <= 1))
  probs
}



# vector of probabilities for Bernoulli dists
buildBernDensity <- function(ps){
  assert(all(ps >= 0 & ps <= 1))
  
    P_density <- map(ps, function(p){
      function(x){
      ddiscrete(x, c(p, 1 - p), values = c(1, 0))
    }})
  P_density
}