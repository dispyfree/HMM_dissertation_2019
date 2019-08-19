library(ramify)


# maps vector of probabilities to vector of Bernoulli pdfs
buildBernDensity <- function(ps){
  assert(all(ps >= 0 & ps <= 1))
  
  P_density <- map(ps, function(p){
    function(x){
      ddiscrete(x, c(p, 1 - p), values = c(1, 0))
    }})
  P_density
}


# contains the parameterisation of the entire model (hence \theta)
# m is the number of states
getInitialBernoulliTheta <- function(m){
  list("delta" = c(0.3, 0.9),#rep(1.0/m, m), 
       "gamma" = drawRandomGamma(m), # (1/m) in all components
       # parameterization of the states; for Bernoulli-model, this is just
       # p for each state. 
       "statePara" = runif(m))
}


# attaches current theta values to progress and returns progress
thetaToProgress <- function(progress, theta, d){
  progress <- rbind(progress, c(
    d,
    theta$delta,
    flatten(theta$gamma, across='rows'),
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