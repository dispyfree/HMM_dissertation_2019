library(lambda.tools)

# generates a number of weights to be used with rdirichlet
# pom point of mass, index deciding where most of the probability mass lies
# parameters are tweaked to produced reasonable values (subjective)
getStandardDist <- function(m, pom){
  weights      <- rep.int(0.5, m)
  weights[pom] <- 1
  
  weights
}


#generates an entire model consisting of 
# delta, gamma, R (sampling functions), P_dens (densities) and 
# statePara (bernoulli parameters)
# m: no of states
generateBernoulliModel <- function(m){
  
  # weight is - w.l.o.g. - always on the first component
  delta <- rdirichlet(1, getStandardDist(m, 1))
  gamma <- sampleGammaDist(m)
  # list containing functions to sample from each distribution
  ps <- runif(m)
  R <- lapply(1:m, function(i){
    function(){
      rdiscrete(1, c(ps[i], 1-ps[i]), c(1, 0))
    }
  })
  
  P_dens <- lapply(1:m, function(i){
    function(x){
      ddiscrete(x, c(ps[i], 1-ps[i]), c(1, 0))
    }
  })
  
  list(delta = delta, gamma  = gamma, R = R, P_dens = P_dens, statePara = ps)
}


sampleGammaDist <- function(m){
  gamma <- matrix(rep.int(0, m * m), nrow = m)
  
  for(i in 1:m)
    gamma[i, ] <- rdirichlet(1, getStandardDist(m, i))
  gamma 
}