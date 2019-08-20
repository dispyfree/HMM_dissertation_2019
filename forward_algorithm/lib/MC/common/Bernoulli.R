




# contains the parameterisation of the entire model (hence \theta)
# m is the number of states
getInitialBernoulliTheta <- function(m){
  list("delta" = rep(1.0/m, m),
       "gamma" = drawRandomGamma(m), # (1/m) in all components
       # parameterization of the states; for Bernoulli-model, this is just
       # p for each state. 
       "statePara" = runif(m))
}


# maps vector of probabilities to vector of Bernoulli pdfs
buildBernDensity <- function(ps){
  testit::assert(all(ps >= 0 & ps <= 1))
  
  P_density <- map(ps, function(p){
    function(x){
      ddiscrete(x, c(p, 1 - p), values = c(1, 0))
    }})
  P_density
}
