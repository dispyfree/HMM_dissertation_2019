library(tictoc)

# Simulation of truncated normal variables
# Christian P. Roberts
drawTheRobertWay <- function(n, a ,b){
  z <- runif(n, a, b)
  
  if(a <= 0 && 0 <= b){
    delta_z <- exp(- z^2 / 2)
  }
  else if(b < 0){
    delta_z <- exp( - (b^2 - z^2) / 2)
  }
  else if(0 < a){
    delta_z <- exp(- (a^2 - z^2) / 2)
  }
  
  toAccept <- runif(n) <= delta_z
  z[toAccept]
}

drawUsingCDF <- function(n, a ,b){
  mu <- 0
  sigma <- 1
  qnorm(pnorm(a,mu,sigma) + runif(n)*(pnorm(b,mu,sigma) - pnorm(a,mu,sigma)))  
}

tic()
robertVals <- drawTheRobertWay(100000, 0, 1)
toc() 

tic()
cdfVals <- drawUsingCDF(50000, 0, 1)
toc()