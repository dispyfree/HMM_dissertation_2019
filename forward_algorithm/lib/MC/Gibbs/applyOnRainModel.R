library('readr')
source('lib/MC/Gibbs/Gibbs.R')


# contains the parameterisation of the entire model (hence \theta)
# m is the number of states
getInitialTheta <- function(m){
  list("delta" = rep(1.0/m, m), 
       "gamma" = drawRandomGamma(m), # (1/m) in all components
       # parameterization of the states; for Bernoulli-model, this is just
       # p for each state. 
       "statePara" = c(0.55, 0.45))
}


thetaToProgress <- function(progress, theta, d){
  progress <- rbind(progress, c(
    theta$delta[1:2],
    theta$gamma[1, 1:2], theta$gamma[2, 1:2],
    theta$statePara[1:2],
    d
  ))
  progress
}


# delta, gamma, P_dens, obs 
GibbsSampler <- function(m, obs){
  n <- length(obs$obs)
  
  # initialize uniformly 
  theta <- getInitialTheta(m)
  
  runs <- 50
  progress <- data.frame(delta1 = c(0), delta2 = c(0), 
                         gamma11 = c(0), gamma12 = c(0), 
                         gamma21 = c(0), gamma22 = c(0),
                         p1 = c(0), p2 = c(0),
                         dist = c(0)
  )
  
  for(n in 1:runs){
    P_dens    <- buildBernDensity(theta$statePara)
    hiddenStates <- sampleHiddenStates(theta, P_dens, obs)
    theta <- sampleTheta(m, hiddenStates, obs, theta$delta, n)
    
    s1 <- abs(tail(hiddenStates, -1) - rainySample$states)
    s2 <- abs(tail(replace(hiddenStates, c(1, 2), c(2, 1)), -1) - rainySample$states)
    d <- min(sum(s1), sum(s2))
    progress <- thetaToProgress(progress, theta, d)
    print(n)
    
    print(d)
    print('-----------------------------------------')
  }
  
  list("theta" = theta, "progress" = progress)
}

 source('lib/models/rainModel.R')
 rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv")
 rainySample$time <- rainySample$X1 - 1

# sample <- data.frame(time = c(0, 1, 2, 3, 4, 5, 6, 7), 
#                      obs = c(1, 1, 1, 1, 0, 0, 0, 0), 
#                      states = c(1, 1, 1, 1, 2, 2, 2, 2))
# 
# ret <- GibbsSampler(2, sample)
ret <- GibbsSampler(2, rainySample)

indices <- 1:length(ret$progress$p1)
plot(indices, ret$progress$p1, col='red', type='l')
lines(indices, ret$progress$p2, col='blue', type='l')




