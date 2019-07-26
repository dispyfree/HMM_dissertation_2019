library('readr')
source('lib/MC/MetropolisHastings/MH.R')
source('lib/MC/directGibbsSampler.R')



# contains the parameterisation of the entire model (hence \theta)
# m is the number of states
getInitialTheta <- function(m){
  list("delta" = rep(1.0/m, m), 
       "gamma" = drawRandomGamma(m),
       # parameterization of the states; for Bernoulli-model, this is just
       # p for each state. 
       "statePara" = sampleBernoulliP(rep(1.0/m, m)))
}


sampleTheta <- function(theta, obs){
  print(sampleBernoulliP(theta$statePara))
  list("delta" = sampleInitialDist(theta$delta),
       "gamma" = sampleGamma(theta$gamma),
       "statePara" = sampleBernoulliP(theta$statePara)
       )
}

thetaToProgress <- function(progress, theta){
  progress <- rbind(progress, c(
    theta$delta[1:2],
    theta$gamma[1, 1:2], theta$gamma[2, 1:2],
    theta$statePara[1:2]
  ))
  progress
}

# delta, gamma, P_dens, obs 
directMHSampler <- function(m, obs){
  n <- length(obs$obs)
  
  # initialize uniformly 
  theta <- getInitialTheta(m)
  
  runs <- 500
  progress <- data.frame(delta1 = c(0), delta2 = c(0), 
                         gamma11 = c(0), gamma12 = c(0), 
                         gamma21 = c(0), gamma22 = c(0),
                         p1 = c(0), p2 = c(0)
                         )
  for(n in 1:runs){
    newTheta <- sampleTheta(theta, obs)
    newP_dens <- buildBernDensity(newTheta$statePara)
    P_dens    <- buildBernDensity(theta$statePara)
    
    newProb <- estimTProb(newTheta, newP_dens, obs)
    oldProb <- estimTProb(theta, P_dens, obs)
    
    alpha <- min(1, newProb / oldProb)
    
    #accept
    if(runif(1) <= alpha){
      print('accepted!');
      theta <- newTheta
      progress <- thetaToProgress(progress, newTheta)
    }else{
      print('rejected!')
    }
    
  }
  
  list("theta" = theta, "progress" = progress)
}

source('lib/models/rainModel.R')
rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv")
rainySample$time <- rainySample$X1 - 1

estim_theta <- directMHSampler(2, rainySample)



