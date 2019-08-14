library(tictoc)
source('lib/MC/common/utility.R')
source('lib/estimLogProb.R')

# samples gamma by altering with sample drawn from normal distribution
# first draws _one_ row to alter; then alters _solely_ this row
# always returns valid distributions
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



# delta, gamma, P_dens, obs 
directMHSampler <- function(m, obs, theta, progress, sampleTheta, buildDensity, thetaToProgress){
  n <- length(obs$obs)
  
  runs <- 400
  for(n in 1:runs){
    tic()
    newTheta <- sampleTheta(theta, obs)
    newP_dens <- buildDensity(newTheta$statePara)
    P_dens    <- buildDensity(theta$statePara)
    
    newProb <- estimTLogProb(newTheta, newP_dens, obs)
    oldProb <- estimTLogProb(theta, P_dens, obs)
    
    alpha <- min(0, newProb$logSum - oldProb$logSum)
    
    #accept
    if(log(runif(1)) <= alpha){
      print('accepted!');
      theta <- newTheta
      progress <- thetaToProgress(progress, newTheta, TRUE)
    }else{
      print('rejected!')
      progress <- thetaToProgress(progress, theta, FALSE)
    }
    print(newProb$logSum)
    print(theta$statePara)
    toc()
  }
  
  list("theta" = theta, "progress" = progress)
}