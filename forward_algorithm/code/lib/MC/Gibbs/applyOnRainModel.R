
setwd("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/forward_algorithm")
library('readr')
source('lib/MC/Gibbs/Gibbs.R')
source('lib/MC/common/common.R')
source('lib/MC/common/Bernoulli.R')
#source('../common/genMC.R')
library(ggplot2)
library(grid)
library(ggExtra)
library(gridExtra)


 source('lib/models/rainModel.R')
 rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv")
 rainySample$time <- rainySample$X1 - 1

f01 <- function(x){
  y <- x
  y[x == 1] <- 2
  y[x == 2] <- 1
  y
}

 runs <- 30
ss <- 500 
 
 f <- list("getInitialTheta" = getInitialBernoulliTheta, 
           "buildDensity"    = buildBernDensity,
           "sampleTheta"     = gibbs.sampleBernoulliTheta,
           "noFixedParams" = 2,
           "maxRuns" = 2000,
           "origTheta" = list(
             "delta" = u1,
             "gamma" = gamma1,
             "statePara" = lambda,
             "P_dens" = P_density
           ),
           "progressCallback" = function(n, theta, progress, hiddenStates, extra){
             
             d1 <- sum(abs((tail(hiddenStates, -1)) - rainySample$states))
             d2 <- sum(abs(f01((tail(hiddenStates, -1))) - rainySample$states))
             d <- min(d1, d2)
             print(paste("distance: ", d))
             print(theta$statePara)
             print(n)
             print('--------------------------')
           })
 
 
# GibbsSampler(2, data, f, runs)
ret <- GibbsSampler(2, rainySample[500,], f, 0.01)