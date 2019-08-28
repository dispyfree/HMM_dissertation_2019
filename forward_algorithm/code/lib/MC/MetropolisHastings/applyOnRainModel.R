library('readr')
library(ggplot2)
source('lib/MC/MetropolisHastings/MH.R')
source('lib/MC/common/common.R')

setwd("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/forward_algorithm")

source('lib/models/rainModel.R')
rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv",
                        col_names = c('time', 'states', 'obs', 'prob'), skip = 1)
rainySample$time <- rainySample$time - 1

#parms <- getRequiredModel(3)
f <- list("getInitialTheta" = getInitialBernoulliTheta, 
          "buildDensity"    = buildBernDensity,
          "sampleTheta"     = mh.sampleBernoulliTheta,
          "progressCallback" = NA,
          "maxRuns" = 2000,
          "thinningFactor" = 1, 
          "noFixedParams" = 2, #parms$noFixedParams,
          "origTheta" = list(
            "delta" = u1,
            "gamma" = gamma1,
            "statePara" = lambda,
            "P_dens" = P_density
            )
          )


ret <- directMHSampler(2, rainySample[500,], f, 0.05)