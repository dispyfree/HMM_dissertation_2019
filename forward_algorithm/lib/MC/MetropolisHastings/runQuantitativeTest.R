setwd("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/forward_algorithm")


source('lib/models/bernoulliModelGenerator.R')
source('../common/genMC.R')
source('lib/MC/MetropolisHastings/MH.R')
library(tictoc)
library(testit)
# generates a number of Bernoulli models and measures performance

# no of states
noParamsToEstimate <- seq(2, 10, by = 1)
sampleSizes <- seq(500, 3000, by = 500)

# no of models to run (to reduce variance)
noModelsEachSize <- 5
runsPerModel <- 5


results <- data.frame(modelSize = c(0), 
                      ss = c(0),
                      run = c(0), 
                      modelRun = c(0),
                      noIterations = c(0),
                      timeSpent = c(0)
                      )


for(noParams in noParamsToEstimate){
  parms <- getRequiredModel(noParams)
  
  for(ss in sampleSizes){
    for(run in 1:noModelsEachSize){
      cat('noParams: ', noParams, ' sample size:', ss, ' run: ', run)
      
      model <- generateBernoulliModel(parms$m)
      f <- list("getInitialTheta" = getInitialBernoulliTheta, 
                "buildDensity"    = buildBernDensity,
                "sampleTheta"     = mh.sampleBernoulliTheta,
                "progressCallback" = NA,
                "maxRuns" = 3000,
                "thinningFactor" = 1, 
                "noFixedParams" = parms$noFixedParams,
                "origTheta" = list(
                  "delta" = model$delta,
                  "gamma" = model$gamma,
                  "statePara" = model$statePara,
                  "P_dens" = model$P_dens
                )
      )
      
      data <- genMCByTheta(model, ss)
      
      for(modelRun in 1:runsPerModel){
        tic()
        
        ret <- directMHSampler(parms$m, data, f, 0.05)
        
        timer <- toc(quiet=TRUE)
        
        results <- rbind(results, data.frame(
          modelSize = parms$m, ss = ss,  run = run, modelRun = modelRun,
          noIterations = ret$theta$noRuns,
          timeSpent = (timer$toc - timer$tic)
        ))
      }
    }
  }
}