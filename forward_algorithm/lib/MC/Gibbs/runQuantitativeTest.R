setwd("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/forward_algorithm")


source('lib/models/bernoulliModelGenerator.R')
source('../common/genMC.R')
source('lib/MC/Gibbs/Gibbs.R')
source('lib/MC/common/common.R')
source('lib/MC/common/Bernoulli.R')
library(tictoc)
library(testit)
# generates a number of Bernoulli models and measures performance

# no of states
noParamsToEstimate <- seq(7, 10, by = 1)
sampleSizes <- seq(1000, 3000, by = 1000)

# no of models to run (to reduce variance)
noModelsEachSize <- 3
runsPerModel <- 3


results <- data.frame(modelSize = c(0), 
                      ss = c(0),
                      run = c(0), 
                      modelRun = c(0),
                      noIterations = c(0),
                      timeSpent = c(0),
                      meanDist = c(0)
                      )


for(noParams in noParamsToEstimate){
  parms <- getRequiredModel(noParams)
  
  for(ss in sampleSizes){
    for(run in 1:noModelsEachSize){
      print(paste0('noParams: ', noParams, ' sample size:', ss, ' run: ', run))
      
      model <- generateBernoulliModel(parms$m)
      f <- list("getInitialTheta" = getInitialBernoulliTheta, 
                "buildDensity"    = buildBernDensity,
                "sampleTheta"     = gibbs.sampleBernoulliTheta,
                "noFixedParams" = parms$noFixedParams,
                "maxRuns" = 3000,
                "progressCallback" = NA,
                "origTheta" = list(
                  "delta" = model$delta,
                  "gamma" = model$gamma,
                  "statePara" = model$statePara,
                  "P_dens" = model$P_dens
                ))
      
      data <- genMCByTheta(model, ss)
      
      for(modelRun in 1:runsPerModel){
        tic()
        
        ret <- GibbsSampler(parms$m, data, f, 0.01)
        
        timer <- toc(quiet=TRUE)
        
        # track quality of solution
        estMeans <- getEstimatedMeans(parms$m, ret$progress)
        origMeans <- model$statePara
        
        results <- rbind(results, data.frame(
          modelSize = noParams, ss = ss,  run = run, modelRun = modelRun,
          noIterations = ret$theta$noRuns,
          timeSpent = (timer$toc - timer$tic),
          meanDist = shortestDistanceUnderPerm(estMeans, origMeans)
        ))
      }
    }
  }
}