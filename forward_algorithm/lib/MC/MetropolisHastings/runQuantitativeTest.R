setwd("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/forward_algorithm")


source('lib/models/bernoulliModelGenerator.R')
source('../common/genMC.R')
source('lib/MC/MetropolisHastings/MH.R')
library(tictoc)
# generates a number of Bernoulli models and measures performance

# no of states
modelSizes <- seq(2, 2, by = 1)
sampleSizes <- seq(100, 1000, by = 100)

# no of models to run (to reduce variance)
noModelsEachSize <- 5
runsPerModel <- 5

f <- list("getInitialTheta" = getInitialBernoulliTheta, 
          "buildDensity"    = buildBernDensity,
          "sampleTheta"     = mh.sampleBernoulliTheta,
          "progressCallback" = NA,
          "maxRuns" = 2000
)

results <- data.frame(modelSize = c(0), 
                      ss = c(0),
                      run = c(0), 
                      modelRun = c(0),
                      noIterations = c(0),
                      timeSpent = c(0)
                      )


for(m in modelSizes){
  for(ss in sampleSizes){
    for(run in 1:noModelsEachSize){
      cat('model size: ', m, ' sample size:', ss, ' run: ', run)
      model <- generateBernoulliModel(m)
      data <- genMCByTheta(model, ss)
      
      for(modelRun in 1:runsPerModel){
        tic()
        ret <- directMHSampler(m, data, f, 0.1)
        
        timer <- toc(quiet=TRUE)
        
        results <- rbind(results, data.frame(
          modelSize = m, ss = ss,  run = run, modelRun = modelRun,
          noIterations = ret$theta$noRuns,
          timeSpent = (timer$toc - timer$tic)
        ))
      }
    }
  }
}