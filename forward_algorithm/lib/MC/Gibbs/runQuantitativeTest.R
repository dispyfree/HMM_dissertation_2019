
source('lib/models/bernoulliModelGenerator.R')
source('../common/genMC.R')
# generates a number of Bernoulli models and measures performance

# no of states
modelSizes <- seq(2, 3, by = 1)
sampleSizes <- seq(100, 500, by = 100)

# no of models to run (to reduce variance)
noModelsEachSize <- 5

for(m in modelSizes){
  for(ss in sampleSizes){
    for(run in 1:noModelsEachSize){
      cat('model size: ', m, ' sample size:', ss, ' run: ', run)
      model <- generateBernoulliModel(m)
      
      data <- genMCByTheta(model, ss)
      
    }
  }
}