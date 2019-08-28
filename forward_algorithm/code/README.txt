

folder structure:

runTests.R:                 contains a variety of tests for the implementations (Bernoulli/Poisson, naiive/log implementations)
showLogSpaceSuperiority.R:  demostrates that log-implementation reaches higher accuracy
estimRainyProb.R:           demonstrates how to evaluate probability given model (rainyModel in this case)

./lib                      
    MC/                     Monte Carlo implementations
        Gibbs/
              applyOnRainModel.R    reference file for how to apply Gibb's Sampler to rainyModel
              Gibbs.R               main Gibbs functions
              runQuantitativeTest.R extensive benchmark used to assess algorithm
                
        MetropolisHastings/
              applyOnRainModel.R    reference file for how to apply Metropolis Hastings Sampler to rainyModel        
              MH.R                  main Metropolis Hastings functions
              runQuantitativeTest.R extensive benchmark used to assess algorithm
    
        common/
              Bernoulli.R           functions common to Bernoulli distributions
              common.R              functionality shared by both Gibb's and Metropolis Hastings


    models/                 model definitions and Bernoulli model generator
    

./results                   contains data from simulations, scripts for visualisation
./tests                     tests for uniformity of different step proposals for Metropolis Hastings (chapter 2.3)
./data                      Sample generator and rainySample dataset


Inspect "applyOnRainModel.R" for each sampler to get acquainted with how the samplers can be used. 
