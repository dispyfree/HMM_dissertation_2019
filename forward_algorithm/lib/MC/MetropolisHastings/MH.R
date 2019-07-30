
source('lib/MC/common/utiltiy.R')


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