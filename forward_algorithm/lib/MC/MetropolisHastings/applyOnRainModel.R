library('readr')
library(ggplot2)
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


sampleBernTheta <- function(theta, obs){
  print(theta$statePara)
  sampledBernoulli <-  sampleBernoulliP(theta$statePara)estimLogProb
  list("delta" = sampleInitialDist(theta$delta),
       "gamma" = sampleGamma(theta$gamma),
       "statePara" = sampledBernoulli 
       )
}


# initialize parameters 
theta <- getInitialTheta(2)
progress <- data.frame(delta1 = c(0), delta2 = c(0), 
                       gamma11 = c(0), gamma12 = c(0), 
                       gamma21 = c(0), gamma22 = c(0),
                       p1 = c(0), p2 = c(0), accepted = c(FALSE)
)


source('lib/models/rainModel.R')
rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv",
                        col_names = c('time', 'states', 'obs', 'prob'), skip = 1)
rainySample$time <- rainySample$time - 1


ret <- directMHSampler(2, rainySample, theta, progress, sampleBernTheta, 
                       buildBernDensity, BernThetaToProgress)
ret$progress$time <- 1:length(ret$progress$p1)
ggplot(ret$progress, aes(x=time)) + 
  geom_line(aes(y = p1), color = "darkred") + 
  geom_line(aes(y = p2), color="darkblue") +
  scale_color_manual(values = c("darkred", "darkblue"))+
  ggtitle('Course of Bernoulli paramters') +
  ylab('p1, p2')


