library('readr')
library(ggplot2)
source('lib/MC/MetropolisHastings/MH.R')
source('lib/MC/directGibbsSampler.R')

normaliseToGreater0 <- function(val){
  if(val < 0)
    -val
  else
    val
}


# samples by altering probs with normal distribution
# always returns a valid distribution
samplePoissonLambda <- function(lambdas){
  sd <- 1
  assert(all(lambdas > 0))
  
  m <- length(lambdas)
  lambdas <- lambdas + rnorm(m, mean = 0, sd=sd)
  
  lambdas <- sapply(lambdas, normaliseToGreater0)
  lambdas
}



# contains the parameterisation of the entire model (hence \theta)
# m is the number of states
getInitialTheta <- function(m){
  list("delta" = rep(1.0/m, m), 
       "gamma" = drawRandomGamma(m),
       # parameterization of the states; for Bernoulli-model, this is just
       # p for each state. 
       "statePara" = samplePoissonLambda(rep(1.0/m, m)))
}


samplePoissonTheta <- function(theta, obs){
  list("delta" = sampleInitialDist(theta$delta),
       "gamma" = sampleGamma(theta$gamma),
       "statePara" = samplePoissonLambda(theta$statePar) 
  )
}


# initialize parameters 
theta <- getInitialTheta(3)
progress <- data.frame(delta1 = c(0), delta2 = c(0), 
                       gamma11 = c(0), gamma12 = c(0), 
                       gamma21 = c(0), gamma22 = c(0),
                       lambda1 = c(0), lambda2 = c(0), 
                       lambda3 = c(0),
                       accepted = c(FALSE)
)


source('lib/models/rainModel.R')
lambdaSample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/data/LambdaThousand.csv")
lambdaSample$time <- lambdaSample$X1 - 1


ret <- directMHSampler(2, lambdaSample, theta, progress, samplePoissonTheta, 
                       buildPoissonDensity, PoissonThetaToProgress)

ret$progress$time <- 1:length(ret$progress$p1)
ggplot(ret$progress, aes(x=time)) + 
  geom_line(aes(y = p1), color = "darkred") + 
  geom_line(aes(y = p2), color="darkblue") +
  scale_color_manual(values = c("darkred", "darkblue"))+
  ggtitle('Course of Bernoulli paramters') +
  ylab('p1, p2')


