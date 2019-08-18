library('readr')
source('lib/MC/Gibbs/Gibbs.R')
library(ggplot2)
library(grid)
library(ggExtra)
library(gridExtra)

source('../common/genMC.R')
source('lib/models/ourFirstPoissonEx.R')

# contains the parameterisation of the entire model (hence \theta)
# m is the number of states
getInitialTheta <- function(m){
  list("delta" = rep(1.0/m, m), 
       "gamma" = drawRandomGamma(m), # (1/m) in all components
       # parameterization of the states; for Bernoulli-model, this is just
       # p for each state. 
       "statePara" = c(1, 5, 10))
}


thetaToProgress <- function(progress, theta, d, p){
  progress <- rbind(progress, c(
    theta$delta[1:3],
    theta$gamma[1, 1:2], theta$gamma[2, 1:2],
    theta$statePara[1:3], 
    p
  ))
  progress
}


# delta, gamma, P_dens, obs 
GibbsSampler <- function(m, obs){
  n <- length(obs$obs)
  
  # initialize uniformly 
  theta <- getInitialTheta(m)
  
  runs <- 50
  progress <- data.frame(delta1 = c(0), delta2 = c(0), delta3 = c(0), 
                         gamma11 = c(0), gamma12 = c(0), 
                         gamma21 = c(0), gamma22 = c(0),
                         tau1 = c(0), tau2 = c(0), tau3 = c(0), p = c(0)
  )
  
  for(n in 1:runs){
    P_dens    <- buildPoissonDensityFromTau(theta$statePara)
    hiddenStates <- sampleHiddenStates(theta, P_dens, obs)
    theta <- samplePoissonTheta(theta, m, hiddenStates, obs, theta$delta, n, theta$statePara)
    
    p <- estimLogProb(theta$delta, theta$gamma, P_dens, obs)$logSum
    progress <- thetaToProgress(progress, theta, d, p)
    print(n)
    print(theta$statePara)
    print(p)
    print('-----------------------------------------')
  }
  
  list("theta" = theta, "progress" = progress)
}

#lambdaSample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/data/LambdaThousand.csv")
#lambdaSample$time <- lambdaSample$X1 - 1

T <- 1000
dat <- genMC(delta1, gamma1, P, P_density, T)

ret <- GibbsSampler(3, dat)

# true probability
#print(estimLogProb(ret$theta$delta, ret$theta$gamma, P_density, dat)$logSum)

# 
# indices <- 1:length(ret$progress$p1)
# plot(indices, ret$progress$p1, col='red', type='l')
# lines(indices, ret$progress$p2, col='blue', type='l')
# 
# dat <- data.frame(p1 = ret$progress$p1, p2 = ret$progress$p2, time = indices)
# 
# hist_right <- ggplot()+
#   geom_density(aes(c(dat$p1, dat$p2)), show.legend=NA, n=64, adjust = 0.25)+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank()) +
#   ggtitle('freq') +
#   coord_flip()
# 
# s = 150 / 3
# p <- ggplot(dat, aes(x=time)) + 
#   geom_line(aes(y = p1), color = "darkred") + 
#   geom_line(aes(y = p2), color="darkblue") +
#   scale_color_manual(values = c("darkred", "darkblue"))+
#   ggtitle('Course of Bernoulli paramters') +
#   ylab('p1, p2') +
#   geom_vline(xintercept = s, colour='blue', linetype="dotted")+
#   geom_vline(xintercept = 2 * s, colour='blue', linetype="dotted")+
#   annotate("text", x=15, y=0.05, label= "First Part", colour='blue') +
#   annotate("text", x= s + 25, y=0.05, label= "Second Part", colour='blue') +
#   annotate("text", x= 2 * s + 25, y=0.05, label= "Third Part", colour='blue') 
# p  
# #grid.arrange(p, hist_right, ncol=2, nrow=1, widths = c(5, 1))
# 
# 
# # show convergence of distance to real hidden states under L_1 norm
# ret$progress$time <- 1:length(ret$progress$p1)
# T <- length(rainySample$states)
# ggplot(data=ret$progress) +
#   geom_line(aes(x = time, y = dist), color = 'darkblue') +
#   geom_hline(yintercept = estimatedHiddenStateDist * T, color='red') + 
#   ylab('L1 distance') +
#   ggtitle('L1 distance to real hidden states')
# 
# 

