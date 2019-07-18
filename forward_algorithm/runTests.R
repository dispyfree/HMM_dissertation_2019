library(e1071)
library(ggplot2)

source('lib/estimProb.R')

# threshold for numeric inaccuracies
threshold <- 0.0001

# simple rain model as defined in lecture nodes (slide set 2)
# chech the probability of (sun, rain, sun) is equal 
# define calculation manually and also run algorithm, compare results
source('lib/rainModel.R')
pSun <- matrix(c(0.9, 0.0, 0.0, 0.3), nrow = 2, byrow=TRUE)
pRain <- matrix(c(0.1, 0.0, 0.0, 0.7), nrow = 2, byrow=TRUE)
superSimpleProb <- t(u1) %*% gamma1 %*% pSun %*% gamma1  %*% pRain %*% gamma1  %*% pSun %*%  matrix(rep.int(1, 2), ncol= 1)

superSimpleData <- data.frame(time = c(1:3), obs = c(1, 0, 1))
computedProb <- estimProb(u1, gamma1, P_density, superSimpleData)
assert(abs(superSimpleProb - computedProb)  < threshold)


# test log implementation
logComputedProb <- estimLogProb(u1, gamma1, P_density, superSimpleData)
assert(abs(superSimpleProb - logComputedProb)  < threshold)

simplestProb<- t(u1) %*% pSun %*% gamma1 %*% pRain %*% gamma1  %*% pSun %*% matrix(rep.int(1, 2), ncol= 1)

simplestData <- data.frame(time = c(1, 2, 3), obs = c(1, 2, 1))
naiivelyEstimatedProb <- estimProb(u1, gamma1, P_density, simplestData)
logComputedProb <- estimLogProb(u1, gamma1, P_density, simplestData)
assert(abs(simplestProb - naiivelyEstimatedProb)  < threshold)
assert(abs(simplestProb - logComputedProb)  < threshold)


source('lib/simpleSwitchingModel.R')
obs1 <- data.frame(obs = c(1, 1, 1, 1, 0, 1, 1, 0 , 1, 1),
                   time = 1:10) #likely
obs12 <- data.frame(obs = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 1),
                    time = 1:10) #unlikely

p1 <- estimProb(delta1, gamma1, P1_density, obs1)
p2 <- estimProb(delta1, gamma1, P1_density, obs12)
assert(p1 > p2)









