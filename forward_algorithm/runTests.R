library(e1071)
library(ggplot2)

source('lib/estimProb.R')

# threshold for numeric inaccuracies
threshold <- 0.0001

# simple rain model as defined in lecture nodes (slide set 2)
# chech the probability of (sun, rain, sun) is equal 
# define calculation manually and also run algorithm, compare results
source('lib/models/rainModel.R')
pSun <- matrix(c(0.9, 0.0, 0.0, 0.3), nrow = 2, byrow=TRUE)
pRain <- matrix(c(0.1, 0.0, 0.0, 0.7), nrow = 2, byrow=TRUE)
superSimpleProb <- t(u1) %*% gamma1 %*% pSun %*% gamma1  %*% pRain %*% gamma1  %*% pSun %*%  matrix(rep.int(1, 2), ncol= 1)

superSimpleData <- data.frame(time = c(1:3), obs = c(1, 0, 1))
computedProb <- estimProb(u1, gamma1, P_density, superSimpleData)
assert(abs(superSimpleProb - computedProb)  < threshold)


# test log implementation
ret <- estimLogProb(u1, gamma1, P_density, superSimpleData)
logComputedProb <- ret$logSum
assert(abs(superSimpleProb - exp(logComputedProb))  < threshold)

simplestProb<- t(u1) %*% pSun %*% gamma1 %*% pRain %*% gamma1  %*% pSun %*% matrix(rep.int(1, 2), ncol= 1)

simplestData <- data.frame(time = c(1, 2, 3), obs = c(1,0,1))
naiivelyEstimatedProb <- estimProb(u1, gamma1, P_density, simplestData)
ret <- estimLogProb(u1, gamma1, P_density, simplestData)
logComputedProb <- ret$logSum
assert(abs(simplestProb - naiivelyEstimatedProb)  < threshold)
assert(abs(simplestProb - exp(logComputedProb))  < threshold)


source('lib/models/simpleSwitchingModel.R')
obs1 <- data.frame(obs = c(1, 1, 1, 1, 0, 1, 1, 0 , 1, 1),
                   time = 1:10) #likely
obs12 <- data.frame(obs = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 1),
                    time = 1:10) #unlikely

p1 <- estimProb(delta1, gamma1, P1_density, obs1)
p2 <- estimProb(delta1, gamma1, P1_density, obs12)
assert(p1 > p2)



 source('lib/models/ourSecondPoissonEx.R')
 obs <- data.frame(obs = c(1, 2), time = c(0, 1))
 prob<- estimProb(u1, gamma1, P_density, obs)
 ret <- estimLogProb(u1, gamma1, P_density, obs)
 logProb <- ret$logSum 
 
 pOne <- diag(c(dpois(1, 1), dpois(1, 2)))
 pTwo <- diag(c(dpois(2, 1), dpois(2, 2)))
 
 realProb <- t(u1) %*% pOne %*% gamma1 %*% pTwo %*% matrix(rep.int(1, 2), ncol= 1)


 assert(abs(prob - exp(logProb))  < threshold)
 assert(abs(realProb - exp(logProb))  < threshold)





