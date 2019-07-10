library(readr)
library(e1071)

rainySample <- read_csv("rainySample.csv")

# convert to difference
rainySample$time <- c(1, diff(rainySample$X1))

# define rainy model
# use stationary distribution of transition matrix
u1 <- c(1.0, 0.5) / 1.5
gamma1 <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
P <- c(function(){
  rdiscrete(1, c(0.9, 0.1))
},
function(){
  rdiscrete(1, c(0.3, 0.7))
}
)

P_density <- c(function(x){
  ddiscrete(x, c(0.9, 0.1))
},
function(x){
  ddiscrete(x, c(0.3, 0.7))
}
)

# pSun <- matrix(c(0.9, 0.0, 0.0, 0.3), nrow = 2, byrow=TRUE)
# pRain <- matrix(c(0.1, 0.0, 0.0, 0.7), nrow = 2, byrow=TRUE)
# superSimpleProb <- t(u1) %*% pSun %*% gamma1  %*% pRain %*% gamma1  %*% pSun %*%  matrix(rep.int(1, 2), ncol= 1)
# 
# superSimpleData <- data.frame(time = c(1:3), obs = c(1,2,1))
# prob <- estimProb(u1, gamma1, P_density, superSimpleData)
# print(prob)

prob <- estimProb(u1, gamma1, P_density, rainySample)
print(prob)
