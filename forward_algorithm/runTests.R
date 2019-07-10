library(e1071)
library(ggplot2)

# define simple switching model
# 1: one state very likely, the other highly unlikely
#likely state (second) outputs 1 exclusively, unlikely state (first) outputs 0 
# exclusively

delta1 <- c(0.1, 0.9)
gamma1 <- matrix(c(0.1,0.9, 0.1, 0.9), nrow=2, ncol=2, byrow=TRUE)
P1 <- c(
  function(x){ as.integer(x == 0)},
  function(x){ as.integer(x == 1)}
)

obs1 <- data.frame(obs = c(1, 1, 1, 1, 0, 1, 1, 0 , 1, 1),
                    time = rep.int(1, 10)) #likely
obs12 <- data.frame(obs = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 1),
                   time = rep.int(1, 10)) #unlikely

p1 <- estimProb(delta1, gamma1, P1, obs1)
p2 <- estimProb(delta1, gamma1, P1, obs12)
assert(p1 > p2)




  






