
# define simple switching model
# 1: one state very likely, the other highly unlikely
#likely state (second) outputs 1 exclusively, unlikely state (first) outputs 0 
# exclusively

delta1 <- c(0.1, 0.9)
gamma1 <- as.matrix(c(0.1,0.9, 0.1, 0.9))
P1 <- c(
  function(x){ as.integer(x == 0)},
  function(x){ as.integer(x == 1)}
)

obs1 <- data.frame(obs = c(1, 1, 1, 1, 0, 1, 1, 0 , 1, 1),
                    time = 1:10) #likely
obs12 <- data.frame(obs = c(0, 0, 0, 1, 0, 1, 1, 0, 0, 1),
                   time = 1:10) #likely

estimProb(delta1, gamma1, P1, obs1)
estimProb(delta1, gamma1, P1, obs12)

