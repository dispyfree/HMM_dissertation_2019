library(e1071)

# define rainy model as defined in 
# lecture "Hidden Markov Models", "Weather example" by Dr. Xavier Didelot
# state 1: high pressure, state 2: low pressure
# 1 : sun, 0 : rain

# use stationary distribution of transition matrix
u1 <- c(1.0, 0.5) / 1.5
gamma1 <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
P <- c(function(){
  # high pressure
  rdiscrete(1, c(0.9, 0.1),c(1, 0))
},
function(){
  # low pressure
  rdiscrete(1, c(0.3, 0.7), c(1, 0))
}
)

P_density <- c(function(x){
  # high pressure
  ddiscrete(x, c(0.9, 0.1), values = c(1, 0))
},
function(x){
  # low pressure
  ddiscrete(x, c(0.3, 0.7), values = c(1, 0))
}
)