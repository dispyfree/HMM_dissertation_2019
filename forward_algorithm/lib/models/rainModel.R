library(e1071)

# define rainy model
# use stationary distribution of transition matrix
# 1 : sun, 0 : rain

u1 <- c(1.0, 0.5) / 1.5
gamma1 <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
P <- c(function(){
  rdiscrete(1, c(0.9, 0.1),c(1, 0))
},
function(){
  rdiscrete(1, c(0.3, 0.7), c(1, 0))
}
)

P_density <- c(function(x){
  ddiscrete(x, c(0.9, 0.1), values = c(1, 0))
},
function(x){
  ddiscrete(x, c(0.3, 0.7), values = c(1, 0))
}
)