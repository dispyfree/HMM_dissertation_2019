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