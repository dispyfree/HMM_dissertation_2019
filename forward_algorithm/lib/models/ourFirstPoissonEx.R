library(e1071)

# define rainy model
# use stationary distribution of transition matrix
# 1 : sun, 0 : rain

delta1 <- c(0.3, 0.7)
gamma1 <- matrix(c(0.3, 0.3, 0.4, 0.0, 0.9, 0.1, 0.2, 0.3, 0.5), 
                 nrow = 3, byrow = TRUE)
P <- c(function(){
  rpois(1, 10)
},
function(){
  rpois(1, 20)
},
function(){
  rpois(1, 5)
}
)

P_density <- c(function(x){
  dpois(x, 10)
},
function(x){
  dpois(x, 20)
},
function(x){
  dpois(x, 5)
}
)