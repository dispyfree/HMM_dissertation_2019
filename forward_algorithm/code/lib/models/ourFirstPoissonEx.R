library(e1071)

# define rainy model
# use stationary distribution of transition matrix
# 1 : sun, 0 : rain

delta1 <- c(0.9, 0.1, 0.0)
gamma1 <- matrix(c(0.4, 0.2, 0.4,  0.2, 0.7, 0.1,  0.1, 0.3, 0.6), 
                 nrow = 3, byrow = TRUE)
P <- c(function(){
  rpois(1, 5)
},
function(){
  rpois(1, 10)
}
,
function(){
  rpois(1, 15)
}
)

P_density <- c(function(x){
  dpois(x, 5)
},
function(x){
  dpois(x, 10)
},
function(x){
  dpois(x, 12)
}
)