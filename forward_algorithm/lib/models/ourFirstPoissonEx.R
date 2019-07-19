library(e1071)

# define rainy model
# use stationary distribution of transition matrix
# 1 : sun, 0 : rain

delta1 <- c(0.3, 0.2, 0.5)
gamma1 <- matrix(c(0, 0, 1, 0.2, 0.4, 0.4, 0.2, 0.2, 0.6), 
                 nrow = 3, byrow = TRUE)
P <- c(function(){
  rpoiss(r, 10)
},
function(){
  rpoiss(r, 20)
},
function(){
  rpoiss(r, 1)
}
)

P_density <- c(function(x){
  dpois(x, 10)
},
function(x){
  dpois(x, 20)
},
function(x){
  dpois(x, 1)
}
)