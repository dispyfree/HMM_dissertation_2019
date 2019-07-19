library(e1071)

# define rainy model
# use stationary distribution of transition matrix
# 1 : sun, 0 : rain

delta1 <- c(2, 1) / 3.0
gamma1 <- matrix(c(1, 0, 0, 1), 
                 nrow = 2, byrow = TRUE)
P <- c(function(){
    rpoiss(r, 1)
  },
  function(){
    rpoiss(r, 2)
  }
)

P_density <- c(function(x){
  dpois(x, 1)
},
function(x){
  dpois(x, 2)
}
)