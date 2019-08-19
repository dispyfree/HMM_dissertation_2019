library(e1071)

# define rainy model
# use stationary distribution of transition matrix
# 1 : sun, 0 : rain

delta1 <- c(0.5, 0.2) / 3.0
gamma1 <- matrix(c(0.8, 0.2, 0.4, 0.6), 
                 nrow = 2, byrow = TRUE)
P <- c(function(){
    rpois(1, 4)
  },
  function(){
    rpois(1, 7)
  }
)

P_density <- c(function(x){
  dpois(x, 4)
},
function(x){
  dpois(x, 7)
}
)