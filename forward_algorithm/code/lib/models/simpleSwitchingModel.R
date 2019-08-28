# define simple switching model
# 1: one state very likely, the other highly unlikely
#likely state (second) outputs 1 exclusively, unlikely state (first) outputs 0 
# exclusively

delta1 <- c(0.1, 0.9)
gamma1 <- matrix(c(0.1,0.9, 0.1, 0.9), nrow=2, ncol=2, byrow=TRUE)
P1_density <- c(
  function(x){ 
    if(x == 0){
      1.0
    }
    else{
      0
    }},
  function(x){ 
    if(x == 1){
      1.0
    }
    else{
      0
    }}
)