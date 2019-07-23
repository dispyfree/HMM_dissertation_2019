obs <- data.frame(obs = c(1, 10, 3, 0, 100), time = c(1, 2, 3, 4, 5))

P_density <- c(function(x){
  dpois(x, 3)
},
function(x){
  dpois(x, 4)
},
function(x){
  dpois(x, 7)
},
function(x){
  dpois(x, 1)
},
function(x){
  dpois(x, 100)
}
)

u_1<-c(0.2, 0.3, 0.01, 0.4, 0.09)
gamma_mat<-matrix(c(0.5, 0.4, 0.1, 0, 0,
                    0, 0.8, 0.2, 0, 0, 
                    0.1, 0.2, 0.5, 0.1, 0.1,
                    0.2, 0.2, 0.2, 0.2, 0.2,
                    0, 0, 0, 0.5, 0.5), nrow = 5, ncol = 5, byrow=TRUE)

