library(e1071)

# Hadi's MLE is not working and he is trying to break mine as well :(
gamma1<-matrix(c(0.6122449, 0.2244898,0.1632653,
                    0.2592593,0.3333333,0.4074074,
                    0.5652174,0.3043478,0.1304348), 
                  nrow=3, byrow=TRUE)
par_vec<-c(4.5,20,10.43478)
u1 <- c(0.5058365, 0.2718319, 0.2223317)

P_dens <- c(function(x){
  dpois(x, par_vec[1])},
function(x){
  dpois(x, par_vec[2])},
function(x){
  dpois(x, par_vec[3])}
)