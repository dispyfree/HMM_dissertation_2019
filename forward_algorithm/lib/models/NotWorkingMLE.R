library(e1071)

# Hadi's MLE is not working and he is trying to break mine as well :(

par_vec<-c(4.441215,21.98937, 12.17503)
gamma1<-matrix(c(0.6361111, 0.1815369,0.1823520,
                    0.3326998,0.3073686,0.3599315,
                    0.4697118,0.1485795,0.3817086),nrow=3, ncol=3, byrow=TRUE)

u1 <- c(0.5310234, 0.1974305, 0.2715461)

P_dens <- c(function(x){
  dpois(x, par_vec[1])},
function(x){
  dpois(x, par_vec[2])},
function(x){
  dpois(x, par_vec[3])}
)