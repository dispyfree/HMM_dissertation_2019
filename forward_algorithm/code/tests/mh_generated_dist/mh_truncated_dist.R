# simulation for ''bumping'' parameters
library(ggplot2)

dat <- c()
sigma <- 0.1
steps <- 200
samples <- 500

drawUsingCDF <- function(n, mu, a ,b){
  qnorm(pnorm(a,mu,sigma) + runif(n)*(pnorm(b,mu,sigma) - pnorm(a,mu,sigma))) * sigma + mu
}


for(sample in 1:samples){
  if(sample %% 10 == 0 ){
    print(sample)
  }
  
  proposal <- runif(1, 0, 1)
  
  dat <- c(dat, proposal)
  for(step in 1:steps){
    proposal <- drawUsingCDF(1, proposal, 0, 1)
    dat <- c(dat, proposal)
  }
}

dat <- data.frame(x = dat)
ggplot(data=dat, aes(x)) +
  geom_histogram(breaks = seq(from = 0, to = 1, by = 0.05)) 
  ggtitle('p sampled with truncated normal distribution')

