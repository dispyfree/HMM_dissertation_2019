# simulation for ''bumping'' parameters
library(ggplot2)

dat <- data.frame(x = c())
sigma <- 0.1
steps <- 200
samples <- 200

drawUsingCDF <- function(n, mu, a ,b){
  qnorm(pnorm(a,mu,sigma) + runif(n)*(pnorm(b,mu,sigma) - pnorm(a,mu,sigma))) * sigma + mu
}


for(sample in 1:samples){
  proposal <- runif(1, 0, 1)
  
  dat <- rbind(dat, data.frame(x = p))
  for(step in 1:steps){
    proposal <- drawUsingCDF(1, proposal, 0, 1)
    dat <- rbind(dat, data.frame(x = proposal))
  }
}

ggplot(data=dat, aes(x)) +
  geom_histogram(breaks = seq(from = 0, to = 1, by = 0.05)) +
  ggtitle('p sampled with truncated normal distribution')

