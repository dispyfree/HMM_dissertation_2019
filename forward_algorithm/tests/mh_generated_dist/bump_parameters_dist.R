# simulation for ''bumping'' parameters
library(ggplot2)

dat <- data.frame(x = c())
sigma <- 0.1
steps <- 200
samples <- 200

# param x shall be in [a, b]
# if x is < a, then x is ''reflected'' on a
# if x > b, then x is ''reflected'' on b 
# if x is too far to be bumped, NA is returned
bumpParam <- function(x, a, b){
  dist <- (b - a)
  if(a <= x && x <= b){
    x
  }
  #if it can be reflected back at all
  else if(min(abs(x - a), abs(x - b)) < dist){
    if(x < a ){
      a + (a - x)
    }
    else{
      b - (x - b)
    }
  }
  else{
    NA
  }
}

for(sample in 1:samples){
  proposal <- runif(1, 0, 1)
  
  dat <- rbind(dat, data.frame(x = p))
  for(step in 1:steps){
    proposal <- proposal + rnorm(1, mean = 0, sd = sigma)
    proposal <- bumpParam(proposal, 0, 1)
    
    if(!(is.na(proposal))){
      dat <- rbind(dat, data.frame(x = proposal))
    }
  }
}

ggplot(data=dat, aes(x)) +
  geom_histogram(breaks = seq(from = 0, to = 1, by = 0.05)) +
  ggtitle('sigma sampled with bump method')

