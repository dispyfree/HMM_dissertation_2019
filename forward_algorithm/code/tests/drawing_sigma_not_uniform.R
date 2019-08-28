library(ggplot2)

dat <- data.frame(sigma = c(0))

sigma <- 0.1
steps <- 200
samples <- 200

for(j in 1:samples){
  curSigma <- runif(1, 0, 10)
  
  newSigma <- curSigma * exp(rnorm(1, 0, sigma))
  for(i in 1:steps){
    dat <- rbind(dat, data.frame(sigma = curSigma))
    
    if(runif(1) <= newSigma / curSigma){
      curSigma <- newSigma
    }
  }
}

ggplot(data=dat, aes(sigma)) +
 geom_histogram(breaks = seq(from = 0, to = 1, by = 0.05)) +
 ggtitle('sigma sampled with Carré\'s method')



