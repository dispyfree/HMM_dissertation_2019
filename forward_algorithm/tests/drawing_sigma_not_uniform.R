library(ggplot2)

dat <- data.frame(sigma = c(0))

sigma <- 0.5
steps <- 500
samples <- 100

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
 geom_histogram(bins=20) +
 ggtitle('sigma sampled with Carré\'s method')



