library(ggplot2)
library(gridExtra)

dat <- data.frame(x = c(0), y = c(0))

sigma <- 0.1
steps <- 200
samples <- 200

for(j in 1:samples){
  curXt <- runif(1, 0, 1)
  curYt <- runif(1, 0, 1)
  curX <- curXt / (curXt + curYt)
  curY <- curYt / (curXt + curYt)
  
  
  
  for(i in 1:steps){
    dat <- rbind(dat, data.frame(x = curX, y = curY))
    
    curX1 <- curX * exp(rnorm(1, mean =0, sd = sigma))
    curY1 <- curY * exp(rnorm(1, mean = 0, sd = sigma))
    
    #tentative solution
    curXt <- curX1 / (curX1 + curY1)
    curYt <- curY1 / (curX1 + curY1)
     if(runif(1) <= (curXt / curX) * (curYt / curY)
        * (exp(-curXt + curX - curYt + curY))){
       curX <- curXt
       curY <- curYt
     }
  }
}

ggplot(dat, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
  ggtitle('estimated density of Carré\'s  samples of p1, p2')


p1 <- ggplot(data=dat, aes(x)) +
  geom_histogram(breaks = seq(from = 0, to = 1, by = 0.05)) +
  ggtitle('p1 sampled with Carré\'s method')

p2 <- ggplot(data=dat, aes(y)) +
  geom_histogram(breaks = seq(from = 0, to = 1, by = 0.05)) +
  ggtitle('p2 sampled with Carré\'s method')

grid.arrange(p1, p2, nrow=2)




