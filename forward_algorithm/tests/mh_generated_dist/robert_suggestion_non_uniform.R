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
     if(runif(1) <= correctF(curXt) * (curXt / curX) * (curYt / curY)
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


# invert function
d <- c(dat$x, dat$y)
h <- hist(d, breaks = 20)

stepSize <- h$breaks[2] - h$breaks[1]
points <- seq(from = stepSize/2, length.out = 20, by = stepSize)
frequencies <- h$counts / sum(h$counts)

# nice article:
# https://datascienceplus.com/fitting-polynomial-regression-r/

# fit polynomial to invert
m <- lm(frequencies ~ points + I(points^2) + I(points^3))

f <- function(x){
  0.028 + 0.13 * x - 0.1301 * x^2
}

ggplot() + 
  geom_line(aes(x = points, y = f(points)), color= 'darkblue') +
  xlab('x/y') + 
  ylab('estimated frequency') +
  ggtitle('estimating frequencies from histogram')

plot(points, f(points))

maxP <- max(f(points))

correctF <- function(x){
  maxP / f(x)
}
