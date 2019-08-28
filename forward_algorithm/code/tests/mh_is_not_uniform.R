library(tidyverse)
library(viridis)
library(RColorBrewer)

# test samples 
means <- seq(from = 0.0, to = 0.1, by = 0.01)

stepsPerSample <- 40
samplesPerRun <- 50
sigma <- 0.02
overallMean <- 0.5
dat <- data.frame(x = c(0), y = c(0))

bump <- function(x){
  if(x < 0){
    -x
  }
  else if(x > 1){
    1 - (x - 1)
  }
  else{
    x
  }
}

logVal <- function(a, b){
  # discard value if it is highly unlikely
  if( (a > -1 && a < 0) || (a > 1 && a < 2) || (0 <= a && a <= 1)){
    dat <- rbind(dat, data.frame(x = c(bump(a)), y = c(b)))
  }
  dat
}

for(noMeanIndex in 1:length(means)){
  curMean <- means[noMeanIndex]
  # simulate samplesPerRun paths
  for(sample in 1:samplesPerRun){
    val <- rnorm(1, mean=curMean, sd = sigma)
    dat <- logVal(val, curMean)
    for(step in 1:stepsPerSample){
      val <- rnorm(1, mean = val, sd = sigma)
      dat <- logVal(val, curMean)
    }
  }
}

# extract quantiles
quant <- data.frame(q1 = c(0), q2 = c(0), qm = c(0), origM = c(0), y = c(0))
for(noMeanIndex in 1:length(means)){
  curMean <- means[noMeanIndex]
  data <- dat$x[dat$y == curMean]
  q1 <- quantile(data, 0.25)
  qm <- quantile(data, 0.5)
  q2 <- quantile(data, 0.75)
  quant <- rbind(quant, data.frame(y = curMean, q1 = q1, q2 = q2, qm = qm,
                                   origM = curMean))
}


ggplot(dat, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
  geom_point(data=quant, aes( x = q1, y = y), col='red') +
  geom_point(data=quant, aes( x = qm, y = y), col='green') +
  geom_point(data=quant, aes( x = q2, y = y), col='red') + 
  geom_point(data=quant, aes( x = origM, y = y), col='pink') 