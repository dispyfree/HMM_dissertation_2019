s <- floor(length(ret$progress$p1) / 3)
firstPart <- dat[2:s, ]
secondPart <- dat[s:(2*s),]
thirdPart <- dat[(2*s):length(ret$progress$p1), ]

minL <- 0.6
maxL <- 1

firstHist <- ggplot() + 
  geom_density(aes(firstPart$p1), n=128, adjust = 0.25, color = "darkblue") +
  xlab('p1') + ylab('density') + ggtitle('estimated density first part') +
  geom_vline(xintercept = quantile(firstPart$p1, 0.25), colour='red' )+
  geom_vline(xintercept = quantile(firstPart$p1, 0.5), colour='darkgreen') + 
  geom_vline(xintercept = quantile(firstPart$p1, 0.75), colour='red') + 
  scale_x_continuous(limits = c(minL, maxL)) 

secondHist <- ggplot() + 
  geom_density(aes(secondPart$p1), n=128, adjust = 0.25, color = "darkblue") +
  xlab('p1') + ylab('density') + ggtitle('estimated density second part') +
  geom_vline(xintercept = quantile(secondPart$p1, 0.25), colour='red' )+
  geom_vline(xintercept = quantile(secondPart$p1, 0.5), colour='darkgreen') + 
  geom_vline(xintercept = quantile(secondPart$p1, 0.75), colour='red') +
  scale_x_continuous(limits = c(minL, maxL)) 


thirdHist <- ggplot() + 
  geom_density(aes(thirdPart$p1), n=128, adjust = 0.25, color = "darkblue") + 
  xlab('p1') + ylab('density') + ggtitle('esetimated density thirdPart') +
geom_vline(xintercept = quantile(thirdPart$p1, 0.25), colour='red')+
  geom_vline(xintercept = quantile(secondPart$p1, 0.5), colour='darkgreen') + 
  geom_vline(xintercept = quantile(thirdPart$p1, 0.75), colour='red') +
  scale_x_continuous(limits = c(minL, maxL)) 

grid.arrange(firstHist, secondHist, thirdHist, nrow=3, ncol=1)



# plot quanties for different chain lengths
n <- length(ret$progress$p1)
chain <- ret$progress$p1

dat <- data.frame( ss <- c(),
                  secondLQ <- c(), secondMQ <- c(), secondUQ <- c(), 
                  thirdLQ <- c(), thirdMQ <- c(), thirdUQ <- c(), maxDiff = c())

for(sampleLength in 3:n){
  s <- floor(sampleLength / 3)
  
  secondPart <- ret$progress$p1[s: (2*s)]
  thirdPart  <- ret$progress$p1[(2 * s) : (3 * s)]
  
  secondLQ = quantile(secondPart, 0.25)
  secondMQ = quantile(secondPart, 0.5)
  secondUQ = quantile(secondPart, 0.75)
  thirdLQ  = quantile(thirdPart, 0.25)
  thirdMQ  = quantile(thirdPart, 0.5)
  thirdUQ  = quantile(thirdPart, 0.75)
  
  dat <- rbind(dat, data.frame(
    ss = sampleLength,
    secondLQ = secondLQ,
    secondMQ = secondMQ,
    secondUQ = secondUQ,
    thirdLQ  = thirdLQ,
    thirdMQ  = thirdMQ,
    thirdUQ  = thirdUQ,
    maxDiff = max(abs(c(secondLQ - thirdLQ, 
                        secondMQ - thirdMQ, 
                        secondUQ - thirdUQ)))
  ))
}

ggplot(dat, aes(x = ss)) + 
  geom_line(aes(y = secondLQ, color='darkred')) +
  geom_line(aes(y = secondMQ, color='red')) +
  geom_line(aes(y = secondUQ, color='darkred')) +
  geom_line(aes(y = thirdLQ, color='darkblue')) +
  geom_line(aes(y = thirdMQ, color='blue')) +
  geom_line(aes(y = thirdUQ, color='darkblue')) +
  geom_line(aes(y = maxDiff, color='pink')) +
  ggtitle('Course of chains\' parts quantiles') + 
  xlab('sample size') + 
  ylab('estimated quantiles') +
  scale_colour_manual("", 
                      labels = c( "MQ third", "LQ,UQ third", 
                                 "LQ,UQ second", "max. Diff", "MQ second"),
                      values = c("darkred" = "darkred", "red" = "red", 
                                 "darkblue" = "darkblue", "blue" = "blue", 
                                 "pink" = "pink")) +
   theme(legend.position="right") 
  