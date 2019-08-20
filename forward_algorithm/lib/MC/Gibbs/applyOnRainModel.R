library('readr')
source('lib/MC/Gibbs/Gibbs.R')
source('lib/MC/Gibbs/common.R')
source('../common/genMC.R')
library(ggplot2)
library(grid)
library(ggExtra)
library(gridExtra)


 source('lib/models/rainModel.R')
 rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv")
 rainySample$time <- rainySample$X1 - 1

f01 <- function(x){
  y <- x
  y[x == 1] <- 2
  y[x == 2] <- 1
  y
}

 runs <- 30
ss <- 500 
 model <- generateBernoulliModel(2)
 data <- genMCByTheta(model, ss)
 
 f <- list("getInitialTheta" = getInitialBernoulliTheta, 
           "buildDensity"    = buildBernDensity,
           "sampleTheta"     = sampleBernoulliTheta,
           "progressCallback" = function(n, theta, progress, hiddenStates, extra){
             
             d1 <- sum(abs((tail(hiddenStates, -1)) - rainySample$states))
             d2 <- sum(abs(f01((tail(hiddenStates, -1))) - rainySample$states))
             d <- min(d1, d2)
             print(paste("distance: ", d))
             print(theta$statePara)
             print(n)
             print('--------------------------')
           })
 
 
# GibbsSampler(2, data, f, runs)
ret <- GibbsSampler(2, rainySample, f, runs)

# indices <- 1:length(ret$progress$p1)
# plot(indices, ret$progress$p1, col='red', type='l')
# lines(indices, ret$progress$p2, col='blue', type='l')
# 
# dat <- data.frame(p1 = ret$progress$p1, p2 = ret$progress$p2, time = indices)
# 
# hist_right <- ggplot()+
#   geom_density(aes(c(dat$p1, dat$p2)), show.legend=NA, n=64, adjust = 0.25)+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank()) +
#   ggtitle('freq') +
#   coord_flip()
# 
# s = 150 / 3
# p <- ggplot(dat, aes(x=time)) + 
#   geom_line(aes(y = p1), color = "darkred") + 
#   geom_line(aes(y = p2), color="darkblue") +
#   scale_color_manual(values = c("darkred", "darkblue"))+
#   ggtitle('Course of Bernoulli paramters') +
#   ylab('p1, p2') +
#   geom_vline(xintercept = s, colour='blue', linetype="dotted")+
#   geom_vline(xintercept = 2 * s, colour='blue', linetype="dotted")+
#   annotate("text", x=15, y=0.05, label= "First Part", colour='blue') +
#   annotate("text", x= s + 25, y=0.05, label= "Second Part", colour='blue') +
#   annotate("text", x= 2 * s + 25, y=0.05, label= "Third Part", colour='blue') 
# p  
# #grid.arrange(p, hist_right, ncol=2, nrow=1, widths = c(5, 1))
# 
# 
# # show convergence of distance to real hidden states under L_1 norm
# ret$progress$time <- 1:length(ret$progress$p1)
# T <- length(rainySample$states)
# ggplot(data=ret$progress) +
#   geom_line(aes(x = time, y = dist), color = 'darkblue') +
#   geom_hline(yintercept = estimatedHiddenStateDist * T, color='red') + 
#   ylab('L1 distance') +
#   ggtitle('L1 distance to real hidden states')
#     
# 

