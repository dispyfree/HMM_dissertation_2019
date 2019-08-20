library('readr')
library(ggplot2)
source('lib/MC/MetropolisHastings/MH.R')



source('lib/models/rainModel.R')
rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv",
                        col_names = c('time', 'states', 'obs', 'prob'), skip = 1)
rainySample$time <- rainySample$time - 1

f <- list("getInitialTheta" = getInitialBernoulliTheta, 
          "buildDensity"    = buildBernDensity,
          "sampleTheta"     = mh.sampleBernoulliTheta,
          "progressCallback" = NA
          )

runs <- 200
ret <- directMHSampler(2, rainySample, f, runs)


# ret$progress$time <- 1:length(ret$progress$p1)
# ggplot(ret$progress, aes(x=time)) + 
#   geom_line(aes(y = p1), color = "darkred") + 
#   geom_line(aes(y = p2), color="darkblue") +
#   scale_color_manual(values = c("darkred", "darkblue"))+
#   ggtitle('Course of Bernoulli paramters') +
#   ylab('p1, p2')


