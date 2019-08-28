library('readr')
source('lib/MC/directGibbsSampler.R')
source('lib/models/rainModel.R')
source('lib/MC/common/utility.R')
# source('../common/genMC.R')
# 
#dat <- genMC(u = u1, gamma = gamma1, P = P,P_density = P_density, T= 500)
#write.csv(dat, file='../common/rainySample.csv')

csvFilePath = '../common/rainySample.csv'
rainySample <- read_csv("~/data/education/university/warwick/statistics/dissertation/programming/HMM_dissertation_2019/common/rainySample.csv")

rainySample$time <- rainySample$X1 - 1
res <- directGibbsSampler(u1, gamma1, P_density, rainySample)
maxStates <- extractMaxFromHistory(res$stateHistory, 2)
sum((rainySample$states - maxStates)^2)


