library(readr)
library(e1071)

# used to compare implementation of Hadi to mine in a "real-world" setting 
# (i.e. large dataset)
source('lib/rainModel.R')
rainySample <- read_csv("../common/rainySample.csv")
rainySample$time <-rainySample$X1

prob <- estimProb(u1, gamma1, P_density, rainySample)
print(prob)
