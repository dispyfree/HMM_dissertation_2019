library(ggplot2)

source('lib/simpleSwitchingModel.R')
source('lib/estimProb.R')
source('lib/estimLogProb.R')
#trialsNos <- seq(15, 25, 1)
trialsNos <- seq(15, 25, 1)
base <- 1.5

probs1 <- c()
probs2 <- c()

for(b in trialsNos){
  n <- base^b
  obs1 <- data.frame(obs = rep(1, n),
                   time = 1:n) 
  
  p1 <- estimProb(delta1, gamma1, P1_density, obs1)
  p2 <- estimLogProb(delta1, gamma1, P1_density, obs1)
  print(p1)
  print(p2)
  probs1 <- c(probs1, log(p1))
  probs2 <- c(probs2, p2)
}

results <- data.frame(naiiveProbs <- probs1, logProbs <- probs2,
                      noObs <- base^(trialsNos))

ggplot() +
  geom_line(data=results, aes(x=noObs, y=naiiveProbs, color='red')) +
  geom_point(data=results, aes(x=noObs, y=naiiveProbs, color='red')) +
  geom_line(data=results, aes(x=noObs, y=logProbs, color='blue' ))+
  geom_point(data=results, aes(x=noObs, y=logProbs, color='blue' )) +
  labs(x = '# of observations', y = 'log-probability')+
  ggtitle('log-probability naiive algorithm switcher model') + 
  scale_color_discrete(name = "log-probs", labels = c("log-based", "naiive"))
