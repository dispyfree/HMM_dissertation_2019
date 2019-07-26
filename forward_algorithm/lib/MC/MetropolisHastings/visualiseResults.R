library(ggplot2)

progress <- estim_theta$progress
probTrace <- data.frame(p1 = progress$p1, p2 = progress$p2)

# convergence of state probs
chain_position <- 1:length(progress$p1)
colouring <- aes(colour = chain_position)
ggplot(data=probTrace, aes(x=p1, y=p2)) +
  geom_path(colouring) + 
  geom_point(colouring) +
  # add the values themselves
  geom_point(aes(x=0.3, y=0.9), colour="red", stroke=1) +
  geom_point(aes(x=0.9, y=0.3), colour="red", stroke=1) +
  geom_text(x = 0.8, y = 0, label="ground truth", colour = 'red') +
  ggtitle('Convergence of chain towards state probabilities')



#convergence of delta probs
# add minimal noise
sd <- 0.01
n1 <- rnorm(1:length(progress$delta1), mean = 0, sd=sd)
n2 <- rnorm(1:length(progress$delta1), mean = 0, sd=sd)

deltaTrace <- data.frame(delta1 = progress$delta1 + n1, 
                         delta2 = progress$delta2 + n1)
ggplot(data=deltaTrace, aes(x=delta1, y=delta2)) +
  geom_path(colouring) + 
  geom_point(colouring) +
  # add the values themselves
  geom_point(aes(x=0.66, y=0.33), colour="red", stroke=1) +
  geom_point(aes(x=0.33, y=0.66), colour="red", stroke=1) +
  geom_text(x = 0.8, y = 0, label="ground truth", colour = 'red') +
  ggtitle('Convergence of chain towards delta')



#convergence of gamma
gammaTrace <- data.frame(gamma11 = progress$gamma11, gamma22 = progress$gamma22)

ggplot(data=gammaTrace, aes(x=gamma11, y=gamma22)) +
  geom_path(colouring) + 
  geom_point(colouring) +
  # add the values themselves
  geom_point(aes(x=0.9, y=0.8), colour="red", stroke=1) +
  geom_point(aes(x=0.8, y=0.9), colour="red", stroke=1) +
  geom_text(x = 0.8, y = 0, label="ground truth", colour = 'red') +
  ggtitle('Convergence of chain towards gamma probabilities')

