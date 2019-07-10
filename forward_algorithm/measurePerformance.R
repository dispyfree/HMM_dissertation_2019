
# now show that the algorithm runs in T * m^2
testNoEmissions <- 10

testCombin <- function(T, m){
  # initial dist is just equal distribution
  delta <- (1/m) * rep.int(1, m)
  gamma <- matrix(rnorm(m * m), nrow = m)
  gamma <- normalizeRows(gamma)
  
  #just dummy giving 1/m for _any_ value
  P <- c()
  for (i in 1:m){
    probs <- rep.int(1, m)
    probs[i] <- m
    probs <- normalizeRow(probs)
    P <- c(P, function(x){
      ddiscrete(x, probs)
    })
  }
  
  # generate from _first_ state always
  probs   <- rep.int(1, m)
  probs[1]<-m
  probs <- normalizeRow(probs)
  obs <- data.frame(obs = rdiscrete(T, probs), time = rep.int(1, T))
  
  equalDelta <- (1/m) * rep.int(1, m)
  estimProb(equalDelta, gamma, P, obs) 
}

# normalizes rows by making sure they sum to 1 and individual values are in [0,1]
normalizeRows <- function(mat){
  m <- dim(mat)
  m <- m[1]
  for ( i in 1:m){
    mat[i, ] <- mat[i, ] - min(mat[i, ])
    mat[i, ] <- mat[i, ] / sum(mat[i, ])
  }
  mat
}

normalizeRow <- function(row){
  if(min(row) < 0){
    row <- row - min(row)
  }
  row <- row / sum(row)
}


tValues <- c(50)
mValues <- 1:30 * 100
noReplicates <- 3

M <- expand.grid(tValues, mValues)

res <- mapply(function(t, m){
  system.time(replicate(testCombin(t, m), noReplicates))[["elapsed"]] / noReplicates
}, M$Var1, M$Var2);

results <- data.frame(noStates = mValues, timeElapsed = res)


# https://ggplot2.tidyverse.org/reference/labs.html

ggplot(data=results, aes(x=mValues, y=timeElapsed)) +
  geom_line()+
  geom_point()+
  labs(x = 'number of states', y = 'time elapsed in seconds')+
  ggtitle('computational complexity of forward algorithm')
