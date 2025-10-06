# HW 1 
# Q3 

n <- 100 # number of samples
B <- 1000 # choose a large number of repetitions

means <- rep(0, B) # placeholder vector of zeros we will fill up with means
for (b in 1:B) {
  sample <- rexp(n)
  means[b] <- mean(sample) # means[b] denotes the b-th element of 'means' vector
}
49/85
hist(means, freq=FALSE, xlim=c(0.6,1.4))
xgrid <- seq(0.6,1.4,length=1000)
lines(xgrid, dnorm(xgrid, 1, 1/sqrt(n)), col="red")