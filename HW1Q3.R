# HW 1 
# Q3 

set.seed(0)

##############
### n = 10 ###
##############
n <- 10 # number of samples
B <- 1000 # number of repetitions

means <- rep(0, B)
se <- rep(0,B)
lower_ci <- rep(0,B)
upper_ci <- rep(0,B)

# populating sample mean 
for (b in 1:B) {
  sample <- rpois(n, lambda = 5)
  
  means[b] <- mean(sample) # calculate sample mean
  se[b] <- sd(sample) / (sqrt(n)) # calculate sample se
  
  lower_ci[b] <- means[b] - (2*se[b]) # calculate sample CI lower bound
  upper_ci[b] <- means[b] + (2*se[b]) # calculate sample CI upper bound
  
}

# a. plot histogram of means
hist(means, freq=FALSE, main = "Histogram of Means, n=10")

# b. plot histogram of lower & upper CI 
hist(lower_ci, freq = FALSE, main = "Histogram of Lower 95% CI, n = 10", xlab = "Lower endpoint")
hist(upper_ci, freq = FALSE, main = "Histogram of Upper 95% CI, n = 10", xlab = "Upper endpoint")

# c. proportion of B containing population mean 
prop <- mean(lower_ci <= 5 & upper_ci >= 5)
prop

# d. BOOTSTRAP 
Bsim = 100 
Bboot = 100 
bs_means <- rep(NA, Bboot)
bs_lower_ci <- rep(NA, Bsim)
bs_upper_ci <- rep(NA, Bsim)

for (bs in 1:Bsim) {
  sample <- rpois(n, lambda=5)
  
  for(bb in 1:Bboot){
    bs_indices <- sample.int(n, size = n, replace=TRUE) 
    bs_sample <- sample[bs_indices]
    bs_means[bb]<- mean(bs_sample)
  }
  bs_lower_ci[bs] <- mean(sample) + -2 * sd(bs_means)
  bs_upper_ci[bs] <- mean(sample) + 2 * sd(bs_means)
}

# repeat of part b and c, bootstrap histograms 
hist(bs_lower_ci, freq = FALSE, main = "Histogram of Lower 95% CI Bootstrap, n = 10", xlab = "Lower endpoint")
hist(bs_upper_ci, freq = FALSE, main = "Histogram of Upper 95% CI Bootstrap, n = 10", xlab = "Upper endpoint")
prop <- mean(bs_lower_ci <= 5 & bs_upper_ci >= 5)
prop



##############
### n = 50 ###
##############
n <- 150 # number of samples
B <- 1000 # number of repetitions

means <- rep(0, B)
se <- rep(0,B)
lower_ci <- rep(0,B)
upper_ci <- rep(0,B)

# populating sample mean 
for (b in 1:B) {
  sample <- rpois(n, lambda = 5)
  
  means[b] <- mean(sample) # calculate sample mean
  se[b] <- sd(sample) / (sqrt(n)) # calculate sample se
  
  lower_ci[b] <- means[b] - (2*se[b]) # calculate sample CI lower bound
  upper_ci[b] <- means[b] + (2*se[b]) # calculate sample CI upper bound
  
}

# a. plot histogram of means
hist(means, freq=FALSE, main = "Histogram of Means, n=50")

# b. plot histogram of lower & upper CI 
hist(lower_ci, freq = FALSE, main = "Histogram of Lower 95% CI, n = 50", xlab = "Lower endpoint")
hist(upper_ci, freq = FALSE, main = "Histogram of Upper 95% CI, n = 50", xlab = "Upper endpoint")

# c. proportion of B containing population mean 
prop <- mean(lower_ci <= 5 & upper_ci >= 5)
prop

# d. BOOTSTRAP 
Bsim = 100 
Bboot = 100 
bs_means <- rep(NA, Bboot)
bs_lower_ci <- rep(NA, Bsim)
bs_upper_ci <- rep(NA, Bsim)

for (bs in 1:Bsim) {
  sample <- rpois(n, lambda=5)
  
  for(bb in 1:Bboot){
    bs_indices <- sample.int(n, size = n, replace=TRUE) 
    bs_sample <- sample[bs_indices]
    bs_means[bb]<- mean(bs_sample)
  }
  bs_lower_ci[bs] <- mean(sample) + -2 * sd(bs_means)
  bs_upper_ci[bs] <- mean(sample) + 2 * sd(bs_means)
}

# repeat of part b and c, bootstrap histograms 
hist(bs_lower_ci, freq = FALSE, main = "Histogram of Lower 95% CI Bootstrap, n = 50", xlab = "Lower endpoint")
hist(bs_upper_ci, freq = FALSE, main = "Histogram of Upper 95% CI Bootstrap, n = 50", xlab = "Upper endpoint")
prop <- mean(bs_lower_ci <= 5 & bs_upper_ci >= 5)
prop

##############
### n = 200 ##
##############
n <- 200 # number of samples
B <- 1000 # number of repetitions

means <- rep(0, B)
se <- rep(0,B)
lower_ci <- rep(0,B)
upper_ci <- rep(0,B)

# populating sample mean 
for (b in 1:B) {
  sample <- rpois(n, lambda = 5)
  
  means[b] <- mean(sample) # calculate sample mean
  se[b] <- sd(sample) / (sqrt(n)) # calculate sample se
  
  lower_ci[b] <- means[b] - (2*se[b]) # calculate sample CI lower bound
  upper_ci[b] <- means[b] + (2*se[b]) # calculate sample CI upper bound
  
}

# a. plot histogram of means
hist(means, freq=FALSE, main = "Histogram of Means, n=200")

# b. plot histogram of lower & upper CI 
hist(lower_ci, freq = FALSE, main = "Histogram of Lower 95% CI, n = 200", xlab = "Lower endpoint")
hist(upper_ci, freq = FALSE, main = "Histogram of Upper 95% CI, n = 200", xlab = "Upper endpoint")

# c. proportion of B containing population mean 
prop <- mean(lower_ci <= 5 & upper_ci >= 5)
prop

# d. BOOTSTRAP 
Bsim = 100 
Bboot = 100 
bs_means <- rep(NA, Bboot)
bs_lower_ci <- rep(NA, Bsim)
bs_upper_ci <- rep(NA, Bsim)

for (bs in 1:Bsim) {
  sample <- rpois(n, lambda=5) 
  
  for(bb in 1:Bboot){
    bs_indices <- sample.int(n, size = n, replace=TRUE) 
    bs_sample <- sample[bs_indices]
    bs_means[bb]<- mean(bs_sample)
  }
  bs_lower_ci[bs] <- mean(sample) + -2 * sd(bs_means)
  bs_upper_ci[bs] <- mean(sample) + 2 * sd(bs_means)
}

# repeat of part b and c, bootstrap histograms 
hist(bs_lower_ci, freq = FALSE, main = "Histogram of Lower 95% CI Bootstrap, n = 200", xlab = "Lower endpoint")
hist(bs_upper_ci, freq = FALSE, main = "Histogram of Upper 95% CI Bootstrap, n = 200", xlab = "Upper endpoint")
prop <- mean(bs_lower_ci <= 5 & bs_upper_ci >= 5)
prop
