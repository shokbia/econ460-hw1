#HW 1
#ECON 460
#

#Oct 6, 2025

#question 1
set.seed(0)
#bia setwd
setwd("/Users/biashok/DSCI/Fall2025/ECON460/data")
#read file
oj <- read.csv('oj.csv')
pickup_data <- read.csv('pickup.csv')
#pickup_data <- read.csv("C:/Users/rawre/Downloads/pickup.csv")

#1a
sample_correlation <- cor(pickup_data$miles, pickup_data$price)
print(paste("Sample correlation between miles and price:", 
            round(sample_correlation, 4)))

#1b
number_bootstrap_samples <- 1000 #set n of bootstrap samples
n_observations <- nrow(pickup_data) #see how many obs in dataset
bootstrap_correlations <- numeric(number_bootstrap_samples) #empty vector to store results

for(bootstrap_iteration in 1:number_bootstrap_samples) {
  resampled_indices <- sample(1:n_observations, n_observations, 
                              replace = TRUE)
  resampled_miles <- pickup_data$miles[resampled_indices]
  resampled_price <- pickup_data$price[resampled_indices]
  bootstrap_correlations[bootstrap_iteration] <- cor(resampled_miles, resampled_price)
}

#calculate 95% ci
confidence_interval_lower <- quantile(bootstrap_correlations, 0.025)  # 2.5th percentile
confidence_interval_upper <- quantile(bootstrap_correlations, 0.975)  # 97.5th percentile
print(paste(
  "95% CI for the population correlation using bootstrap:",
  round(confidence_interval_lower, 4), "to", round(confidence_interval_upper, 4)
))

#question 2
#set.seed(0)
#oj <- read.csv("/Users/baur/Downloads/oj (1).csv")

oj$minutemaid <- as.integer(oj$brand == "minute.maid")
oj$tropicana  <- as.integer(oj$brand == "tropicana")


y <- log(oj$sales)
X <- cbind(1, log(oj$price), oj$minutemaid, oj$tropicana)


beta_hat <- solve(t(X) %*% X) %*% (t(X) %*% y)
beta4_hat <- beta_hat[4]


percent_est <- (exp(beta4_hat) - 1) * 100
cat("Point estimate:", round(percent_est, 4), "%\n")


B <- 1000
n <- nrow(oj)
boot_vals <- numeric(B)

for (b in 1:B) {
  idx <- sample(1:n, n, replace = TRUE)
  Xb <- X[idx, ]
  yb <- y[idx]
  beta_b <- solve(t(Xb) %*% Xb) %*% (t(Xb) %*% yb)
  boot_vals[b] <- (exp(beta_b[4]) - 1) * 100
}

ci <- quantile(boot_vals, c(0.025, 0.975))
cat("95% CI:", sprintf("[%.4f%%, %.4f%%]", ci[1], ci[2]), "\n")

#question 3
#set.seed(0)

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

#question 4
#set.seed(0)
#setwd("~/Downloads/Fall 2025")

oj$log_sales <- log(oj$sales)
oj$log_price <- log(oj$price)

#fitting model (no intercept)
ols <- glm(log_sales ~ log_price-1, data=oj)
summary(ols)

#a) manual calculation of homoskedastic standard errors
#extract residuals
e <- residuals(ols)
n <- nrow(oj)

#create X matrix( just log_price, no intercept)
X <- as.matrix(oj$log_price)
d <- ncol(X)

# calculate s^2 = (1/(n-d)) * sum(e_i^2)
s2 <- sum(e^2) / (n - d)

# calculate (X'X)^(-1) using matrix operations from slides
XtX_inv <- solve(t(X) %*% X)
# standard error formula: sqrt(diag(s^2 * (X'X)^(-1)))
se_manual <- sqrt(diag(s2 * XtX_inv))

# get SE from glm summary
se_from_glm <- summary(ols)$coefficients["log_price", "Std. Error"]

# compare
cat("Standard Error from glm():", se_from_glm, "\n")
cat("Standard Error from manual calculation:", se_manual, "\n")
cat("Difference:", abs(se_from_glm - se_manual), "\n")

#b) heteroskedasticity-robust standard errors
library(sandwich)
library(lmtest)

#get heteroskedasticity-robust standard errors
coeftest(ols, vcov = sandwich)

#c): bootstrap standard errors with B = 1000
B <- 1000

# store bootstrap estimates
boot_betas <- numeric(B)

# bootstrap loop
for(b in 1:B) {
  # re-sample rows (observations) with replacement
  boot_indices <- sample(1:nrow(oj), replace = TRUE)
  boot_data <- oj[boot_indices, ]
  
  # fit model on bootstrap sample
  boot_ols <- glm(log_sales ~ log_price - 1, data = boot_data)
  
  # store the coefficient estimate
  boot_betas[b] <- coef(boot_ols)["log_price"]
}

# bootstrap standard error is the standard deviation of bootstrap estimates
boot_se <- sd(boot_betas)

cat("Bootstrap SE (B=1000):", boot_se, "\n")

#d) compare all three standard errors
cat("\n=== COMPARISON OF STANDARD ERRORS ===\n")
cat("Homoskedastic SE (part a):", se_from_glm, "\n")

# extract robust SE from part (b)
robust_se <- sqrt(diag(vcovHC(ols, type = "HC0")))
cat("Heteroskedastic-robust SE (part b):", robust_se, "\n")

cat("bootstrap SE (part c):", boot_se, "\n")

# calculate ratios for comparison
cat("\nRobust SE / Homoskedastic SE:", robust_se / se_from_glm, "\n")
cat("Bootstrap SE / Homoskedastic SE:", boot_se / se_from_glm, "\n")

