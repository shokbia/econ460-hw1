# Homework 1 Question 4
set.seed(0)
#setwd("~/Downloads/Fall 2025")
#bia setwd
setwd("/Users/biashok/DSCI/Fall2025/ECON460/data")

#read file
oj <- read.csv('oj.csv')

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

#written answer: 
#The bootstrap SE is noticeably smaller than the homoskedastic SE, suggesting potential efficiency gains or that the bootstrap captured different variance properties in this sample.
#Homoskedastic Standard Errors are the classical OLS standard errors that assume constant error variance (homoskedasticity). The main advantage is computational simplicity and efficiency when the homoskedasticity assumption holds true. However, if heteroskedasticity exists in the data, these standard errors are biased and inconsistent, leading to invalid hypothesis tests and confidence intervals. In modern applied work, this method is generally considered unreliable unless homoskedasticity can be verified.
#Heterooskedastic-Robust Standard Errors("sandwich") provides consistent standard errors even when heteroskedasticity is present. This method has become the default in applied econometrics because it remains valid under weaker assumptions without requiring explicit modeling of the variance structure. The main drawback is it requires large samples for good finite-sample performance and can be slightly less efficient than homoskedastic SEs when homoskedasticity actually holds. However, this small efficiency loss is typically worth the gain.
#Bootstrap Standard Errors makes minimal distributional assumptions and works by resampling data to empirically estimate sampling distribution of estimator. This approach is flexible and handles complex situations where analytical formulas are unavailable/unreliable. The primary disadvantage is computational cost bc it requires refitting model B times (1000 in our case). Additionally, bootstrap results have some random variation depending on the seed, though this becomes negligible with sufficient bootstrap replications.
#In this application, I would recommend using the heteroskedastic-robust standard errors as the primary inference method bc they provide valid inference regardless of heteroskedasticity presence, with minimal computational burden compared to the bootstrap.