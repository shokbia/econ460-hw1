#question 1
set.seed(0)
pickup_data <- read.csv("C:/Users/rawre/Downloads/pickup.csv")

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
