set.seed(0)
oj <- read.csv("/Users/larali/Downloads/oj.csv")

# factor variable 
oj$brand <- factor(oj$brand)

# Non-bootstrap
OLS_model <- glm(log(sales) ~ log(price) + brand, data =oj)
b4_hat = 100*(exp(coef(OLS_model)['brandtropicana'])-1)
b4_hat

# Bootstrap
B <- 1000 
n <- nrow(oj)
bs_coeffs <- rep(NA, B)

for(b in 1:B){
  
  bs_indices <- sample.int(n, replace=TRUE) #resample data
  bs_data <- oj[bs_indices, ]
  OLS_model <- glm(log(sales) ~ log(price) + brand, data =bs_data)
  bs_coeffs[b]<- 100*(exp(coef(OLS_model)['brandtropicana'])-1) # calculate B4hat
}

# calculate 95% CI 
CI_b4 <- mean(bs_coeffs) + c(-2,2)*sd(bs_coeffs)
CI_b4

