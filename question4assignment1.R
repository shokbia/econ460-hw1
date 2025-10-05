# Homework 1 Question 4
set.seed(0)
setwd("~/Downloads/Fall 2025")

oj -> read.csv('oj.csv')

oj$log_sales <- log(oj$sales)
oj$log_price <- log(oj$price)

ols <- glm(log_sales ~ log_price, data=oj)
summary(ols)

library(sandwich)
library(lmtest)

coeftest(ols, vcov = sandwich)
