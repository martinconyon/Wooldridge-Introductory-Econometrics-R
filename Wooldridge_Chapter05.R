# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 05

# Chapter 05: Multiple Regression Analysis: OLS Asymptotics

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("officer")) install.packages("officer", dependencies = TRUE)
if (!require("flextable")) install.packages("flextable", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(ggplot2)
library(stargazer)
library(officer)
library(flextable)
library(car)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink("output/chapter05_log.txt")

# Example: LM Test for Additional Variables
data(crime1, package='wooldridge')

# 1. Estimate the restricted model
restr <- lm(narr86 ~ pcnv + ptime86 + qemp86, data = crime1)
summary(restr)

# 2. Regression of residuals from the restricted model
utilde <- resid(restr)
LMreg <- lm(utilde ~ pcnv + ptime86 + qemp86 + avgsen + tottime, data = crime1)
summary(LMreg)
r2 <- summary(LMreg)$r.squared
LM <- r2 * nobs(LMreg)
cat("LM test statistic:", LM, "\n")

# 4. Critical value from the chi-squared distribution, alpha = 10%
cat("Critical value (alpha = 10%):", qchisq(1 - 0.10, 2), "\n")

# Alternative to critical value: p-value
cat("p-value:", 1 - pchisq(LM, 2), "\n")

# Alternative: automatic F test
unrestr <- lm(narr86 ~ pcnv + ptime86 + qemp86 + avgsen + tottime, data = crime1)
print(linearHypothesis(unrestr, c("avgsen=0", "tottime=0")))

# Sim-Asy-OLS-chisq.R
n <- 100  # Example sample size
set.seed(1234567)
b0 <- 1
b1 <- 0.5
b1hat <- numeric(10000)
x <- rnorm(n, 4, 1)

for (j in 1:10000) {
  u <- (rchisq(n, 1) - 1) / sqrt(2)
  y <- b0 + b1 * x + u
  bhat <- coef(lm(y ~ x))
  b1hat[j] <- bhat["x"]
}

png(file="output/chapter05_b1hat_chisq.png")
hist(b1hat, main="Distribution of b1hat (Chi-squared Errors)", xlab="b1hat")
dev.off()

# Sim-Asy-OLS-norm.R
n <- 100  # Example sample size
set.seed(1234567)
b0 <- 1
b1 <- 0.5
b1hat <- numeric(10000)
x <- rnorm(n, 4, 1)

for (j in 1:10000) {
  u <- rnorm(n)
  y <- b0 + b1 * x + u
  bhat <- coef(lm(y ~ x))
  b1hat[j] <- bhat["x"]
}

png(file="output/chapter05_b1hat_norm.png")
hist(b1hat, main="Distribution of b1hat (Normal Errors)", xlab="b1hat")
dev.off()

# Sim-Asy-OLS-uncond.R
n <- 100  # Example sample size
set.seed(1234567)
b0 <- 1
b1 <- 0.5
b1hat <- numeric(10000)

for (j in 1:10000) {
  x <- rnorm(n, 4, 1)
  u <- rnorm(n)
  y <- b0 + b1 * x + u
  bhat <- coef(lm(y ~ x))
  b1hat[j] <- bhat["x"]
}

png(file="output/chapter05_b1hat_uncond.png")
hist(b1hat, main="Distribution of b1hat (Unconditional x, Normal Errors)", xlab="b1hat")
dev.off()

# Sim-Asy-OLS-unif.R
n <- 100  # Example sample size
set.seed(1234567)
b0 <- 1
b1 <- 0.5
b1hat <- numeric(10000)
x <- rnorm(n, 4, 1)

for (j in 1:10000) {
  u <- runif(n, -sqrt(3), sqrt(3))
  y <- b0 + b1 * x + u
  bhat <- coef(lm(y ~ x))
  b1hat[j] <- bhat["x"]
}

png(file="output/chapter05_b1hat_unif.png")
hist(b1hat, main="Distribution of b1hat (Uniform Errors)", xlab="b1hat")
dev.off()

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and simulations
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - LM Test for Additional Variables
# - Simulations for OLS Asymptotics with different distributions

# To run this script:
# 1. Set your working directory to the location where you saved this script.
#    Use setwd("path/to/your/folder") to do this.
# 2. Highlight each chunk of code and press Ctrl + Enter (Cmd + Enter on Mac) to execute it.
#    This allows you to see the output and understand each step of the analysis.

# End of Chapter 5 analysis
