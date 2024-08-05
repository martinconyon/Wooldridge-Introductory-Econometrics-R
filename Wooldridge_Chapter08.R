# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 08

# Chapter 08: Heteroscedasticity

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("foreign")) install.packages("foreign", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(lmtest)
library(car)
library(foreign)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter08_log.txt", split = TRUE)

# Example: Example-8-2-cont.R

data(gpa3, package = 'wooldridge')

# Estimate model (only for spring data)
cat("Estimate model (only for spring data)\n")
reg <- lm(cumgpa ~ sat + hsperc + tothrs + female + black + white, data = gpa3, subset = (spring == 1))
summary(reg)

# F-Tests using different variance-covariance formulas:
myH0 <- c("black", "white")
cat("Usual VCOV\n")
linearHypothesis(reg, myH0)
cat("Refined White VCOV\n")
linearHypothesis(reg, myH0, vcov = hccm)
cat("Classical White VCOV\n")
linearHypothesis(reg, myH0, vcov = hccm(reg, type = "hc0"))

# Example: Example-8-2.R

data(gpa3, package = 'wooldridge')

# Estimate model (only for spring data)
cat("Estimate model (only for spring data)\n")
reg <- lm(cumgpa ~ sat + hsperc + tothrs + female + black + white, data = gpa3, subset = (spring == 1))
summary(reg)

# Usual SE:
cat("Usual SE\n")
coeftest(reg)
# Refined White heteroscedasticity-robust SE:
cat("Refined White heteroscedasticity-robust SE\n")
coeftest(reg, vcov = hccm)

# Example: Example-8-4.R

data(hprice1, package = 'wooldridge')

# Estimate model
cat("Estimate model\n")
reg <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
summary(reg)

# Automatic BP test
cat("Automatic BP test\n")
bptest(reg)

# Manual regression of squared residuals
cat("Manual regression of squared residuals\n")
summary(lm(resid(reg)^2 ~ lotsize + sqrft + bdrms, data = hprice1))

# Example: Example-8-5.R

data(hprice1, package = 'wooldridge')

# Estimate model
cat("Estimate model\n")
reg <- lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data = hprice1)
summary(reg)

# BP test
cat("BP test\n")
bptest(reg)

# White test
cat("White test\n")
bptest(reg, ~fitted(reg) + I(fitted(reg)^2))

# Example: Example-8-6.R

cat("Load data for 401k\n")
d401k <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/401ksubs.dta")

# OLS (only for singles: fsize == 1)
cat("OLS (only for singles: fsize == 1)\n")
ols_reg <- lm(nettfa ~ inc + I((age - 25)^2) + male + e401k, data = d401k, subset = (fsize == 1))
summary(ols_reg)

# WLS
cat("WLS (Weighted Least Squares)\n")
wls_reg <- lm(nettfa ~ inc + I((age - 25)^2) + male + e401k, weight = 1/inc, data = d401k, subset = (fsize == 1))
summary(wls_reg)

# Example: Example-8-7.R

data(smoke, package = 'wooldridge')

# OLS
cat("OLS regression\n")
olsreg <- lm(cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = smoke)
summary(olsreg)

# BP test
cat("BP test\n")
bptest(olsreg)

# FGLS: estimation of the variance function
cat("FGLS: estimation of the variance function\n")
logu2 <- log(resid(olsreg)^2)
varreg <- lm(logu2 ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = smoke)
summary(varreg)

# FGLS: WLS
cat("FGLS: WLS\n")
w <- 1/exp(fitted(varreg))
fgls_reg <- lm(cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, weight = w, data = smoke)
summary(fgls_reg)

# Example: WLS-Robust.R

cat("Load data for 401k\n")
d401k <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/401ksubs.dta")

# WLS
cat("WLS\n")
wlsreg <- lm(nettfa ~ inc + I((age - 25)^2) + male + e401k, weight = 1/inc, data = d401k, subset = (fsize == 1))
summary(wlsreg)

# non-robust results
cat("Non-robust results\n")
coeftest(wlsreg)

# robust results (Refined White SE:)
cat("Robust results (Refined White SE:)\n")
coeftest(wlsreg, vcov = hccm)

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and tests for heteroscedasticity
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - F-Tests with different variance-covariance formulas
# - Breusch-Pagan (BP) tests
# - White tests
# - OLS and WLS estimations
# - FGLS estimations
