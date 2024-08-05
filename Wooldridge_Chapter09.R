# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 09

# Chapter 09: Specification and Data Issues

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("quantreg")) install.packages("quantreg", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(lmtest)
library(car)
library(quantreg)
library(stargazer)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter09_log.txt", split = TRUE)

# Example: Example-9-2-automatic.R

data(hprice1, package = 'wooldridge')

# Original linear regression
cat("Original linear regression\n")
orig <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
summary(orig)

# RESET test using lmtest package
cat("RESET test\n")
reset_test <- resettest(orig)
print(reset_test)

# Example: Example-9-2-manual.R

data(hprice1, package = 'wooldridge')

# Original linear regression
cat("Original linear regression\n")
orig <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
summary(orig)

# Regression for RESET test
cat("Regression for RESET test\n")
RESETreg <- lm(price ~ lotsize + sqrft + bdrms + I(fitted(orig)^2) + I(fitted(orig)^3), data = hprice1)
summary(RESETreg)

# RESET test using linearHypothesis from car package
cat("RESET test using linearHypothesis from car package\n")
linearHypothesis(RESETreg, matchCoefs(RESETreg, "fitted"))

# Example: LAD.R

data(rdchem, package = 'wooldridge')

# OLS Regression
cat("OLS Regression\n")
ols <- lm(rdintens ~ I(sales / 1000) + profmarg, data = rdchem)
summary(ols)

# LAD Regression using quantreg package
cat("LAD Regression\n")
lad <- rq(rdintens ~ I(sales / 1000) + profmarg, data = rdchem)
summary(lad)

# Regression table using stargazer
cat("Regression table using stargazer\n")
stargazer(ols, lad, type = "text")

# Example: Missings-Analyses.R

data(lawsch85, package = 'wooldridge')

# Mean of a variable with missings
cat("Mean of LSAT including missings\n")
mean_lsat_incl <- mean(lawsch85$LSAT)
print(mean_lsat_incl)

cat("Mean of LSAT excluding missings\n")
mean_lsat_excl <- mean(lawsch85$LSAT, na.rm = TRUE)
print(mean_lsat_excl)

# Regression with missings
cat("Regression with missings\n")
reg_miss <- summary(lm(log(salary) ~ LSAT + cost + age, data = lawsch85))
print(reg_miss)

# Example: Missings.R

data(lawsch85, package = 'wooldridge')

# Extract LSAT
lsat <- lawsch85$LSAT

# Create logical indicator for missings
missLSAT <- is.na(lawsch85$LSAT)

# LSAT and indicator for Schools No. 120-129
cat("LSAT and indicator for Schools No. 120-129\n")
print(rbind(lsat, missLSAT)[, 120:129])

# Frequencies of indicator
cat("Frequencies of indicator\n")
print(table(missLSAT))

# Missings for all variables in data frame (counts)
cat("Missings for all variables in data frame (counts)\n")
print(colSums(is.na(lawsch85)))

# Indicator for complete cases
cat("Indicator for complete cases\n")
compl <- complete.cases(lawsch85)
print(table(compl))

# Example: NA-NaN-Inf.R

x <- c(-1, 0, 1, NA, NaN, -Inf, Inf)
logx <- log(x)
invx <- 1 / x
ncdf <- pnorm(x)
isna <- is.na(x)

cat("Data frame with NA, NaN, and Inf handling\n")
print(data.frame(x, logx, invx, ncdf, isna))

# Example: Nonnested-Test.R

data(hprice1, package = 'wooldridge')

# Two alternative models
cat("Two alternative models\n")
model1 <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
model2 <- lm(price ~ log(lotsize) + log(sqrft) + bdrms, data = hprice1)

# Test against comprehensive model using encomptest from lmtest package
cat("Test against comprehensive model\n")
encomp_test <- encomptest(model1, model2, data = hprice1)
print(encomp_test)

# Example: Outliers.R

data(rdchem, package = 'wooldridge')

# Regression
cat("Regression\n")
reg <- lm(rdintens ~ sales + profmarg, data = rdchem)
summary(reg)

# Studentized residuals for all observations
cat("Studentized residuals for all observations\n")
studres <- rstudent(reg)

# Display extreme values
cat("Extreme values of studentized residuals\n")
print(min(studres))
print(max(studres))

# Histogram (and overlayed density plot)
cat("Histogram of studentized residuals\n")
png(file = "output/chapter09_studentized_residuals_hist.png")
hist(studres, freq = FALSE)
lines(density(studres), lwd = 2)
dev.off()

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and tests for specification and data issues
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - RESET tests (automatic and manual)
# - LAD and OLS regressions
# - Handling and analyzing missing data
# - Non-nested model tests
# - Detection and analysis of outliers
