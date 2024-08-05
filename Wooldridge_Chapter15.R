# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 15

# Chapter 15: Instrumental Variables Estimation and Two-Stage Least Squares

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("AER")) install.packages("AER", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("plm")) install.packages("plm", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(AER)
library(stargazer)
library(plm)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter15_log.txt", split = TRUE)

# Example 15.1: OLS and IV Estimation for Wage Equation

data(mroz, package = 'wooldridge')

# Restrict to non-missing wage observations
cat("Restricting to non-missing wage observations\n")
oursample <- subset(mroz, !is.na(wage))

# OLS slope parameter manually
cat("Calculating OLS slope parameter manually\n")
ols_slope <- with(oursample, cov(log(wage), educ) / var(educ))
print(ols_slope)

# IV slope parameter manually
cat("Calculating IV slope parameter manually\n")
iv_slope <- with(oursample, cov(log(wage), fatheduc) / cov(educ, fatheduc))
print(iv_slope)

# OLS automatically
cat("Running OLS regression automatically\n")
reg_ols <- lm(log(wage) ~ educ, data = oursample)

# IV automatically
cat("Running IV regression automatically\n")
reg_iv <- ivreg(log(wage) ~ educ | fatheduc, data = oursample)

# Pretty regression table
cat("Displaying regression results\n")
stargazer(reg_ols, reg_iv, type = "text")

# Example 15.10: IV FD Regression

data(jtrain, package = 'wooldridge')

# Define panel data (for 1987 and 1988 only)
cat("Defining panel data for 1987 and 1988\n")
jtrain_87_88 <- subset(jtrain, year <= 1988)
jtrain_p <- pdata.frame(jtrain_87_88, index = c("fcode", "year"))

# IV FD regression
cat("Running IV FD regression\n")
iv_fd_reg <- plm(log(scrap) ~ hrsemp | grant, model = "fd", data = jtrain_p)
summary(iv_fd_reg)

# Example 15.4: Checking Relevance and IV Estimation

data(card, package = 'wooldridge')

# Checking for relevance: reduced form
cat("Running reduced form regression to check for relevance\n")
redf <- lm(educ ~ nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 +
             reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data = card)

# OLS regression
cat("Running OLS regression\n")
ols <- lm(log(wage) ~ educ + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 +
            reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data = card)

# IV estimation
cat("Running IV estimation\n")
iv <- ivreg(log(wage) ~ educ + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 + 
              reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669 | nearc4 + exper + 
              I(exper^2) + black + smsa + south + smsa66 + reg662 + reg663 + reg664 + reg665 + 
              reg666 + reg667 + reg668 + reg669, data = card)

# Pretty regression table of selected coefficients
cat("Displaying regression results for selected coefficients\n")
stargazer(redf, ols, iv, type = "text", keep = c("ed", "near", "exp", "bl"), keep.stat = c("n", "rsq"))

# Example 15.5: Two-Stage Least Squares (2SLS)

data(mroz, package = 'wooldridge')

# Restrict to non-missing wage observations
cat("Restricting to non-missing wage observations\n")
oursample <- subset(mroz, !is.na(wage))

# 1st stage: reduced form
cat("Running first stage regression (reduced form)\n")
stage1 <- lm(educ ~ exper + I(exper^2) + motheduc + fatheduc, data = oursample)

# 2nd stage
cat("Running second stage regression\n")
man_2SLS <- lm(log(wage) ~ fitted(stage1) + exper + I(exper^2), data = oursample)

# Automatic 2SLS estimation
cat("Running automatic 2SLS estimation\n")
aut_2SLS <- ivreg(log(wage) ~ educ + exper + I(exper^2) | motheduc + fatheduc + exper + I(exper^2), data = oursample)

# Pretty regression table
cat("Displaying 2SLS regression results\n")
stargazer(stage1, man_2SLS, aut_2SLS, type = "text", keep.stat = c("n", "rsq"))

# Example 15.7: Two-Stage Least Squares with Residuals

data(mroz, package = 'wooldridge')

# Restrict to non-missing wage observations
cat("Restricting to non-missing wage observations\n")
oursample <- subset(mroz, !is.na(wage))

# 1st stage: reduced form
cat("Running first stage regression (reduced form)\n")
stage1 <- lm(educ ~ exper + I(exper^2) + motheduc + fatheduc, data = oursample)

# 2nd stage with residuals
cat("Running second stage regression with residuals\n")
stage2 <- lm(log(wage) ~ educ + exper + I(exper^2) + resid(stage1), data = oursample)

# Results including t tests
cat("Displaying regression results with t tests\n")
coeftest(stage2)

# Example 15.8: Testing for Endogeneity

data(mroz, package = 'wooldridge')

# Restrict to non-missing wage observations
cat("Restricting to non-missing wage observations\n")
oursample <- subset(mroz, !is.na(wage))

# IV regression
cat("Running IV regression\n")
res_2sls <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + motheduc + fatheduc, data = oursample)

# Auxiliary regression
cat("Running auxiliary regression\n")
res_aux <- lm(resid(res_2sls) ~ exper + I(exper^2) + motheduc + fatheduc, data = oursample)

# Calculations for test
cat("Calculating test statistics\n")
r2 <- summary(res_aux)$r.squared
n <- nobs(res_aux)
teststat <- n * r2
pval <- 1 - pchisq(teststat, 1)
cat("R-squared:", r2, "\n")
cat("Number of observations:", n, "\n")
cat("Test statistic:", teststat, "\n")
cat("P-value:", pval, "\n")

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and instrumental variable methods from Wooldridge's "Introductory Econometrics".
# The analyses include:
# - OLS and IV estimation for wage equations
# - Checking relevance and IV estimation
# - Two-stage least squares (2SLS)
# - Testing for endogeneity
#
# Credit: Code from Florian Heiss, Professor for Statistics and Econometrics at the Heinrich Heine University of Düsseldorf.
# Link: "http://urfie.net/code.html"
