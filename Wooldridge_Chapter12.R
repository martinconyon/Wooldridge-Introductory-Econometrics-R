# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 12

# Chapter 12: Serial Correlation and Heteroskedasticity in Time Series Regressions

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("dynlm")) install.packages("dynlm", dependencies = TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("orcutt")) install.packages("orcutt", dependencies = TRUE)
if (!require("sandwich")) install.packages("sandwich", dependencies = TRUE)
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(dynlm)
library(lmtest)
library(car)
library(orcutt)
library(sandwich)
library(zoo)
library(quantmod)
library(stargazer)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter12_log.txt", split = TRUE)

# Example: Static Phillips Curve and Expectations-Augmented Phillips Curve

data(phillips, package = 'wooldridge')

# Define Yearly time series beginning in 1948
cat("Defining Yearly time series beginning in 1948 for Phillips data\n")
tsdata <- ts(phillips, start = 1948)

# Estimation of static Phillips curve
cat("Estimating static Phillips curve\n")
reg.s <- dynlm(inf ~ unem, data = tsdata, end = 1996)

# Residuals and AR(1) test
cat("Conducting AR(1) test on residuals of static Phillips curve\n")
residual.s <- resid(reg.s)
coeftest(dynlm(residual.s ~ L(residual.s)))

# Expectations-augmented Phillips curve
cat("Estimating expectations-augmented Phillips curve\n")
reg.ea <- dynlm(d(inf) ~ unem, data = tsdata, end = 1996)
residual.ea <- resid(reg.ea)
coeftest(dynlm(residual.ea ~ L(residual.ea)))

# Example: Breusch-Godfrey Test for Autocorrelation

data(barium, package = 'wooldridge')

# Define monthly time series beginning in Feb. 1978
cat("Defining monthly time series beginning in Feb. 1978 for barium data\n")
tsdata <- ts(barium, start = c(1978, 2), frequency = 12)

# Regression model
cat("Running regression model\n")
reg <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) +
               befile6 + affile6 + afdec6, data = tsdata)

# Breusch-Godfrey test manually
cat("Conducting manual Breusch-Godfrey test for autocorrelation\n")
residual <- resid(reg)
resreg <- dynlm(residual ~ L(residual) + L(residual, 2) + L(residual, 3) +
                  log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6, data = tsdata)
linearHypothesis(resreg, c("L(residual)", "L(residual, 2)", "L(residual, 3)"))

# Automatic Breusch-Godfrey test
cat("Conducting automatic Breusch-Godfrey test for autocorrelation\n")
bgtest(reg, order = 3, type = "F")

# Example: Cochrane-Orcutt Procedure for Serial Correlation

data(barium, package = 'wooldridge')

# Define monthly time series beginning in Feb. 1978
cat("Defining monthly time series beginning in Feb. 1978 for barium data\n")
tsdata <- ts(barium, start = c(1978, 2), frequency = 12)

# OLS estimation
cat("Running OLS estimation\n")
olsres <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) +
                  befile6 + affile6 + afdec6, data = tsdata)

# Cochrane-Orcutt estimation
cat("Running Cochrane-Orcutt estimation\n")
cochrane.orcutt(olsres)

# Example: Heteroskedasticity and Autocorrelation Consistent Standard Errors

data(prminwge, package = 'wooldridge')

# Define yearly time series beginning in 1950
cat("Defining yearly time series beginning in 1950 for prminwge data\n")
tsdata <- ts(prminwge, start = 1950)

# OLS regression
cat("Running OLS regression\n")
reg <- dynlm(log(prepop) ~ log(mincov) + log(prgnp) + log(usgnp) + trend(tsdata), data = tsdata)

# Results with usual SE
cat("Results with usual standard errors\n")
coeftest(reg)

# Results with HAC SE
cat("Results with heteroskedasticity and autocorrelation consistent (HAC) standard errors\n")
coeftest(reg, vcovHAC)

# Example: ARCH Model for Conditional Heteroskedasticity

data(nyse, package = 'wooldridge')

# Define time series
cat("Defining time series data for NYSE returns\n")
tsdata <- ts(nyse)

# Linear regression of model
cat("Running linear regression model\n")
reg <- dynlm(return ~ L(return), data = tsdata)

# Squared residuals
cat("Calculating squared residuals\n")
residual.sq <- resid(reg)^2

# Model for squared residuals
cat("Running model for squared residuals\n")
ARCHreg <- dynlm(residual.sq ~ L(residual.sq))
coeftest(ARCHreg)

# Example: Downloading and Analyzing Financial Data with Quantmod

cat("Downloading AAPL data using quantmod package\n")
getSymbols("AAPL", auto.assign = TRUE)

# Calculate return as the log difference
cat("Calculating return as the log difference\n")
ret <- diff(log(AAPL$AAPL.Adjusted))
# Subset data from 2008 to 2016
cat("Subsetting data from 2008 to 2016\n")
ret <- ret["2008/2016"]

# AR(1) model for returns
cat("Running AR(1) model for returns\n")
ret <- as.zoo(ret)
reg <- dynlm(ret ~ L(ret))

# Squared residuals
cat("Calculating squared residuals\n")
residual.sq <- resid(reg)^2

# Model for squared residuals
cat("Running model for squared residuals\n")
ARCHreg <- dynlm(residual.sq ~ L(residual.sq))
summary(ARCHreg)

# Example: Durbin-Watson Test for Serial Correlation

data(phillips, package = 'wooldridge')

# Define yearly time series beginning in 1948
cat("Defining yearly time series beginning in 1948 for Phillips data\n")
tsdata <- ts(phillips, start = 1948)

# Estimation of static Phillips curve
cat("Estimating static Phillips curve\n")
reg.s <- dynlm(inf ~ unem, data = tsdata, end = 1996)

# Estimation of expectations-augmented Phillips curve
cat("Estimating expectations-augmented Phillips curve\n")
reg.ea <- dynlm(d(inf) ~ unem, data = tsdata, end = 1996)

# Durbin-Watson tests
cat("Conducting Durbin-Watson tests\n")
dwtest(reg.s)
dwtest(reg.ea)

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses with time series data from Wooldridge's "Introductory Econometrics".
# The analyses include:
# - Static and expectations-augmented Phillips curves
# - Breusch-Godfrey test for autocorrelation
# - Cochrane-Orcutt procedure for serial correlation
# - Heteroskedasticity and autocorrelation consistent standard errors (HAC SE)
# - ARCH model for conditional heteroskedasticity
# - Downloading and analyzing financial data with quantmod
# - Durbin-Watson test for serial correlation
#
# Credit: Code from Florian Heiss, Professor for Statistics and Econometrics at the Heinrich Heine University of Düsseldorf.
# Link: "http://urfie.net/code.html"
