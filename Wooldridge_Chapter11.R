# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 11

# Chapter 11: Further Issues in Using OLS with Time Series Data

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("dynlm")) install.packages("dynlm", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(dynlm)
library(stargazer)
library(zoo)
library(quantmod)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter11_log.txt", split = TRUE)

# Example: Example 11-4: Regression with Lags for NYSE Data

data(nyse, package = 'wooldridge')

# Define time series
cat("Defining time series data for NYSE returns\n")
tsdata <- ts(nyse)

# Linear regression of models with lags
cat("Running linear regressions with increasing lags\n")
reg1 <- dynlm(return ~ L(return), data = tsdata)
reg2 <- dynlm(return ~ L(return) + L(return, 2), data = tsdata)
reg3 <- dynlm(return ~ L(return) + L(return, 2) + L(return, 3), data = tsdata)

# Output regression results
cat("Outputting regression table for models with lags\n")
stargazer(reg1, reg2, reg3, type = "text", keep.stat = c("n", "rsq", "adj.rsq", "f"))

# Example: Example 11-6: Regression with First Differences for Fertility Data

data(fertil3, package = 'wooldridge')

# Define yearly time series beginning in 1913
cat("Defining yearly time series for fertility data\n")
tsdata <- ts(fertil3, start = 1913)

# Linear regression of model with first differences
cat("Running linear regression with first differences\n")
res1 <- dynlm(d(gfr) ~ d(pe), data = tsdata)

# Linear regression of model with lagged differences
cat("Running linear regression with lagged differences\n")
res2 <- dynlm(d(gfr) ~ d(pe) + L(d(pe)) + L(d(pe), 2), data = tsdata)

# Output regression results
cat("Outputting regression table for models with first and lagged differences\n")
stargazer(res1, res2, type = "text")

# Example: Example-EffMkts: Efficient Markets Hypothesis with AAPL Data

# Download data using the quantmod package
cat("Downloading AAPL data using quantmod package\n")
getSymbols("AAPL", auto.assign = TRUE)

# Calculate return as the log difference
cat("Calculating return as the log difference\n")
ret <- diff(log(AAPL$AAPL.Adjusted))

# Subset data from 2008 to 2016
cat("Subsetting data from 2008 to 2016\n")
ret <- ret["2008/2016"]

# Plot returns
cat("Plotting returns\n")
png(file = "output/chapter11_AAPL_returns.png")
plot(ret)
dev.off()

# Convert returns to zoo object for dynlm
ret <- as.zoo(ret)

# Linear regression of models with lags
cat("Running linear regressions with lags for AAPL returns\n")
reg1 <- dynlm(ret ~ L(ret))
reg2 <- dynlm(ret ~ L(ret) + L(ret, 2))
reg3 <- dynlm(ret ~ L(ret) + L(ret, 2) + L(ret, 3))

# Output regression results
cat("Outputting regression table for AAPL return models with lags\n")
stargazer(reg1, reg2, reg3, type = "text", keep.stat = c("n", "rsq", "adj.rsq", "f"))

# Example: Simulate-RandomWalk: Simulating a Random Walk

# Initialize Random Number Generator
cat("Initializing random number generator for random walk simulation\n")
set.seed(348546)

# Initial plot setup
cat("Setting up initial plot for random walk simulation\n")
png(file = "output/chapter11_random_walk.png")
plot(c(0, 50), c(0, 0), type = "l", lwd = 2, ylim = c(-18, 18))

# Loop over draws
cat("Simulating random walk\n")
for (r in 1:30) {
  # i.i.d. standard normal shock
  e <- rnorm(50)
  # Random walk as cumulative sum of shocks
  y <- ts(cumsum(e))
  # Add line to graph
  lines(y, col = gray(0.6))
}
dev.off()

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses with time series data from Wooldridge's "Introductory Econometrics".
# The analyses include:
# - Time series data handling and plotting
# - Linear regression models with lags
# - Regression with first differences
# - Simulation of random walks
# - Visualization of time series data
# - Efficient Markets Hypothesis testing with real stock data
# - Credit: Florian Heiss
