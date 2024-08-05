# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 10

# Chapter 10: Basic Regression Analysis with Time Series Data

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("xts")) install.packages("xts", dependencies = TRUE)
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(xts)
library(zoo)
library(lmtest)
library(car)
library(stargazer)
library(quantmod)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter10_log.txt", split = TRUE)

# Example: Example 10.2: Effects of Inflation and Deficits on Interest Rates

data("intdef", package = "wooldridge")

# Load eXtensible Time Series package. xts is excellent for time series plots and properly indexing time series.
cat("Creating xts object from data.frame\n")
# Index year as yearmon class of monthly data. Adding 11/12 sets the month to December (end of year).
index <- zoo::as.yearmon(intdef$year + 11/12)

# Create the xts object, ordering by the index above.
intdef.xts <- xts(intdef[,-1], order.by = index)

# Extract 3-month Tbill, inflation, and deficit data
intdef.xts <- intdef.xts[, c("i3", "inf", "def")]

# Rename columns for clarity
colnames(intdef.xts) <- c("Tbill3mo", "cpi", "deficit")

# Plot the time series data
cat("Plotting time series data: Tbill3mo, cpi, and deficit\n")
png(file = "output/chapter10_inflation_deficit_interest_plot.png")
plot(x = intdef.xts, main = "Inflation, Deficits, and Interest Rates", legend.loc = "topleft")
dev.off()

# Linear regression model
cat("Running linear regression model\n")
tbill_model <- lm(Tbill3mo ~ cpi + deficit, data = intdef.xts)
summary(tbill_model)

# Output regression table
cat("Generating regression table\n")
stargazer(type = "text", tbill_model, single.row = TRUE, header = FALSE, digits = 5)

# Example: Updating the example with current data from FRED using quantmod
cat("Downloading and processing data from FRED\n")


# DO NOT RUN the following section

# library(quantmod)
# 
# # Tbill, 3 month
# getSymbols("TB3MS", src = "FRED")
# TB3MS <- to.yearly(TB3MS, OHLC=FALSE, drop.time = TRUE)
# index(TB3MS) <- zoo::as.yearmon(index(TB3MS))
# 
# # Inflation
# getSymbols("FPCPITOTLZGUSA", src = "FRED")
# index(FPCPITOTLZGUSA) <- zoo::as.yearmon(index(FPCPITOTLZGUSA)) + 11/12
# inflation <- FPCPITOTLZGUSA
# colnames(inflation) <- "inflation"
# 
# # Deficit, percent of GDP: Federal outlays - federal receipts
# getSymbols("FYFRGDA188S", src = "FRED")
# index(FYFRGDA188S) <- zoo::as.yearmon(index(FYFRGDA188S)) + 11/12
# outlays <- FYFRGDA188S
# colnames(outlays) <- "outlays"
# 
# getSymbols("FYONGDA188S", src = "FRED")
# index(FYONGDA188S) <- zoo::as.yearmon(index(FYONGDA188S)) + 11/12
# receipts <- FYONGDA188S
# colnames(receipts) <- "receipts"
# 
# # Create deficits from outlays - receipts
# deficit <- outlays - receipts
# colnames(deficit) <- "deficit"
# 
# # Merge and remove leading and trailing NAs for a balanced data matrix
# intdef_updated <- merge(TB3MS, inflation, deficit)
# intdef_updated <- zoo::na.trim(intdef_updated)
# 
# # Plot the updated data
# png(file = "output/chapter10_updated_inflation_deficit_interest_plot.png")
# plot(intdef_updated, main = "T-bill (3mo rate), inflation, and deficit (% of GDP)", legend.loc = "topright")
# dev.off()
# 
# # Run the updated model
# updated_model <- lm(TB3MS ~ inflation + deficit, data = intdef_updated)
# 
# # Output regression table for the updated model
# stargazer(type = "text", updated_model, single.row = TRUE, header = FALSE, digits = 5)

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses with time series data from Wooldridge's "Introductory Econometrics".
# The analyses include:
# - Time series data handling and plotting
# - Linear regression model fitting
# - Visualization of time series data
# - Downloading and processing financial data using quantmod package
# - Calculating and merging deficit data
# - Updating regression models with current data
# - Credit Justin Shea -- https://github.com/JustinMShea/wooldridge/blob/master/vignettes/Introductory-Econometrics-Examples.Rmd


