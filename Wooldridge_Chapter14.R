# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 13

# Chapter 13: Panel Data Methods

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("plm")) install.packages("plm", dependencies = TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(plm)
library(lmtest)
library(stargazer)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter13_log.txt", split = TRUE)

# Example: OLS with Interaction Terms

data(cps78_85, package = 'wooldridge')

# Detailed OLS results including interaction terms
cat("Running OLS regression with interaction terms\n")
model_ols <- lm(lwage ~ y85 * (educ + female) + exper + I((exper^2) / 100) + union, data = cps78_85)
summary(model_ols)

# Example: Separate Regressions and Joint Regression

data(kielmc, package = 'wooldridge')

# Separate regressions for 1978 and 1981
cat("Running separate regressions for 1978 and 1981\n")
coef_1978 <- coef(lm(rprice ~ nearinc, data = kielmc, subset = (year == 1978)))
coef_1981 <- coef(lm(rprice ~ nearinc, data = kielmc, subset = (year == 1981)))

# Joint regression including an interaction term
cat("Running joint regression including an interaction term\n")
model_joint <- lm(rprice ~ nearinc * y81, data = kielmc)
coeftest(model_joint)

# Example: Difference-in-Differences (DiD)

cat("Running Difference-in-Differences (DiD) regression\n")
DiD <- lm(log(rprice) ~ nearinc * y81, data = kielmc)
DiDcontr <- lm(log(rprice) ~ nearinc * y81 + age + I(age^2) + log(intst) +
                 log(land) + log(area) + rooms + baths, data = kielmc)
stargazer(DiD, DiDcontr, type = "text")

# Example: First Differences (FD) Model

data(crime4, package = 'wooldridge')

# Convert data to panel data frame
cat("Converting crime4 data to panel data frame\n")
crime4.p <- pdata.frame(crime4, index = c("county", "year"))
pdim(crime4.p)

# Manually calculate first differences of crime rate
cat("Manually calculating first differences of crime rate\n")
crime4.p$dcrmrte <- diff(crime4.p$crmrte)

# Display selected variables for observations 1-9
cat("Displaying selected variables for observations 1-9\n")
print(crime4.p[1:9, c("county", "year", "crmrte", "dcrmrte")])

# Estimate FD model
cat("Estimating FD model\n")
model_fd <- plm(log(crmrte) ~ d83 + d84 + d85 + d86 + d87 + lprbarr + lprbconv + 
                  lprbpris + lavgsen + lpolpc, data = crime4.p, model = "fd")
coeftest(model_fd)

# Example: First Differences (FD) Model with crime2 Data

data(crime2, package = 'wooldridge')

# Convert data to panel data frame
cat("Converting crime2 data to panel data frame\n")
crime2.p <- pdata.frame(crime2, index = 46)

# Manually calculate first differences
cat("Manually calculating first differences\n")
crime2.p$dcrmrte <- diff(crime2.p$crmrte)
crime2.p$dunem <- diff(crime2.p$unem)

# Display selected variables for observations 1-6
cat("Displaying selected variables for observations 1-6\n")
print(crime2.p[1:6, c("id", "time", "year", "crmrte", "dcrmrte", "unem", "dunem")])

# Estimate FD model with lm on differenced data
cat("Estimating FD model with lm on differenced data\n")
model_fd_lm <- lm(dcrmrte ~ dunem, data = crime2.p)
coeftest(model_fd_lm)

# Estimate FD model with plm on original data
cat("Estimating FD model with plm on original data\n")
model_fd_plm <- plm(crmrte ~ unem, data = crime2.p, model = "fd")
coeftest(model_fd_plm)

# Example: Calculations within pdata.frame

data(crime4, package = 'wooldridge')

# Convert data to panel data frame
cat("Converting crime4 data to panel data frame\n")
crime4.p <- pdata.frame(crime4, index = c("county", "year"))

# Calculations within the pdata.frame
cat("Calculating lagged, differenced, between, and within transformations of crmrte\n")
crime4.p$cr.l <- lag(crime4.p$crmrte)
crime4.p$cr.d <- diff(crime4.p$crmrte)
crime4.p$cr.B <- Between(crime4.p$crmrte)
crime4.p$cr.W <- Within(crime4.p$crmrte)

# Display selected variables for observations 1-16
cat("Displaying selected variables for observations 1-16\n")
print(crime4.p[1:16, c("county", "year", "crmrte", "cr.l", "cr.d", "cr.B", "cr.W")])

# Example: Defining Panel Data Frame

data(crime2, package = 'wooldridge')

# Define panel data frame
cat("Defining panel data frame for crime2 data\n")
crime2.p <- pdata.frame(crime2, index = 46)

# Panel dimensions
cat("Displaying panel dimensions\n")
print(pdim(crime2.p))

# Display selected variables for observations 1-6
cat("Displaying selected variables for observations 1-6\n")
print(crime2.p[1:6, c("id", "time", "year", "pop", "crimes", "crmrte", "unem")])

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and panel data methods from Wooldridge's "Introductory Econometrics".
# The analyses include:
# - OLS with interaction terms
# - Separate regressions and joint regression
# - Difference-in-Differences (DiD) analysis
# - First Differences (FD) models
# - Calculations within pdata.frame
# - Defining panel data frames
#
# Credit: Code from Florian Heiss, Professor for Statistics and Econometrics at the Heinrich Heine University of Düsseldorf.
# Link: "http://urfie.net/code.html"
