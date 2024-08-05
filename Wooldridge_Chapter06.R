# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 06

# Chapter 06: Multiple Regression Analysis: Further Issues

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script chunk by chunk to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("officer")) install.packages("officer", dependencies = TRUE)
if (!require("flextable")) install.packages("flextable", dependencies = TRUE)
if (!require("effects")) install.packages("effects", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)

# Load necessary packages
library(wooldridge)
library(ggplot2)
library(stargazer)
library(officer)
library(flextable)
library(effects)
library(car)

# Clear environment
rm(list = ls())

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Regressions and examples

# Example 6.1: Basic model for birth weight:
data(bwght, package='wooldridge')
lm( bwght ~ cigs + faminc, data=bwght)

# Weight in pounds, manual way:
bwght$bwghtlbs <- bwght$bwght / 16
lm( bwghtlbs ~ cigs + faminc, data=bwght)

# Weight in pounds, direct way:
lm( I(bwght / 16) ~ cigs + faminc, data=bwght)

# Packs of cigarettes:
lm( bwght ~ I(cigs / 20) + faminc, data=bwght)

# Effects-Automatic.R

# Repeating the regression from Example 6.2:
data(hprice2, package='wooldridge')
res <- lm( log(price) ~ log(nox) + log(dist) + rooms + I(rooms^2) + stratio, data = hprice2)

# Automatic effects plot using the package "effects"
plot( effect("rooms", res) )

# Effects-Manual.R

# Repeating the regression from Example 6.2:
data(hprice2, package='wooldridge')
res <- lm( log(price) ~ log(nox) + log(dist) + rooms + I(rooms^2) + stratio, data = hprice2)

# Predictions: Values of the regressors:
# rooms = 4-8, all others at the sample mean:
X <- data.frame(rooms = seq(4, 8), nox = 5.5498, dist = 3.7958, stratio = 18.4593)

# Calculate predictions and confidence interval:
pred <- predict(res, X, interval = "confidence")

# Table of regressor values, predictions and CI:
cbind(X, pred)

# Plot 
matplot(X$rooms, pred, type="l", lty=c(1, 2, 2))

# Example-6-1.R

data(hprice2, package='wooldridge')

# Estimate model with standardized variables:
lm(scale(price) ~ 0 + scale(nox) + scale(crime) + scale(rooms) + scale(dist) + scale(stratio), data=hprice2)

# Example-6-2-Anova.R

data(hprice2, package='wooldridge')
res <- lm(log(price) ~ log(nox) + log(dist) + poly(rooms, 2, raw=TRUE) + stratio, data=hprice2)

# Manual F test for rooms:
linearHypothesis(res, matchCoefs(res, "rooms"))

# ANOVA (type 2) table:
Anova(res)

# Example-6-2.R

data(hprice2, package='wooldridge')
res <- lm(log(price) ~ log(nox) + log(dist) + rooms + I(rooms^2) + stratio, data=hprice2)
summary(res)

# Using poly(...):
res <- lm(log(price) ~ log(nox) + log(dist) + poly(rooms, 2, raw=TRUE) + stratio, data=hprice2)
summary(res)

# Example-6-3.R

data(attend, package='wooldridge')

# Estimate model with interaction effect:
(myres <- lm(stndfnl ~ atndrte * priGPA + ACT + I(priGPA^2) + I(ACT^2), data=attend))

# Estimate for partial effect at priGPA = 2.59:
b <- coef(myres)
b["atndrte"] + 2.59 * b["atndrte:priGPA"]

# Test partial effect for priGPA = 2.59:
linearHypothesis(myres, c("atndrte + 2.59 * atndrte:priGPA"))

# Example-6-5.R

data(gpa2, package='wooldridge')

# Regress and report coefficients
reg <- lm(colgpa ~ sat + hsperc + hsize + I(hsize^2), data=gpa2)
reg

# Generate data set containing the regressor values for predictions
cvalues <- data.frame(sat = 1200, hsperc = 30, hsize = 5)

# Point estimate of prediction
predict(reg, cvalues)

# Point estimate and 95% confidence interval
predict(reg, cvalues, interval = "confidence")

# Define three sets of regressor variables
cvalues <- data.frame(sat = c(1200, 900, 1400), hsperc = c(30, 20, 5), hsize = c(5, 3, 1))
cvalues

# Point estimates and 99% confidence intervals for these
predict(reg, cvalues, interval = "confidence", level = 0.99)

# Example-6-6.R

data(gpa2, package='wooldridge')

# Regress (as before)
reg <- lm(colgpa ~ sat + hsperc + hsize + I(hsize^2), data=gpa2)

# Define three sets of regressor variables (as before)
cvalues <- data.frame(sat = c(1200, 900, 1400), hsperc = c(30, 20, 5), hsize = c(5, 3, 1))

# Point estimates and 95% prediction intervals for these
predict(reg, cvalues, interval = "prediction")

# Formula-Logarithm.R

data(hprice2, package='wooldridge')

# Estimate model with logs:
lm(log(price) ~ log(nox) + rooms, data=hprice2)

# Save individual regression results (if applicable)
# For each regression, you can save the results as text, CSV, Word, or LaTeX as done previously.

# Explanation:
# This script performs multiple regression analyses on various datasets
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - Example 6.1: Basic Model for Birth Weight
# - Example 6.2: Effects of Job Training
# - Example 6.3: Return on Investment in Training
# - Example 6.4: Effects of Schooling on Wage
# - Example 6.5: Returns to Education
# - Example 6.6: Union Wage Premium
