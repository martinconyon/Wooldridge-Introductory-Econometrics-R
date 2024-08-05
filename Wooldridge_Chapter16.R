# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 16

# Chapter 16: Simultaneous Equations Models

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("AER")) install.packages("AER", dependencies = TRUE)
if (!require("systemfit")) install.packages("systemfit", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(AER)
library(systemfit)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter16_log.txt", split = TRUE)

# Example 16.5-3SLS: Three-Stage Least Squares

# This example demonstrates the use of Three-Stage Least Squares (3SLS) for estimating a system of equations.

# Ensure that you run the script Example-16-5-systemfit-prep.R first to define the system of equations and instruments.

cat("Running 3SLS for the system of equations\n")
data(mroz, package='wooldridge')
oursample <- subset(mroz, !is.na(wage))

# Define system of equations and instruments
eq.hrs   <- hours    ~ log(wage) + educ + age + kidslt6 + nwifeinc
eq.wage  <- log(wage) ~ hours    + educ + exper + I(exper^2)
eq.system<- list(eq.hrs, eq.wage)
instrum  <- ~educ + age + kidslt6 + nwifeinc + exper + I(exper^2)

# Run 3SLS estimation
cat("Running 3SLS estimation\n")
fit_3sls <- systemfit(eq.system, inst = instrum, data = oursample, method = "3SLS")
summary(fit_3sls)

# Example 16.5-IVREG: Instrumental Variables Regression

# This example demonstrates the use of 2SLS for individual equations using the AER package.

data(mroz, package='wooldridge')
oursample <- subset(mroz, !is.na(wage))

cat("Running 2SLS regressions for hours and wage equations\n")
# 2SLS regression for hours equation
ivreg_hours <- ivreg(hours ~ log(wage) + educ + age + kidslt6 + nwifeinc | educ + age + kidslt6 + nwifeinc + exper + I(exper^2), data = oursample)
summary(ivreg_hours)

# 2SLS regression for wage equation
ivreg_wage <- ivreg(log(wage) ~ hours + educ + exper + I(exper^2) | educ + age + kidslt6 + nwifeinc + exper + I(exper^2), data = oursample)
summary(ivreg_wage)

# Example 16.5-Systemfit-Prep: Preparation for System Estimation

# This script prepares the system of equations and instruments for system estimation using the systemfit package.

data(mroz, package='wooldridge')
oursample <- subset(mroz, !is.na(wage))

cat("Preparing system of equations and instruments for system estimation\n")
# Define system of equations and instruments
eq.hrs   <- hours    ~ log(wage) + educ + age + kidslt6 + nwifeinc
eq.wage  <- log(wage) ~ hours    + educ + exper + I(exper^2)
eq.system<- list(eq.hrs, eq.wage)
instrum  <- ~educ + age + kidslt6 + nwifeinc + exper + I(exper^2)

# Example 16.5-Systemfit: Two-Stage Least Squares for System

# This example demonstrates the use of Two-Stage Least Squares (2SLS) for estimating a system of equations.

# Ensure that you run the script Example-16-5-systemfit-prep.R first to define the system of equations and instruments.

cat("Running 2SLS for the system of equations\n")
fit_2sls <- systemfit(eq.system, inst = instrum, data = oursample, method = "2SLS")
summary(fit_2sls)

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and simultaneous equation methods from Wooldridge's "Introductory Econometrics".
# The analyses include:
# - Three-Stage Least Squares (3SLS)
# - Instrumental Variables Regression (2SLS)
# - System estimation preparation
# - Two-Stage Least Squares (2SLS) for system estimation
#
# Credit: Code from Florian Heiss, Professor for Statistics and Econometrics at the Heinrich Heine University of Düsseldorf.
# Link: "http://urfie.net/code.html"
