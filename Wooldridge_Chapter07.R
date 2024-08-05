# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 07

# Chapter 07: Multiple Regression Analysis: Dummy Variables

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("AER")) install.packages("AER", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(car)
library(AER)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter07_log.txt", split = TRUE)

# Example: Dummy-Interact-Sep.R

data(gpa3, package = 'wooldridge')

# Estimate model for males (& spring data)
cat("Estimate model for males (& spring data)\n")
model_males <- lm(cumgpa ~ sat + hsperc + tothrs, data = gpa3, subset = (spring == 1 & female == 0))
summary(model_males)

# Estimate model for females (& spring data)
cat("Estimate model for females (& spring data)\n")
model_females <- lm(cumgpa ~ sat + hsperc + tothrs, data = gpa3, subset = (spring == 1 & female == 1))
summary(model_females)

# Example: Dummy-Interact.R

data(gpa3, package = 'wooldridge')

# Model with full interactions with female dummy (only for spring data)
cat("Model with full interactions with female dummy (only for spring data)\n")
reg <- lm(cumgpa ~ female * (sat + hsperc + tothrs), data = gpa3, subset = (spring == 1))
summary(reg)

# F-Test from package "car". H0: the interaction coefficients are zero
# matchCoefs(...) selects all coeffs with names containing "female"
cat("F-Test for interaction coefficients\n")
linearHypothesis(reg, matchCoefs(reg, "female"))

# Example: Example-7-1-logical.R

data(wage1, package = 'wooldridge')

# Replace "female" with logical variable
cat("Replace 'female' with logical variable\n")
wage1$female <- as.logical(wage1$female)
table(wage1$female)

# Regression with logical variable
cat("Regression with logical variable\n")
model_logical <- lm(wage ~ female + educ + exper + tenure, data = wage1)
summary(model_logical)

# Example: Example-7-1.R

data(wage1, package = 'wooldridge')

cat("Basic regression model\n")
model_basic <- lm(wage ~ female + educ + exper + tenure, data = wage1)
summary(model_basic)

# Example: Example-7-5.R

data(wage1, package = 'wooldridge')

cat("Regression model with log(wage)\n")
model_logwage <- lm(log(wage) ~ female + educ + exper + I(exper^2) + tenure + I(tenure^2), data = wage1)
summary(model_logwage)

# Example: Example-7-6.R

data(wage1, package = 'wooldridge')

cat("Regression model with married*female interaction\n")
model_interaction <- lm(log(wage) ~ married * female + educ + exper + I(exper^2) + tenure + I(tenure^2), data = wage1)
summary(model_interaction)

# Example: Example-7-8.R

data(lawsch85, package = 'wooldridge')

# Define cut points for the rank
cat("Define cut points for the rank\n")
cutpts <- c(0, 10, 25, 40, 60, 100, 175)

# Create factor variable containing ranges for the rank
cat("Create factor variable containing ranges for the rank\n")
lawsch85$rankcat <- cut(lawsch85$rank, cutpts)

# Display frequencies
cat("Display frequencies\n")
table(lawsch85$rankcat)

# Choose reference category
cat("Choose reference category\n")
lawsch85$rankcat <- relevel(lawsch85$rankcat, "(100,175]")

# Run regression
cat("Run regression\n")
res <- lm(log(salary) ~ rankcat + LSAT + GPA + log(libvol) + log(cost), data = lawsch85)
summary(res)

# ANOVA table
cat("ANOVA table\n")
car::Anova(res)

# Example: Regr-Factors-Anova.R

data(CPS1985, package = "AER")

# Regression
cat("Regression model with factors and ANOVA\n")
res <- lm(log(wage) ~ education + experience + gender + occupation, data = CPS1985)
summary(res)

# ANOVA table
cat("ANOVA table\n")
car::Anova(res)

# Example: Regr-Factors.R

data(CPS1985, package = "AER")

# Table of categories and frequencies for two factor variables:
cat("Table of categories and frequencies for 'gender'\n")
table(CPS1985$gender)
cat("Table of categories and frequencies for 'occupation'\n")
table(CPS1985$occupation)

# Directly using factor variables in regression formula:
cat("Directly using factor variables in regression formula\n")
model_factors <- lm(log(wage) ~ education + experience + gender + occupation, data = CPS1985)
summary(model_factors)

# Manually redefine the reference category:
cat("Manually redefine the reference category\n")
CPS1985$gender <- relevel(CPS1985$gender, "female")
CPS1985$occupation <- relevel(CPS1985$occupation, "management")

# Rerun regression:
cat("Rerun regression with redefined reference categories\n")
model_ref_cat <- lm(log(wage) ~ education + experience + gender + occupation, data = CPS1985)
summary(model_ref_cat)

# Close log file
sink()

# Explanation:
# This script performs multiple regression analyses and tests with dummy variables
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - Separate regressions for males and females
# - Full interaction models
# - Using logical variables in regressions
# - Regressions with log-transformed dependent variables
# - Factor variables and ANOVA tables
# - Credit: Code from Florian Heiss, Professor for Statistics and Econometrics at the Heinrich Heine University of Düsseldorf
# - Link: "http://urfie.net/code.html"
