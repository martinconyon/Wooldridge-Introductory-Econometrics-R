# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 03

# Chapter 03: Multiple Regression Analysis: Estimation

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

# Load necessary packages
library(wooldridge)
library(ggplot2)
library(stargazer)
library(officer)
library(flextable)

# Clear environment
rm(list = ls())

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Example 3.1: Determinants of College GPA
data("gpa1")
summary(gpa1$act)
model_3_1 <- lm(colGPA ~ hsGPA + ACT, data = gpa1)
summary(model_3_1)
model_3_1b <- lm(colGPA ~ ACT, data = gpa1)
summary(model_3_1b)

# Example 3.2: Hourly Wage Equation
data("wage1")

# Example 3.3: Participation in 401(K) Pension Plan
data("k401k")
summary(k401k[c("prate", "mrate", "age")])
model_3_3 <- lm(prate ~ mrate + age, data = k401k)
summary(model_3_3)
model_3_3b <- lm(prate ~ mrate, data = k401k)
summary(model_3_3b)

# Example 3.4: Determinants of College GPA
model_3_4 <- lm(colGPA ~ hsGPA + ACT, data = gpa1)
summary(model_3_4)

# Example 3.5: Explaining Arrest Records
data("crime1")
model_3_5 <- lm(narr86 ~ pcnv + ptime86 + qemp86, data = crime1)
summary(model_3_5)

# Change in the predicted number of arrests when proportion of convictions increases by .5 for 1 man
change_1 <- coef(model_3_5)["pcnv"] * .5
change_1

# Change in the predicted number of arrests when proportion of convictions increases by .5 for 100 men
change_100 <- coef(model_3_5)["pcnv"] * 100 * .5
change_100

# Change in the predicted number of arrests when prison term increases by 12
change_prison <- coef(model_3_5)["ptime86"] * 12
change_prison

# Change in the predicted number of arrests when legal employment increases by a quarter for 100 men
change_employment <- coef(model_3_5)["qemp86"] * 100
change_employment

model_3_5b <- lm(narr86 ~ pcnv + avgsen + ptime86 + qemp86, data = crime1)
summary(model_3_5b)

# Example 3.6: Hourly Wage Equation
model_3_6 <- lm(lwage ~ educ, data = wage1)
summary(model_3_6)

# Save individual regression results
stargazer(model_3_1, type = "text", out = "output/Chapter3_Example3_1_RegressionResults.txt", digits = 3)
stargazer(model_3_1b, type = "text", out = "output/Chapter3_Example3_1b_RegressionResults.txt", digits = 3)
stargazer(model_3_3, type = "text", out = "output/Chapter3_Example3_3_RegressionResults.txt", digits = 3)
stargazer(model_3_3b, type = "text", out = "output/Chapter3_Example3_3b_RegressionResults.txt", digits = 3)
stargazer(model_3_4, type = "text", out = "output/Chapter3_Example3_4_RegressionResults.txt", digits = 3)
stargazer(model_3_5, type = "text", out = "output/Chapter3_Example3_5_RegressionResults.txt", digits = 3)
stargazer(model_3_5b, type = "text", out = "output/Chapter3_Example3_5b_RegressionResults.txt", digits = 3)
stargazer(model_3_6, type = "text", out = "output/Chapter3_Example3_6_RegressionResults.txt", digits = 3)

# Save regression results as CSV
summary_log_wage_model <- summary(model_3_6)
coef_log_wage_model <- as.data.frame(summary_log_wage_model$coefficients)
coef_log_wage_model <- round(coef_log_wage_model, 3) # Round to 3 decimal places
write.csv(coef_log_wage_model, file = "output/Chapter3_Example6_RegressionResults.csv")

# Save regression results as Word document
doc <- read_docx()
reg_table <- flextable(coef_log_wage_model)
doc <- body_add_flextable(doc, value = reg_table)
print(doc, target = "output/Chapter3_Example6_RegressionResults.docx")

# Save regression results as LaTeX
stargazer(model_3_6, type = "latex", out = "output/Chapter3_Example6_RegressionResults.tex", digits = 3)

# Explanation:
# This script performs multiple regression analyses on various datasets
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - Example 3.1: Determinants of College GPA
# - Example 3.2: Hourly Wage Equation
# - Example 3.3: Participation in 401(K) Pension Plan
# - Example 3.4: Determinants of College GPA
# - Example 3.5: Explaining Arrest Records
# - Example 3.6: Hourly Wage Equation

# To run this script:
# 1. Set your working directory to the location where you saved this script.
#    Use setwd("path/to/your/folder") to do this.
# 2. Highlight each chunk of code and press Ctrl + Enter (Cmd + Enter on Mac) to execute it.
#    This allows you to see the output and understand each step of the analysis.

# End of Chapter 3 analysis
