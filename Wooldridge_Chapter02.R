# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 02

# Chapter 02: The Simple Regression Model

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

# Load the data
data("wage1")

# Descriptive statistics
summary(wage1)

# Regression analysis
log_wage_model <- lm(wage ~ educ, data = wage1)

# Print the summary of the regression model
summary(log_wage_model)

# Scatter plot with fitted regression line
ggplot(wage1, aes(x = educ, y = wage)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter plot of Wage vs. Education", x = "Years of Education", y = "Wage")

# Save the plot
ggsave("output/Chapter2_Example2_Scatterplot.png")

# Regression diagnostics
wage1$residuals <- residuals(log_wage_model)
wage1$fitted <- fitted(log_wage_model)

# Plot residuals
ggplot(wage1, aes(x = educ, y = residuals)) +
  geom_point(color = "darkgreen") +
  labs(title = "Residuals vs. Education", x = "Years of Education", y = "Residuals")

# Save the residuals plot
ggsave("output/Chapter2_Example2_Residuals.png")

# Save regression results as text
stargazer(log_wage_model, type = "text", out = "output/Chapter2_Example2_RegressionResults.txt", digits = 3)

# Save regression results as CSV
summary_log_wage_model <- summary(log_wage_model)
coef_log_wage_model <- as.data.frame(summary_log_wage_model$coefficients)
coef_log_wage_model <- round(coef_log_wage_model, 3) # Round to 3 decimal places
write.csv(coef_log_wage_model, file = "output/Chapter2_Example2_RegressionResults.csv")

# Save regression results as Word document
doc <- read_docx()
reg_table <- flextable(coef_log_wage_model)
doc <- body_add_flextable(doc, value = reg_table)
print(doc, target = "output/Chapter2_Example2_RegressionResults.docx")

# Save regression results as LaTeX
stargazer(log_wage_model, type = "latex", out = "output/Chapter2_Example2_RegressionResults.tex", digits = 3)

# Explanation:
# This script performs a simple regression analysis on the `wage1` dataset
# from Wooldridge's "Introductory Econometrics". The analysis includes:
# - Descriptive statistics of the dataset
# - Running a linear regression of wage on education
# - Creating and saving a scatter plot with the regression line
# - Performing regression diagnostics and plotting residuals
# - Saving the regression results in various formats (text, CSV, Word, LaTeX)

# To run this script:
# 1. Set your working directory to the location where you saved this script.
#    Use setwd("path/to/your/folder") to do this.
# 2. Highlight each chunk of code and press Ctrl + Enter (Cmd + Enter on Mac) to execute it.
#    This allows you to see the output and understand each step of the analysis.

# End of Chapter 2 analysis
