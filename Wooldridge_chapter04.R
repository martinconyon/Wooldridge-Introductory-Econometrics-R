# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 04

# Chapter 04: Multiple Regression Analysis: Inference

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

# Example 4.1: Hourly Wage Equation
data("wage1")
model_4_1 <- lm(lwage ~ educ + exper + tenure, data = wage1)
summary(model_4_1)
increase_exper_3 <- coef(model_4_1)["exper"] * 3
increase_exper_3

# Example 4.2: Student Performance and School Size
data("meap93")
meap93$ltotcomp <- log(meap93$totcomp)
meap93$lenroll <- log(meap93$enroll)
meap93$lstaff <- log(meap93$staff)
model_4_2 <- lm(math10 ~ totcomp + staff + enroll, data = meap93)
summary(model_4_2)
model_4_2_log <- lm(math10 ~ ltotcomp + lstaff + lenroll, data = meap93)
summary(model_4_2_log)
change_enroll_1 <- coef(model_4_2_log)["lenroll"] / 100
change_enroll_1

# Example 4.3: Determinants of College GPA
data("gpa1")
model_4_3 <- lm(colGPA ~ hsGPA + ACT + skipped, data = gpa1)
summary(model_4_3)

# Example 4.4: Campus Crime and Enrollment
data("campus")
model_4_4 <- lm(lcrime ~ lenroll, data = campus)
summary(model_4_4)

# T-statistics for testing the coefficient on lenroll equal to 1
t_value <- (coef(model_4_4)["lenroll"] - 1) / summary(model_4_4)$coefficients["lenroll", "Std. Error"]
t_value
p_value <- pt(t_value, df = summary(model_4_4)$df[2], lower.tail = FALSE)
p_value

# Example 4.5: Housing Prices and Air Pollution
data("hprice2")
hprice2$ldist <- log(hprice2$dist)
model_4_5 <- lm(lprice ~ lnox + ldist + rooms + stratio, data = hprice2)
summary(model_4_5)

# Example 4.6: Participation Rates in 401K Plans
data("k401k")
model_4_6 <- lm(prate ~ mrate + age + totemp, data = k401k)
summary(model_4_6)
change_totemp_10000 <- coef(model_4_6)["totemp"] * 10000
change_totemp_10000

# Example 4.7: Effect of Job Training Grants on Firm Scrap Rates
data("jtrain")
summary(jtrain[c("hrsemp", "lsales", "lemploy")])
model_4_7 <- lm(lscrap ~ hrsemp + lsales + lemploy, data = jtrain)
summary(model_4_7)
change_hrsemp_1 <- coef(model_4_7)["hrsemp"] * 1
change_hrsemp_1
change_hrsemp_5 <- coef(model_4_7)["hrsemp"] * 5
change_hrsemp_5

# Example 4.9: Parents Education in a Birth Weight Equation
data("bwght")
model_4_9 <- lm(bwght ~ cigs + parity + faminc + motheduc + fatheduc, data = bwght)
summary(model_4_9)
joint_significance_test <- anova(model_4_9)
joint_significance_test

# Example 4.10: Salary-Pension Tradeoff for Teachers
data("meap93")
meap93$lenrol <- log(meap93$enroll)
model_4_10 <- lm(lsalary ~ bensal + lenrol + lstaff + droprate + gradrate, data = meap93)
summary(model_4_10)
model_4_10b <- lm(lsalary ~ bensal + lenrol + lstaff, data = meap93)
summary(model_4_10b)
model_4_10c <- lm(lsalary ~ bensal, data = meap93)
summary(model_4_10c)

# Save individual regression results
stargazer(model_4_1, type = "text", out = "output/Chapter4_Example4_1_RegressionResults.txt", digits = 3)
stargazer(model_4_2, type = "text", out = "output/Chapter4_Example4_2_RegressionResults.txt", digits = 3)
stargazer(model_4_2_log, type = "text", out = "output/Chapter4_Example4_2_Log_RegressionResults.txt", digits = 3)
stargazer(model_4_3, type = "text", out = "output/Chapter4_Example4_3_RegressionResults.txt", digits = 3)
stargazer(model_4_4, type = "text", out = "output/Chapter4_Example4_4_RegressionResults.txt", digits = 3)
stargazer(model_4_5, type = "text", out = "output/Chapter4_Example4_5_RegressionResults.txt", digits = 3)
stargazer(model_4_6, type = "text", out = "output/Chapter4_Example4_6_RegressionResults.txt", digits = 3)
stargazer(model_4_7, type = "text", out = "output/Chapter4_Example4_7_RegressionResults.txt", digits = 3)
stargazer(model_4_9, type = "text", out = "output/Chapter4_Example4_9_RegressionResults.txt", digits = 3)
stargazer(model_4_10, type = "text", out = "output/Chapter4_Example4_10_RegressionResults.txt", digits = 3)
stargazer(model_4_10b, type = "text", out = "output/Chapter4_Example4_10b_RegressionResults.txt", digits = 3)
stargazer(model_4_10c, type = "text", out = "output/Chapter4_Example4_10c_RegressionResults.txt", digits = 3)

# Save regression results as CSV
summary_log_model_4_10 <- summary(model_4_10)
coef_log_model_4_10 <- as.data.frame(summary_log_model_4_10$coefficients)
coef_log_model_4_10 <- round(coef_log_model_4_10, 3) # Round to 3 decimal places
write.csv(coef_log_model_4_10, file = "output/Chapter4_Example4_10_RegressionResults.csv")

# Save regression results as Word document
doc <- read_docx()
reg_table <- flextable(coef_log_model_4_10)
doc <- body_add_flextable(doc, value = reg_table)
print(doc, target = "output/Chapter4_Example4_10_RegressionResults.docx")

# Save regression results as LaTeX
stargazer(model_4_10, type = "latex", out = "output/Chapter4_Example4_10_RegressionResults.tex", digits = 3)

# Explanation:
# This script performs multiple regression analyses on various datasets
# from Wooldridge's "Introductory Econometrics". The analyses include:
# - Example 4.1: Hourly Wage Equation
# - Example 4.2: Student Performance and School Size
# - Example 4.3: Determinants of College GPA
# - Example 4.4: Campus Crime and Enrollment
# - Example 4.5: Housing Prices and Air Pollution
# - Example 4.6: Participation Rates in 401K Plans
# - Example 4.7: Effect of Job Training Grants on Firm Scrap Rates
# - Example 4.9: Parents Education in a Birth Weight Equation
# - Example 4.10: Salary-Pension Tradeoff for Teachers
