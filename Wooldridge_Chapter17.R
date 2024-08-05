# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 17

# Chapter 17: Limited Dependent Variable Models

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
if (!require("mfx")) install.packages("mfx", dependencies = TRUE)
if (!require("survival")) install.packages("survival", dependencies = TRUE)
if (!require("censReg")) install.packages("censReg", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("sampleSelection")) install.packages("sampleSelection", dependencies = TRUE)

# Load necessary libraries
library(wooldridge)
library(car)
library(lmtest)
library(mfx)
library(survival)
library(censReg)
library(stargazer)
library(sampleSelection)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter17_log.txt", split = TRUE)

# Example 17-1-1: Linear Probability Model with Robust SE

data(mroz, package='wooldridge')

# Estimate linear probability model
cat("Estimating linear probability model\n")
linprob <- lm(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz)

# Regression table with heteroscedasticity-robust SE and t tests:
cat("Regression table with robust standard errors\n")
coeftest(linprob, vcov = hccm)

# Example 17-1-2: Predictions for Extreme Cases

# Predictions for two "extreme" women
cat("Predictions for two 'extreme' women\n")
xpred <- data.frame(nwifeinc = c(100, 0), educ = c(5, 17), exper = c(0, 30), age = c(20, 52), kidslt6 = c(2, 0), kidsge6 = c(0, 0))
predict(linprob, xpred)

# Example 17-1-3: Logit Model

# Estimate logit model
cat("Estimating logit model\n")
logitres <- glm(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, family = binomial(link = "logit"), data = mroz)

# Summary of results:
cat("Summary of logit model\n")
summary(logitres)

# Log likelihood value:
cat("Log likelihood value\n")
logLik(logitres)

# McFadden's pseudo R2:
cat("McFadden's pseudo R2\n")
1 - logitres$deviance / logitres$null.deviance

# Example 17-1-4: Probit Model

# Estimate probit model
cat("Estimating probit model\n")
probitres <- glm(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, family = binomial(link = "probit"), data = mroz)

# Summary of results:
cat("Summary of probit model\n")
summary(probitres)

# Log likelihood value:
cat("Log likelihood value\n")
logLik(probitres)

# McFadden's pseudo R2:
cat("McFadden's pseudo R2\n")
1 - probitres$deviance / probitres$null.deviance

# Example 17-1-5: Likelihood Ratio Test

# Test of overall significance using LR test statistic
cat("Test of overall significance: Likelihood Ratio Test\n")
probitres$null.deviance - probitres$deviance

# Automatic calculations including p-values
cat("Automatic LR test calculations\n")
lrtest(probitres)

# Test of H0: experience and age are irrelevant
cat("Test of H0: experience and age are irrelevant\n")
restr <- glm(inlf ~ nwifeinc + educ + kidslt6 + kidsge6, family = binomial(link = "logit"), data = mroz)
lrtest(restr, probitres)

# Example 17-1-6: Model Predictions

# Predictions from linear probability, logit, and probit models
cat("Predictions from linear probability, logit, and probit models\n")
predict(linprob, xpred, type = "response")
predict(logitres, xpred, type = "response")
predict(probitres, xpred, type = "response")

# Example 17-1-7: Average Partial Effects (APE)

# Calculation of linear index at individual values
cat("Calculation of linear index at individual values\n")
xb.log <- predict(logitres)
xb.prob <- predict(probitres)

# APE factors = average(g(xb))
cat("Calculating APE factors\n")
factor.log <- mean(dlogis(xb.log))
factor.prob <- mean(dnorm(xb.prob))
cbind(factor.log, factor.prob)

# Average partial effects = beta * factor
cat("Calculating average partial effects\n")
APE.lin <- coef(linprob) * 1
APE.log <- coef(logitres) * factor.log
APE.prob <- coef(probitres) * factor.prob

# Table of APEs
cat("Table of APEs\n")
cbind(APE.lin, APE.log, APE.prob)

# Example 17-1-8: Automatic APE Calculations

# Automatic APE calculations with package mfx
cat("Automatic APE calculations\n")
logitmfx(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz, atmean = FALSE)

# Example 17-2-survreg: Tobit Model using survreg

# Estimate Tobit model using survreg
cat("Estimating Tobit model using survreg\n")
res <- survreg(Surv(hours, hours > 0, type = "left") ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz, dist = "gaussian")
summary(res)

# Example 17-2: Tobit Model using censReg

# Estimate Tobit model using censReg
cat("Estimating Tobit model using censReg\n")
TobitRes <- censReg(hours ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz)
summary(TobitRes)

# Partial Effects at the average x
cat("Calculating partial effects at the average x\n")
margEff(TobitRes)

# Example 17-3-1: Poisson Models

data(crime1, package='wooldridge')

# Estimate linear model
cat("Estimating linear model\n")
lm.res <- lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 + black + hispan + born60, data = crime1)

# Estimate Poisson model
cat("Estimating Poisson model\n")
Poisson.res <- glm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 + black + hispan + born60, data = crime1, family = poisson)

# Quasi-Poisson model
cat("Estimating Quasi-Poisson model\n")
QPoisson.res <- glm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 + black + hispan + born60, data = crime1, family = quasipoisson)

# Example 17-3-2: Regression Table

# Regression table (run Example-17-3-1.R first!)
cat("Creating regression table\n")
stargazer(lm.res, Poisson.res, QPoisson.res, type = "text", keep.stat = "n")

# Example 17-4: Censored Regression Model

library(survival)
data(recid, package='wooldridge')

# Define Dummy for uncensored observations
cat("Defining dummy for uncensored observations\n")
recid$uncensored <- recid$cens == 0

# Estimate censored regression model
cat("Estimating censored regression model\n")
res <- survreg(Surv(log(durat), uncensored, type = "right") ~ workprg + priors + tserved + felon + alcohol + drugs + black + married + educ + age, data = recid, dist = "gaussian")
summary(res)

# Example 17-5: Heckman Selection Model

# Estimate Heckman selection model (2 step version)
cat("Estimating Heckman selection model (2-step version)\n")
res <- selection(inlf ~ educ + exper + I(exper^2) + nwifeinc + age + kidslt6 + kidsge6,
                 log(wage) ~ educ + exper + I(exper^2), 
                 data = mroz, method = "2step")

# Summary of results
cat("Summary of Heckman selection model results\n")
summary(res)

# Credit: Code adapted from Florian Heiss
