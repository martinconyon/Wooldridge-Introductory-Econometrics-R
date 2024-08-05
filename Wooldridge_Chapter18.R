# Author: Martin Conyon
# Date: August 2024
# Data set: Wooldridge - Chapter 18

# Chapter 18: Advanced Topics in Time Series Econometrics

# Instructions:
# 1. Set your working directory to the folder where you have saved this script.
#    You can do this by using the setwd() function. For example:
#    setwd("path/to/your/folder")

# 2. Run the script section by section to see the output at each step.

# Load necessary packages
if (!require("dynlm")) install.packages("dynlm", dependencies = TRUE)
if (!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if (!require("urca")) install.packages("urca", dependencies = TRUE)
if (!require("tseries")) install.packages("tseries", dependencies = TRUE)
if (!require("wooldridge")) install.packages("wooldridge", dependencies = TRUE)

# Load necessary libraries
library(dynlm)
library(stargazer)
library(urca)
library(tseries)
library(wooldridge)

# Clear the environment
rm(list = ls())

# Create the output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Open log file
sink(file = "output/chapter18_log.txt", split = TRUE)

# Example-18-1.R: Geometric and Rational Distributed Lag Models

# Load the dataset
data(hseinv, package='wooldridge')

# Detrend the variable: residual from a regression on the observation index
cat("Detrending the variable: residual from a regression on the observation index\n")
trendreg <- dynlm(log(invpc) ~ trend(hseinv), data = hseinv)
hseinv$linv.detr <- resid(trendreg)

# Convert to time series data
cat("Converting to time series data\n")
hseinv.ts <- ts(hseinv)

# Geometric distributed lag model
cat("Estimating Koyck geometric distributed lag model\n")
gDL <- dynlm(linv.detr ~ gprice + L(linv.detr), data = hseinv.ts)

# Rational distributed lag model
cat("Estimating rational distributed lag model\n")
rDL <- dynlm(linv.detr ~ gprice + L(linv.detr) + L(gprice), data = hseinv.ts)

# Display results in a regression table
cat("Displaying results in a regression table\n")
stargazer(gDL, rDL, type = "text", keep.stat = c("n", "adj.rsq"))

# Long-run propensity (LRP) for geometric DL
cat("Calculating Long-run propensity (LRP) for geometric DL\n")
b <- coef(gDL)
gDL_LRP <- b["gprice"] / (1 - b["L(linv.detr)"])
cat("Geometric DL LRP:", gDL_LRP, "\n")

# Long-run propensity (LRP) for rational DL
cat("Calculating Long-run propensity (LRP) for rational DL\n")
b <- coef(rDL)
rDL_LRP <- (b["gprice"] + b["L(gprice)"]) / (1 - b["L(linv.detr)"])
cat("Rational DL LRP:", rDL_LRP, "\n")

# Example-18-4-urca.R: Augmented Dickey-Fuller Test

# Load the dataset
data(inven, package='wooldridge')

# Automated ADF test using urca package
cat("Automated ADF test using urca package\n")
adf_result <- summary(ur.df(log(inven$gdp), type = c("trend"), lags = 1))
print(adf_result)

# Example-18-4.R: Augmented Dickey-Fuller Test using dynlm

# Load the dataset
data(inven, package='wooldridge')

# Variable to test: y = log(gdp)
inven$y <- log(inven$gdp)
inven.ts <- ts(inven)

# Summary output of ADF regression
cat("Summary output of ADF regression using dynlm\n")
adf_reg <- summary(dynlm(d(y) ~ L(y) + L(d(y)) + trend(inven.ts), data = inven.ts))
print(adf_reg)

# Automated ADF test using tseries package
cat("Automated ADF test using tseries package\n")
adf_test <- adf.test(inven$y, k = 1)
print(adf_test)

# Example-18-8.R: Estimating Models and Displaying Results

# load updataed data from URfIE Website since online file is incomplete
library(dynlm); library(stargazer)
data(phillips, package='wooldridge')
tsdat=ts(phillips, start=1948)

# Estimate models and display results
res1 <- dynlm(unem ~ unem_1      , data=tsdat, end=1996)
res2 <- dynlm(unem ~ unem_1+inf_1, data=tsdat, end=1996)
stargazer(res1, res2 ,type="text", keep.stat=c("n","adj.rsq","ser"))

# Predictions for 1997-2003 including 95% forecast intervals:
predict(res1, newdata=window(tsdat,start=1997), interval="prediction")
predict(res2, newdata=window(tsdat,start=1997), interval="prediction")


# Example-18-9.R
# Note: run Example-18-8.R first to generate the results res1 and res2

# Actual unemployment and forecasts:
y  <- window(tsdat,start=1997)[,"unem"]
f1 <- predict( res1, newdata=window(tsdat,start=1997) )
f2 <- predict( res2, newdata=window(tsdat,start=1997) )

# Plot unemployment and forecasts:
matplot(time(y), cbind(y,f1,f2), type="l",  col="black",lwd=2,lty=1:3)
legend("topleft",c("Unempl.","Forecast 1","Forecast 2"),lwd=2,lty=1:3)

# Forecast errors:
e1<- y - f1
e2<- y - f2

# RMSE:
sqrt(mean(e1^2))
sqrt(mean(e2^2))

# MAE:
mean(abs(e1))
mean(abs(e2))

# Simulate-Spurious-Regression-1.R

# Initialize Random Number Generator
set.seed(29846)

# i.i.d. N(0,1) innovations
n <- 50
e <- rnorm(n)
a <- rnorm(n)
# independent random walks
x <- cumsum(a)
y <- cumsum(e)

# plot
plot(x,type="l",lty=1,lwd=1)
lines(y        ,lty=2,lwd=2)
legend("topright",c("x","y"), lty=c(1,2), lwd=c(1,2))

# Regression of y on x
summary( lm(y~x) )


# Simulate-Spurious-Regression-2.R

# Initialize Random Number Generator
set.seed(29846)

# generate 10,000 independent random walks
# and store the p val of the t test 
pvals <- numeric(10000)
for (r in 1:10000) {
  # i.i.d. N(0,1) innovations
  n <- 50
  a <- rnorm(n)
  e <- rnorm(n)
  # independent random walks
  x <- cumsum(a)
  y <- cumsum(e)
  # regression summary
  regsum <- summary(lm(y~x))
  # p value: 2nd row, 4th column of regression table
  pvals[r] <- regsum$coef[2,4]
}

# How often is p<5% ?
table(pvals<=0.05)


# Credit: Florian Heiss (code)
