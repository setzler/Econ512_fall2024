
rm(list=ls())
library(data.table)
library(ggplot2)

# one input production function
# revenue = exp(TFP)*f(labor), where f(labor) = labor^alpha
# labor is selected on TFP, labor = gamma*exp(TFP)
# let TFP be MA(1) process, TFP_t = delta*eta_{t-1} + eta_t
# we can also add a measurement error that is not endogenous

# simulate
set.seed(123)
N = 1000
T = 10
delta = 0.5
gamma = 0.3
alpha = 0.8

# create a data set
production_data = data.table(expand.grid(firm_ID = 1:N, year = 1:T))
production_data = production_data[, eta_current := rnorm(N*T, mean = 0, sd = 1)]
eta_data = production_data[, list(firm_ID, year = year + 1, eta_lag = eta_current)]
production_data = merge(production_data, eta_data, by = c("firm_ID", "year"), all.x = TRUE)
production_data[year == 1, eta_lag := rnorm(N, mean = 0, sd = 1)]
production_data[, TFP := eta_current + delta*eta_lag]

# create the labor and revenue variable
production_data[, labor := gamma*exp(TFP) + exp(rnorm(N*T, mean = 0, sd = 0.1))]
production_data[, revenue := exp(TFP) * labor^alpha * exp(rnorm(N*T, mean = 0, sd = 0.1))]
production_data[, logrevenue := log(revenue)]
production_data[, loglabor := log(labor)]

# try to estimate alpha by OLS
ols_results = lm(logrevenue ~ loglabor, data = production_data)
