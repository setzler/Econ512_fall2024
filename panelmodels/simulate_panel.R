
rm(list=ls())
library(data.table)
library(ggplot2)
setwd("~/github/Econ512_fall2024/panelmodels/")

# one input production function
# revenue = exp(TFP)*f(labor), where f(labor) = labor^alpha
# labor is selected on TFP, labor = gamma*exp(TFP)
# let TFP be MA(1) process, TFP_t = delta*eta_{t-1} + eta_t
# we can also add a measurement error that is not endogenous

# simulate
set.seed(123)
N = 1000
T = 10
delta = 10
gamma = 1
alpha = 0.8

# create a data set
production_data = data.table(expand.grid(firm_ID = 1:N, year = 1:T))
production_data = production_data[, eta_current := rnorm(N*T, mean = 0, sd = 1)]
eta_data = production_data[, list(firm_ID, year = year + 1, eta_lag = eta_current)]
production_data = merge(production_data, eta_data, by = c("firm_ID", "year"), all.x = TRUE)
production_data[year == 1, eta_lag := rnorm(N, mean = 0, sd = 1)]
production_data[, TFP := eta_current + delta*eta_lag]

# create the labor and revenue variable
production_data[, labor := gamma*exp(TFP) + exp(rnorm(N*T, mean = 0, sd = 10))]
production_data[, measurement_error := rnorm(N*T, mean = 0, sd = 1)]
production_data[, revenue := exp(TFP) * labor^alpha ] #* exp(measurement_error)]
production_data[, logrevenue := log(revenue)]
production_data[, loglabor := log(labor)]

# form the first and second lag of loglabor
labor_lags = production_data[, list(firm_ID, year = year + 1, loglabor_lag1 = loglabor)]
production_data = merge(production_data, labor_lags, by = c("firm_ID", "year"), all.x = TRUE)
labor_lags = production_data[, list(firm_ID, year = year + 2, loglabor_lag2 = loglabor)]
production_data = merge(production_data, labor_lags, by = c("firm_ID", "year"), all.x = TRUE)

# try to estimate alpha by OLS
ols_results = lm(logrevenue ~ loglabor, data = production_data)

# construct predicted TFP for a given guess of alpha
moment_fit = function( alpha_guess, dataset){
    dataset[, TFP_hat := (logrevenue - alpha_guess*loglabor)]
    moment = dataset[!is.na(loglabor_lag2), mean(TFP_hat * loglabor_lag2)]
    return( moment^2 )
}

# check some alpha guess
print( moment_fit(0.4, production_data) )

# plot the moment_fit across alpha_guess from -1 to 2
alpha_guesses = seq(-1, 2, by = 0.01)
moment_values = sapply(alpha_guesses, function(x) moment_fit(x, production_data))
# use ggplot
gg = ggplot(data = data.table(alpha_guesses, moment_values), aes(x=alpha_guesses, y=moment_values)) + 
    geom_point() + 
    geom_line() + 
    theme_bw(base_size = 16) + 
    labs(x="Alpha Guess", y="Moment Value") +
    geom_vline(xintercept = alpha, linetype="dashed", color = "red") 
ggsave(gg, file="moment_fit.pdf", width=8, height=5)

# use optim to find the alpha that minimizes the moment
optim_result = optim(0.4, moment_fit, dataset = production_data)
print(optim_result)


