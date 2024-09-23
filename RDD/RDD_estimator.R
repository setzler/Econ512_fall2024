
#######################################################################
## This is a script for estimating the regression discontinuity model
#######################################################################

rm(list=ls()) # clear the memory
library(data.table)
library(ggplot2)
setwd("~/github/Econ512_fall2024/RDD/")

# load the data
the_data_set = read.csv("RDD_simulator_data.csv")
the_data_set = setDT(the_data_set)

######################################
## RDD estimator with simple threshold
######################################

# RDD estimator with simple threshold around cutoff
RDD_estimator_simple <- function( inputdata, c_threshold_point = 0, tau_bandwidth = 0.5 ){
    # find those observations just above the threshold
    meanY_above = inputdata[ X_running_variable >= c_threshold_point & X_running_variable <= c_threshold_point + tau_bandwidth, mean( Y_outcome_of_interest ) ]

    # find those observations just below the threshold
    meanY_below = inputdata[ X_running_variable <= c_threshold_point & X_running_variable >= c_threshold_point - tau_bandwidth, mean( Y_outcome_of_interest ) ]

    # estimate the treatment effect
    RDD_beta_hat = meanY_above - meanY_below
    return( RDD_beta_hat )
}

# loop across threshold choices and see which threshold gives the best estimate
thresholds = seq(0.1, 1.0, by = 0.1)

results = rep(0, length(thresholds) )
for( i in 1:length(thresholds) ){
    results[i] = RDD_estimator_simple( the_data_set, c_threshold_point = 0, tau_bandwidth = thresholds[i] )
}


results2 = sapply( thresholds, function(x) RDD_estimator_simple( the_data_set, c_threshold_point = 0, tau_bandwidth = x ) )

# plot the results, with a horizontal line at 3.0 
gg = ggplot(data = data.table( thresholds, results ), aes(x=thresholds, y=results)) + 
    geom_point() + 
    geom_line() + 
    theme_bw(base_size = 16) + 
    labs(x="Bandwidth (tau)", y="Treatment Effect Estimate") +
    geom_hline(yintercept = 3.0, linetype="dashed", color = "red") +
    ylim(0, 6)

ggsave(gg, file="RDD_estimator_simple.pdf", width=8, height=5)





######################################
## RDD estimator with triangular kernel
######################################

# RDD estimator with triangular kernel
RDD_estimator_triangular <- function( inputdata, c_threshold_point = 0, tau_bandwidth = 0.5 ){

    inputdata[, normalized_distance := (X_running_variable - c_threshold_point) / tau_bandwidth ]
    inputdata[, kernel_weight := (1 - abs(normalized_distance))*(abs(normalized_distance) <= 1) ]

    triangular_loss_function <- function(params, inputdata_subset){

        # unpack the parameter vector into (alpha, gamma) parts
        alpha_guess = params[1] 
        gamma_guess = params[2] 

        # calculate the objective function value
        inputdata_subset[, objective_part := (Y_outcome_of_interest - alpha_guess - gamma_guess * (X_running_variable - c_threshold_point))^2 ]
        objective_value = inputdata_subset[, sum( objective_part * kernel_weight ) ]

        return(objective_value)

    }

    # minimize the triangular loss function, separately for treated and control groups
    optim_result_treated = optim( c(0, 0), triangular_loss_function, inputdata_subset = inputdata[D_treatment == TRUE] )

    optim_result_control = optim( c(0, 0), triangular_loss_function, inputdata_subset = inputdata[D_treatment == FALSE] )

    # extract the treatment effect from the optimized parameters
    alpha_treated = optim_result_treated$par[1]
    alpha_control = optim_result_control$par[1]

    RDD_triangular = alpha_treated - alpha_control

    return( RDD_triangular )

}

results_triangular = rep(0, length(thresholds) )
for( i in 1:length(thresholds) ){
    results_triangular[i] = RDD_estimator_triangular( the_data_set, c_threshold_point = 0, tau_bandwidth = thresholds[i] )
}


# plot the results, with a horizontal line at 3.0 
gg = ggplot(data = data.table( thresholds, results_triangular ), aes(x=thresholds, y=results_triangular)) + 
    geom_point() + 
    geom_line() + 
    theme_bw(base_size = 16) + 
    labs(x="Bandwidth (tau)", y="Treatment Effect Estimate: Triangular") +
    geom_hline(yintercept = 3.0, linetype="dashed", color = "red") +
    ylim(0, 6)

ggsave(gg, file="RDD_estimator_triangular.pdf", width=8, height=5)


