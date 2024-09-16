
#######################################################################
## This is a script for estimating the regression discontinuity model
#######################################################################

rm(list=ls()) # clear the memory
setwd("~/github/Econ512_fall2024/RDD/")

# load the data
the_data_set = read.csv("RDD_simulator_data.csv")
the_data_set = setDT(the_data_set)

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

# plot the results, with a horizontal line at 3.0 
gg = ggplot(data = data.table( thresholds, results ), aes(x=thresholds, y=results)) + 
    geom_point() + 
    geom_line() + 
    theme_bw(base_size = 16) + 
    labs(x="Bandwidth (tau)", y="Treatment Effect Estimate") +
    geom_hline(yintercept = 3.0, linetype="dashed", color = "red") +
    ylim(0, 6)

ggsave(gg, file="RDD_estimator_simple.pdf", width=8, height=5)
