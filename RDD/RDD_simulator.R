
#######################################################################
## This is a script for simulating the regression discontinuity model
#######################################################################

# given parameters
cutoff_parameter = 0.0
beta_treatment_effect = 10.0
N_sample_size = 1000

# simulate the heterogeneous primitives
X_running_variable = rnorm( N_sample_size, mean = 0, sd = 1 )
epsilon_outcome_unobservable = X_running_variable + rnorm( N_sample_size, mean = 0, sd = 1 )

# assign the treated group
D_treatment = X_running_variable >= cutoff_parameter
Y_outcome_of_interest = beta_treatment_effect * D_treatment + epsilon_outcome_unobservable

# combine the variables into a data table
library(data.table) # install.packages("data.table")
the_data_set = data.table( Y_outcome_of_interest, D_treatment, X_running_variable )

# plot the data, X_running_variable on the x-axis and Y_outcome_of_interest on the y-axis, I want to use ggplot2 for this
library(ggplot2) # install.packages("ggplot2")
gg = ggplot(aes(x=X_running_variable, y=Y_outcome_of_interest), data=the_data_set) + 
    geom_point() + 
    theme_bw(base_size = 16) + 
    labs(x="Running Variable (X)", y="Outcome of Interest (Y)") + 
    geom_vline(xintercept = cutoff_parameter)

# save the plot
setwd("~/github/Econ512_fall2024/RDD/")
ggsave(gg, file="RDD_simulator.pdf", width=8, height=5)
