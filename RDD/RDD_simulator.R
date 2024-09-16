
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


