
#######################################################################
## This is a script for estimating the regression discontinuity model
#######################################################################

rm(list=ls()) # clear the memory
setwd("~/github/Econ512_fall2024/RDD/")

# load the data
the_data_set = read.csv("RDD_simulator_data.csv")
the_data_set = setDT(the_data_set)


