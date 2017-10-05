# This code plots the time a car is connected to a pole (ConnectionTimeHours) vs kWh actually charged.
library(readr)
library(dplyr)
library(ggplot2)
library(config)
source("pd/pd_data_subsetted.R")

data <- PD_filtered_data

# plot charged_kwh against time_elapsed in hours
p <- ggplot(data, aes(y = kWh, x = ConnectionTimeHours)) + geom_count(alpha = 0.1)
p + labs(x = "time elapsed in hours", y = "kWh charged")
