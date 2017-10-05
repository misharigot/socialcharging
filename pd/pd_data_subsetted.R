library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(config)
config <- config::get()

# filter data based on
data <- read_csv2(config$pdDataset)

PD_filtered_data <- data[!(is.na(data$ChargeTimeHours) | data$ChargeTimeHours==""), ]
