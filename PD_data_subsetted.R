# install.packages("rstudioapi")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")

library(rstudioapi)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("parameters.R")

# filter data based on 
data <- read_csv2(pathToCSV)

PD_filtered_data <- data[!(is.na(data$ChargeTimeHours) | data$ChargeTimeHours==""), ]
