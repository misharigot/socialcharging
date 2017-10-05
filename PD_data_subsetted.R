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

data <- read_csv(pathToCSV,header=TRUE, sep=";", row.names=NULL)

# filter data based on 
PD_filtered_data <-  data[!(!is.na(data$ChargeTimeHours) & data$ChargeTimeHours==""), ]

