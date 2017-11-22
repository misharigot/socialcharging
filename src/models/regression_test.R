# Linear regression model for predicting end_date
library(ggplot2)
library(config)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)
library(corrplot)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read.csv(config$dataFolder)

# Testing linear model ----------------------------------------------------

#Test the regression based on user classification
testUserClassBasedPrediction <- function() {
  
  # Create dataframe with differences between actual times and predictions
  actual_predicts <- data.frame("difference" = c(df$hours_elapsed - df$user_pred))
  
  # Predictions are off by this many hours on average
  rmse_test <- sqrt(mean((actual_predicts$difference) ^ 2))

  # Calculates minMax accuracy
  minMaxAccuracy <-
    mean(min(actual_predicts$difference) 
         / max(actual_predicts$difference))
  
  
  # How many predictions are in the error range of 4 hours
  acceptableTimeRange <- 4
  
  resultsWithInRange <- actual_predicts %>%
    filter(difference <= acceptableTimeRange / 2 &
             difference >= -(acceptableTimeRange / 2))
  
  # Percentage of predictions within error range
  actualAccuracy <-
    100 / nrow(actual_predicts) * nrow(resultsWithInRange)
  
  resultListUser <- list("rmse_test" = rmse_test, "minMaxAccuracy" = minMaxAccuracy, 
                     "actualAccuracy" = actualAccuracy)
}

#Test the regression based on station classification
testStationClassBasedPrediction <- function() {
  
  # Create dataframe with differences between actual times and predictions
  actual_predicts <- data.frame("difference" = c(df$hours_elapsed - df$station_pred))
  
  # Predictions are off by this many hours on average
  rmse_test <- sqrt(mean((actual_predicts$difference) ^ 2))
  
  # Calculates minMax accuracy
  minMaxAccuracy <-
    mean(min(actual_predicts$difference) 
         / max(actual_predicts$difference))
  
  
  # How many predictions are in the error range of 4 hours
  acceptableTimeRange <- 4
  
  resultsWithInRange <- actual_predicts %>%
    filter(difference <= acceptableTimeRange / 2 &
             difference >= -(acceptableTimeRange / 2))
  
  # Percentage of predictions within error range
  actualAccuracy <-
    100 / nrow(actual_predicts) * nrow(resultsWithInRange)
  
  resultListStation <- list("rmse_test" = rmse_test, "minMaxAccuracy" = minMaxAccuracy, 
                     "actualAccuracy" = actualAccuracy)
}

# Function calls ----------------------------------------------------

testUserClassBasedPrediction()
testStationClassBasedPrediction()

