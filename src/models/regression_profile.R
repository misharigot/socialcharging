# Linear regression model for predicting session end time
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
source("./src/models/user_class.R")

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Data preperation --------------------------------------------------------

prepareDataForLM <- function(df, userClass){
  
  # Create dataframe with user classifications 
  userClassifications <- userClassificationDf(sessionClassificationDf(cleanDf(df)))
  
  # Add user classifications to df
  df$user_class <- userClassifications$class[match(df$user_id, userClassifications$user_id)]
  
  # Makes subset of the sessions for the specified user classification
  if (userClass != 0) {
    specificUserSessions <- subset(userClassifications, userClassifications$class == userClass)
    # filter on users with chosen classification
    df <- df %>%
      filter(user_id %in% specificUserSessions$user_id )
  }
  
  # Filter corrupted data
  df <- df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed > 0 & hours_elapsed <= 24,
      !is.na(start_date),
      !is.na(end_date),
      !is.na(car),
      !is.na(charged_kwh),
      !is.na(user_class)
    )
  
  # Gets the day of the week from the date
  df$dayOfWeek <- weekdays(as.Date(df$start_date))
  # Gets the starting hour of session
  df$hour <- as.numeric(format(round(df$start_date, "hours"), format = "%H"))
  return(df)
}

# Create linear model ----------------------------------------------------

createLinearModelData <- function(df, isTest){
  minimumSessions <- 10
  # if the prediction has to be made for all classifications individually
  i <- 1
  userClassificationsUnique <- unique(as.character(cleanDf$user_class))
  print(userClassificationsUnique)
  
  idsWithPreds <- data.frame("session_id" = numeric(0), "user_pred" = numeric(0))
  
  # Predict session time for each profile
  while (i <= length(userClassificationsUnique)) {
    # Create a df for current user classification 
    sessionsWithSpecificClass <- df %>%
      filter(user_class == userClassificationsUnique[i])
    
    if (nrow(sessionsWithSpecificClass) > minimumSessions) {
      # Create linear model
      lm_df <- lm(hours_elapsed ~ charged_kwh, data = sessionsWithSpecificClass)
      
      # Add predictions to sessions
      sessionsWithSpecificClass$user_pred <- predict(lm_df, sessionsWithSpecificClass)
      
      dfToMerge <- sessionsWithSpecificClass[, c("session_id", "user_pred")]
      
      # Append dfToMerge to idsWithPreds
      idsWithPreds <- rbind(idsWithPreds, dfToMerge)
    }
    i <- i + 1
  }
  return(idsWithPreds)
}

# Testing linear model ----------------------------------------------------
wrapped <- function() {
  modelSummary <- summary(lm_df)
  modelCoeffs <- modelSummary$coefficients
  rSquared <- modelSummary$r.squared
  adjRSquared <- modelSummary$adj.r.squared
  
  actual_predicts <- data.frame(cbind(actual = testData$hours_elapsed,
                                      predicted = chargingSessionPredict))
  
  actual_predicts <- actual_predicts %>%
    filter(!is.na(actual_predicts$predicted))
  
  res_test <- testData$hours_elapsed - chargingSessionPredict
  
  # TEST Estimates are off by this many hours on average
  rmse_test <- sqrt(mean(res_test ^ 2))
  
  # TRAIN Estimates are off by this many hours on average
  rmse_train <- sqrt(mean(lm_df$residuals ^ 2))
  
  # Ratio of test RMSE over training RMSE
  rmse_ratio <- rmse_test / rmse_train
  
  minMaxAccuracy <-
    mean(min(actual_predicts$actual, actual_predicts$predicted) 
         / max(actual_predicts$actual, actual_predicts$predicted))
  
  actual_predicts$difference <- NULL
  
  actual_predicts$difference <-
    actual_predicts$actual - actual_predicts$predicted
  
  acceptableTimeRange <- 4
  
  resultsWithInRange <- actual_predicts %>%
    filter(difference <= acceptableTimeRange / 2 &
             difference >= -(acceptableTimeRange / 2))
  
  actualAccuracy <-
    100 / nrow(actual_predicts) * nrow(resultsWithInRange)
  
  resultList <- list("rmse_test" = rmse_test, "rmse_train" = rmse_train, 
                     "minMaxAccuracy" = minMaxAccuracy, "rSquared" = rSquared,
                     "adjRSquared" = adjRSquared, "rmse_ratio" = rmse_ratio,
                     "actualAccuracy" = actualAccuracy)
}