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

# Minimum amount of sessions required in order to get the
minUserSessions <- 10

# Data preperation --------------------------------------------------------

#Create dataframe with user classifications (profiles) 
userProfiles <- userClassificationDf(sessionClassificationDf(cleanDf(df)))

# Gets the sessions for the specified user_profile
userSessions <- subset(userProfiles, userProfiles$class == 3)

if(nrow(userSessions) >= minUserSessions){
  
  # Gets the day of the week from the date
  df$dayOfWeek <- weekdays(as.Date(df$start_date))
  
  df$hour <- as.numeric(format(round(df$start_date, "hours"), format = "%H"))
  
  df <- df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed > 0 & hours_elapsed <= 24,
      !is.na(start_date),
      !is.na(end_date),
      !is.na(car),
      !is.na(charged_kwh),
      user_id %in% userSessions$user_id #filter on users with certain classification
    ) 
  
}else{
  print("User does not have enough sessions to make a prediction")
  
}

# Create linear model ----------------------------------------------------

# build linear model to predict end_date with the following parameters

# Create train and test data from the data
set.seed(100)
trainingRowIndex <- sample(1:nrow(df), 0.7 * nrow(df))
trainingData <-
  df[trainingRowIndex, ]  # 70% training data
testData <- df[-trainingRowIndex, ]   # remaining test data

# Create linear model
lm_df <<-
  lm(hours_elapsed ~ charged_kwh,
     data = trainingData)

# Testing linear model ----------------------------------------------------

modelSummary <- summary(lm_df)
modelCoeffs <- modelSummary$coefficients
rSquared <- modelSummary$r.squared
adjRSquared <- modelSummary$adj.r.squared


ChargingSessionPredict <<- predict(lm_df, testData)

actual_predicts <- data.frame(cbind(actual = testData$hours_elapsed,
                                    predicted = ChargingSessionPredict))

actual_predicts <- actual_predicts %>%
  filter(!is.na(actual_predicts$predicted))

res_test <- testData$hours_elapsed - ChargingSessionPredict

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

