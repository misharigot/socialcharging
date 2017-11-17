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

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Minimum amount of sessions required in order to get the
minUserSessions <- 10

# Data preperation --------------------------------------------------------

cleanDf <- function(df) {
  # Gets the user that met the minimum required sessions
  usersWithEnoughSessions <- df %>%
    group_by(user_id) %>%
    summarise(count = n()) %>%
    filter(count >= minUserSessions) %>%
    select(user_id, count)
  
  # Gets the day of the week from the date
  df$dayOfWeek <- weekdays(as.Date(df$start_date))
  
  df$hour <- as.numeric(format(round(df$start_date, "hours"), format = "%H"))
  
  df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed >= 0 & hours_elapsed <= 24,
      !is.na(start_date),
      !is.na(end_date),
      !is.na(car),
      !is.na(charged_kwh),
      user_id %in% usersWithEnoughSessions$user_id
    ) 
}

classifyTf <- function(datetime) {
  datetime <- hour(datetime)
  if (datetime >= 0 & datetime < 3) {
    return(0)
  } else if (datetime >= 3 & datetime < 6) {
    return(1)
  } else if (datetime >= 6 & datetime < 9) {
    return(2)
  } else if (datetime >= 9 & datetime < 12) {
    return(3)
  } else if (datetime >= 12 & datetime < 15) {
    return(4)
  } else if (datetime >= 15 & datetime < 18) {
    return(5)
  } else if (datetime >= 18 & datetime < 21) {
    return(6)
  } else if (datetime >= 21 & datetime < 24) {
    return(7)
  }
}

# Classify session on timeframes
sessionClassificationDf <- function(cleanDf) {
  sessionClassifications <- cleanDf %>%
    mutate(kw_charge_point_speed = gsub(00, "", kw_charge_point_speed)) %>%
    mutate(
      start_tf = map(start_date, classifyTf),
      end_tf = map(end_date, classifyTf)
    ) %>%
    mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf)))
  return(sessionClassifications)
}

# Testing linear model ----------------------------------------------------

# build linear model to predict end_date with the following parameters
createLinearModelData <- function(data) {
  
  df <- data
  # Create train and test data from the data
  set.seed(100)
  trainingRowIndex <- sample(1:nrow(df), 0.7 * nrow(df))
  trainingData <-
    df[trainingRowIndex, ]  # 70% training data
  testData <- df[-trainingRowIndex, ]   # remaining test data
  
  # Create linear model
  lm_df <<-
    lm(hours_elapsed ~ hour + charged_kwh + car + start_tf + smart_charging + dayOfWeek,
       data = trainingData)
  
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
  
  # Estimates are off by this many hours on average
  rmse_test <- sqrt(mean(res_test ^ 2))
  
  # oneliner version of rmse calculation above
  # sqrt(mean((testData$hours_elapsed - ChargingSessionPredict) ^2))
  
  # Estimates are off by this many hours on average
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
  plotBox(ChargingSessionPredict, testData)
}

createProfilingPrediction <- function(data) {
  
  df <- data
  # Create train and test data from the data
  set.seed(100)
  trainingRowIndex <- sample(1:nrow(df), 0.7 * nrow(df))
  trainingData <-
    df[trainingRowIndex, ]  # 70% training data
  testData <- df[-trainingRowIndex, ]   # remaining test data
  
  # Create linear model
  lm_df <<-
    lm(hours_elapsed ~ hour + charged_kwh + car + start_tf + smart_charging + dayOfWeek,
       data = trainingData)
  
  modelSummary <- summary(lm_df)
  modelCoeffs <- modelSummary$coefficients
  rSquared <- modelSummary$r.squared
  adjRSquared <- modelSummary$adj.r.squared
  
  ChargingSessionPredict <<- predict(lm_df, testData)
  
  actual_predicts <- data.frame(cbind(actual = testData$hours_elapsed,
                                      predicted = ChargingSessionPredict))
  
  actual_predicts <- actual_predicts %>%
    filter(!is.na(actual_predicts$predicted))
  
  testData <- testData %>%
    mutate(predicted = ChargingSessionPredict) %>%
    arrange(user_id)
  
  return(testData)
}


plotQq <- function(lm) {
  
  qqnorm(lm$residuals, ylab = "Residual Quantiles")
}

plotBox <- function(prediction, data) {
  
  ranks <- order(data$start_tf)
  
  plot(data$start_tf,
       data$hours_elapsed,
       xlab = "Start timeframe",
       ylab = "Hours elapsed")
  
  points(data$start_tf[ranks],
         prediction[ranks],
         col = "green")
}

plotFitted <- function(lm) {
  plot(lm$fitted.values,
       lm$residuals,
       xlab = "Fitted values",
       ylab = "Residuals")
}


# Correlation plot --------------------------------------------------------

createCorrelationPlot <- function(data) {
  
  df <- data
  numberfySmart <- function(smart) {
    if (smart == "Yes") {
      return(1)
    } else if (smart == "No") {
      return(0)
    }
  }
  
  temp <- as.data.frame(unique(df$car))
  colnames(temp) <- c("car")
  temp$carNumber <- NA
  nrow(temp)
  for (i in 1:nrow(temp)) {
    temp[i, 2] <- i
  }
  
  df <- base::merge(df, temp)
  
  df_cor_test <- df %>%
    select(hours_elapsed, hour, start_tf, charged_kwh, carNumber) %>%
    mutate(day_number = wday(as.Date(df$start_date))) %>%
    mutate(smart_charger = map(df$smart_charging, numberfySmart)) %>%
    mutate(smart_charger = as.numeric(unlist(smart_charger)))
  
  df_cor_test$hours_elapsed <- as.numeric(df_cor_test$hours_elapsed)
  df_cor_test$hour <- as.numeric(df_cor_test$hour)
  df_cor_test$start_tf <- as.numeric(df_cor_test$start_tf)
  
  colnames(df_cor_test) <- c("Hours Elapsed", "Time in H", "Timeframe",
                             "Charged KwH", "Car",
                             "Day of week", "Smart")
  str(df_cor_test)
  corrplot.mixed(cor(df_cor_test))
}

returnDf <- createProfilingPrediction(sessionClassificationDf(cleanDf(df)))

# Function calls ----------------------------------------------------------

createProfileRegression <- function(scData) {
  print("loading data")
  returnDf <- createProfilingPrediction(sessionClassificationDf(cleanDf(scData)))
  return(returnDf)
}

getSessions <- function(profileRegression, user) {
  print(user)
  if (!is.null(user)) {
    profileRegression <- profileRegression %>%
      filter(user_id == user)
    return(profileRegression)
  }
}

plotLinearModelsResult <- function(scData) {
  createLinearModelData(sessionClassificationDf(cleanDf(scData)))
}

plotCorrelationResult <- function(scData) {
  createCorrelationPlot(sessionClassificationDf(cleanDf(scData)))
}

plotQqResult <- function() {
  plotQq(lm_df)
}

plotFittedResult <- function() {
  plotFitted(lm_df)
}