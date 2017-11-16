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
source(config$UserClass)

dataFrame <- read_csv2(config$scDataset, col_names = FALSE)

dataFrame <- cleanSecondDf(dataFrame)

# Minimum amount of sessions required in order to get the
minUserSessions <- 10

# Data preperation --------------------------------------------------------

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


userClassdataFrame <- getUserClassifications()
dataFrame <- base::merge(dataFrame, userClassdataFrame)

# Gets the user that met the minimum required sessions
usersWithEnoughSessions <- dataFrame %>%
  group_by(user_id) %>%
  summarise(count = n()) %>%
  filter(count >= minUserSessions) %>%
  select(user_id, count)

# Gets the day of the week from the date
dataFrame$dayOfWeek <- weekdays(as.Date(dataFrame$start_date))

dataFrame$hour <- as.numeric(format(round(dataFrame$start_date, "hours"), format='%H'))

dataFrame <- dataFrame %>%
  filter(
    !is.na(hours_elapsed),
    hours_elapsed >= 0 & hours_elapsed <= 24,
    !is.na(start_date),!is.na(end_date),!is.na(car),!is.na(charged_kwh),
    user_id %in% usersWithEnoughSessions$user_id
  ) %>%
  mutate(kw_charge_point_speed = gsub(00, "", kw_charge_point_speed)) %>%
  mutate(
    start_tf = map(start_date, classifyTf),
    end_tf = map(end_date, classifyTf)
  ) %>%
  mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf)))

# Create train and test data from the data --------------------------------

set.seed(100)
trainingRowIndex <- sample(1:nrow(dataFrame), 0.7 * nrow(dataFrame))
trainingData <-
  dataFrame[trainingRowIndex,]  # 70% training data
testData  <- dataFrame[-trainingRowIndex,]   # remaining test data

# Testing linear model ----------------------------------------------------

# build linear model to predict end_date with the following parameters
createLinearModelData <- function(){
  lm_df <-
    lm(hours_elapsed ~ hour + charged_kwh + car + start_tf + smart_charging + dayOfWeek + class,
       data = trainingData)
  
  modelSummary <- summary(lm_df)
  modelCoeffs <- modelSummary$coefficients
  rSquared <- modelSummary$r.squared
  adjRSquared <- modelSummary$adj.r.squared
  
  ranks <- order(testData$start_tf)
  
  ChargingSessionPredict <- predict(lm_df, testData)
  
  actual_predicts <- data.frame(cbind(actual = testData$hours_elapsed,
                                      predicted = ChargingSessionPredict))
  
  actual_predicts <- actual_predicts %>%
    filter(!is.na(actual_predicts$predicted))
  
  plot(testData$start_tf,
       testData$hours_elapsed,
       xlab = "Start timeframe",
       ylab = "Hours elapsed")
  
  points(testData$start_tf[ranks],
         ChargingSessionPredict[ranks],
         col = "green")
  
  plot(lm_df$fitted.values,
       lm_df$residuals,
       xlab = "Fitted values",
       ylab = "Residuals")
  
  qqnorm(lm_df$residuals, ylab = "Residual Quantiles")
  
  res_test <- testData$hours_elapsed - ChargingSessionPredict
  
  # Estimates are off by this many hours on average
  rmse_test <- sqrt(mean(res_test ^ 2))
  
  # oneliner version of rmse calculation above
  # sqrt(mean((testData$hours_elapsed - ChargingSessionPredict) ^2))
  
  # Estimates are off by this many hours on average
  rmse_train <- sqrt(mean(lm_df$residuals ^ 2))
  
  # Ratio of test RMSE over training RMSE
  rmse_test / rmse_train
  
  minMaxAccuracy <-
    mean(apply(actual_predicts, 1, min) / apply(actual_predicts, 1, max))
  
  actual_predicts$difference <- NULL
  
  actual_predicts$difference <-
    actual_predicts$actual - actual_predicts$predicted
  
  acceptableTimeRange <- 4
  
  resultsWithInRange <- actual_predicts %>%
    filter(difference <= acceptableTimeRange / 2 &
             difference >= -(acceptableTimeRange / 2))
  
  actualAccuracy <-
    100 / nrow(actual_predicts) * nrow(resultsWithInRange)
}

# Correlation plot --------------------------------------------------------

createCorrelationPlot <- function(){
  numberfySmart <- function(smart) {
    if (smart == "Yes") {
      return(1)
    } else if (smart == "No") {
      return(0)
    }
  }
  
  temp <- as.data.frame(unique(dataFrame$car))
  colnames(temp) <- c("car")
  temp$carNumber <- NA
  nrow(temp)
  for (i in 1:nrow(temp)) {
    temp[i, 2] <- i
  }
  
  dataFrame <- base::merge(dataFrame, temp)
  
  df_cor_test <- dataFrame %>%
    select(hours_elapsed, hour, start_tf, charged_kwh, carNumber, class) %>%
    mutate(day_number = wday(as.Date(dataFrame$start_date))) %>%
    mutate(smart_charger = map(dataFrame$smart_charging, numberfySmart)) %>%
    mutate(smart_charger = as.numeric(unlist(smart_charger))) %>%
    mutate(class = as.numeric(unlist(class)))
  
  df_cor_test$hours_elapsed <- as.numeric(df_cor_test$hours_elapsed)
  df_cor_test$hour <- as.numeric(df_cor_test$hour)
  df_cor_test$start_tf <- as.numeric(df_cor_test$start_tf)
  
  colnames(df_cor_test) <- c("Hours Elapsed", "Time in H", "Timeframe", "Charged KwH", "Car", "User class", "Day of week", "Smart")
  str(df_cor_test)
  corrplot.mixed(cor(df_cor_test))
}

# Function calls ----------------------------------------------------------

createLinearModelData()
createCorrelationPlot()
