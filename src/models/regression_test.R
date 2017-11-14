# Linear regression model for predicting end_date
library(ggplot2)
library(config)
library(readr)
library(lubridate)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scBigDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Minimum amount of sessions required in order to get the
minUserSessions <- 5

# Data preperation --------------------------------------------------------

# Gets the user that met the minimum required sessions
usersWithEnoughSessions <- df %>%
  group_by(user_id) %>%
  summarise(count = n()) %>%
  filter(count >= minUserSessions) %>%
  select(user_id, count)

# Gets the day of the week from the date
df$dayOfWeek <- weekdays(as.Date(df$start_date))

df <- df %>%
  filter(
    !is.na(hours_elapsed),
    hours_elapsed > 0.00,
    !is.na(start_date),
    !is.na(end_date),
    user_id %in% usersWithEnoughSessions$user_id
  ) %>%
  select(session_id,
         user_id,
         start_date,
         dayOfWeek,
         end_date,
         charged_kwh)

# Converts the datetime to seconds
df$start_date <- as.numeric(df$start_date)
df$end_date <- as.numeric(df$end_date)


# Create train and test data from the data --------------------------------

set.seed(100)
trainingRowIndex <- sample(1:nrow(df), 0.7 * nrow(df))
trainingData <-
  df[trainingRowIndex, ]  # 70% training data
testData  <- df[-trainingRowIndex, ]   # remaining test data

# Testing linear model ----------------------------------------------------

# build linear model to predict end_date with the following parameters
lm_df <-
  lm(end_date ~ start_date + dayOfWeek, data = trainingData)

modelSummary <- summary(lm_df)
modelCoeffs <- modelSummary$coefficients
rSquared <- modelSummary$r.squared
adjRSquared <- modelSummary$adj.r.squared

ranks <- order(testData$start_date)

ChargingSessionPredict <- predict(lm_df, testData)

actual_predicts <- data.frame(cbind(actual = testData$end_date,
                                    predicted = ChargingSessionPredict))

plot(testData$start_date,
     testData$end_date,
     xlab = "Start date",
     ylab = "End date")

points(testData$start_date[ranks],
       ChargingSessionPredict[ranks],
       col = "green")

# abline(lm_df$coefficients, col = "red")

plot(lm_df$fitted.values,
     lm_df$residuals,
     xlab = "Fitted values",
     ylab = "Residuals")

qqnorm(lm_df$residuals, ylab = "Residual Quantiles")

res_test <- ChargingSessionPredict - testData$end_date

rmse_test <- sqrt(mean(res_test ^ 2))

# oneliner version of rmse calculation above
# sqrt(mean((ChargingSessionPredict - testData$end_date) ^2))

rmse_train <- sqrt(mean(lm_df$residuals ^ 2))

# Ratio of test RMSE over training RMSE
rmse_test / rmse_train

minMaxAccuracy <- mean(apply(actual_predicts, 1, min) / apply(actual_predicts, 1, max))

actual_predicts$difference <- NULL

actual_predicts$difference <-
  actual_predicts$actual - actual_predicts$predicted

# converting seconds to datetime
actual_predicts$actual <-
  as.POSIXct(as.numeric(actual_predicts$actual),
             origin = "1970-01-01",
             tzdb = "GMT1")

actual_predicts$predicted <-
  as.POSIXct(as.numeric(actual_predicts$predicted),
             origin = "1970-01-01",
             tzdb = "GMT1")

acceptableTimeRange <- 4 * 3600

resultsWithInRange <- actual_predicts %>%
  filter(difference <= acceptableTimeRange / 2 & difference >= -(acceptableTimeRange / 2))

actualAccuracy <- 100 / nrow(actual_predicts) * nrow(resultsWithInRange)

# actual_predicts$difference <-
#   seconds_to_period(actual_predicts$actual - actual_predicts$predicted)
# 
# actual_predicts$difference <- gsub("\\..*", "S", as.character(actual_predicts$difference))
