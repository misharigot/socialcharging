# Linear regression model
library(ggplot2)
library(config)
library(readr)
library(lubridate)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scBigDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# # randomly selected date ranges
# minDate <- ymd_hms("2017-05-05 00:00:00")
# maxDate <- ymd_hms("2017-10-30 00:00:00")
# emptyMaxDate <- ymd_hms("")
#
# if(is.null(emptyMaxDate) || is.na(emptyMaxDate)){
#   df <- subset(df, start_date > minDate)
# } else {
#   df <- subset(df, start_date > minDate & start_date < emptyMaxDate)
# }

hoursToSeconds <- 3600
minUserSessions <- 5

usersWithEnoughSessions <- df %>%
  group_by(user_id) %>%
  summarise(count = n()) %>%
  filter(count >= minUserSessions) %>%
  select(user_id)

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

temp <- df[df[, "user_id"] == 5, ]

df$start_date <- as.numeric(df$start_date)
df$end_date <- as.numeric(df$end_date)

set.seed(100)
trainingRowIndex <- sample(1:nrow(df), 0.8 * nrow(df))
trainingData <-
  df[trainingRowIndex, ]  # model training data 80% traning
testData  <- df[-trainingRowIndex, ]   # test data 20% test

# Testing linear model ----------------------------------------------------

lm_df <-
  lm(end_date ~ user_id + start_date + dayOfWeek, data = trainingData)

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

points(testData$start_date[ranks], ChargingSessionPredict[ranks], col = "red")

lines(testData$start_date[ranks],
      ChargingSessionPredict[ranks],
      lwd = 2,
      col = "green")

# correlation_acc <- cor(actual_predicts)

# plot(start_date ~ .,
#      data = testData,
#      xlab = "",
#      ylab = "Start date in seconds")

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

# converting seconds to datetime
actual_predicts$actual <-
  as.POSIXct(as.numeric(actual_predicts$actual),
             origin = "1970-01-01",
             tzdb = "GMT1")

actual_predicts$predicted <-
  as.POSIXct(as.numeric(actual_predicts$predicted),
             origin = "1970-01-01",
             tzdb = "GMT1")

actual_predicts$difference <- NULL

actual_predicts$difference <-
  seconds_to_period(actual_predicts$actual - actual_predicts$predicted)
