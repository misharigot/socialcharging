# Currently testing linear model with hours_elapsed and charged_kwh -------

library(ggplot2)
library(config)
library(readr)
library(lubridate)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scBigDataset, col_names = FALSE)

df <- cleanSecondDataframe(df)
df <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh), hours_elapsed > 0) %>%
  mutate(percentage_charging = sapply(100 / hours_elapsed * effective_charging_hours, function(x) {
    round(x, digits = 2)
  }))

# # random date ranges
# minDate <- ymd_hms("2017-05-05 00:00:00")
# maxDate <- ymd_hms("2017-10-30 00:00:00")
# emptyMaxDate <- ymd_hms("")
# 
# # simple test with specified date range
# if(is.null(emptyMaxDate) || is.na(emptyMaxDate)){
#   test <- subset(df, start_date > minDate)
# } else {
#   test <- subset(df, start_date > minDate & start_date < emptyMaxDate)
# }

test <- df
toSeconds <- 3600

test$start_date <- as.numeric(test$start_date)
test$end_date <- as.numeric(test$end_date)
test$hours_elapsed <- test$hours_elapsed * toSeconds

set.seed(100)
trainingRowIndex <- sample(1:nrow(test), 0.8*nrow(test))
trainingData <- test[trainingRowIndex, ]  # model training data 80% traning
testData  <- test[-trainingRowIndex, ]   # test data 20% test


# Normal dataframe test ---------------------------------------------------

# as.numeric(trainingData$start_date[1])

lm_df <- lm(end_date ~ start_date + hours_elapsed, data = trainingData)
print(lm_df)
modelSummary <- summary(lm_df)
modelCoeffs <- modelSummary$coefficients
rSquared <- modelSummary$r.squared
adjRSquared <- modelSummary$adj.r.squared

ChargingSessionPredict <- predict(lm_df, testData)
AIC(lm_df)

actual_predicts <- data.frame(cbind(actual = testData$end_date, 
                                    predicted = ChargingSessionPredict))

correlation_acc <- cor(actual_predicts)

# converting seconds to datetime
actual_predicts$actual <- as.POSIXct(as.numeric(actual_predicts$actual), origin = '1970-01-01', tzdb = 'GMT1')
actual_predicts$predicted <- as.POSIXct(as.numeric(actual_predicts$predicted), origin = '1970-01-01', tzdb = 'GMT1')

