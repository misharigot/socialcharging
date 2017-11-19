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

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Minimum amount of sessions required in order to get the
minUserSessions <- 10

# Data preperation --------------------------------------------------------


# Gets the sessions for the specified user
userSessions <- subset(df, df$user_id == 20)

# Gets the day of the week from the date
df$dayOfWeek <- weekdays(as.Date(df$start_date))

df$hour <- as.numeric(format(round(df$start_date, "hours"), format = "%H"))

df <- df %>%
  filter(
    !is.na(hours_elapsed),
    hours_elapsed >= 2 & hours_elapsed <= 24,
    !is.na(start_date),
    !is.na(end_date),
    !is.na(car),
    !is.na(charged_kwh),
    user_id %in% userSessions$user_id
  ) 

# Testing linear model ----------------------------------------------------

# build linear model to predict end_date with the following parameters

if(nrow(df) >= minUserSessions){

  # Create train and test data from the data
  set.seed(100)
  trainingRowIndex <- sample(1:nrow(df), 0.7 * nrow(df))
  trainingData <-
    df[trainingRowIndex, ]  # 70% training data
  testData <- df[-trainingRowIndex, ]   # remaining test data
  
  # Create linear model
  lm_df <<-
    lm(hours_elapsed ~ hour,
       data = trainingData)
  
  plot(df$charged_kwh,df$hours_elapsed)
  
}else{
  print("User does not have enough sessions to make a prediction")
}
