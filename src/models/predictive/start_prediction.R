# Linear regression model for predicting session end time based on user classification
library(config)
library(readr)
library(xgboost)
library(lubridate)
library(naivebayes)
library(e1071)
library(tidyr)

config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/helpers/date_helper.R")
set.seed(10)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)

# Filters out sessions invalid for regression. This function is also usable in dplyr's piped statements.
filterSessions <- function(df) {
  df %>% filter(!is.na(charged_kwh), hours_elapsed > 0, charged_kwh > 0)
}

# Adds features (new columns) based on existing columns in df.
addFeatures <- function(df) {
  df %>%
    mutate(
      starting_hour = factor( hour(start_date), levels = seq(0, 23, by = 1) ),
      day = factor(getDay(start_date), levels = seq(1, 7)),
      week_of_month = factor(getWeekOfMonth(start_date), levels = seq(1, 5)), # e.g. second monday of januari
      weekend = as.logical(ifelse(day %in% c(6, 7), 1, 0))
    )
}

# Returns sessions where users have minimumSessions amount of sessions
getSessions <- function(df, minimumSessions = 30) {
  users <- df %>%
    filterSessions() %>%
    group_by(user_id) %>%
    summarise(n = n()) %>%
    filter(n >= minimumSessions)

  sessions <- df %>%
    filter(user_id %in% users$user_id) %>%
    filterSessions() %>%
    select(-smart_charging, -ev_provider, -car, -corporate, -evse_id, -latitude, -longitude, -address, -kw_charge_point_speed, -outlets) %>%
    addFeatures()
  return(sessions)
}

# Adds the probability chance from naiveBase to a dataframe
addProbability <- function(dataframe, probability) {
  probabilityDf <- probability

  if (!is.data.frame(probabilityDf)) {
    probabilityDf <- as.data.frame(probability)
  }

  # Retrieves the max value and set it into a new column named "max"
  probabilityDf[, "max"] <- apply(probabilityDf[ ,1:24], 1, max)

  numberOfDays <- c(1:7)

  for (i in numberOfDays) {
    rowNumber <- which(dataframe$day == i)
    dataframe[rowNumber, "pred_acc"] <- probabilityDf[i, "max"]
  }

  return(dataframe)
}

# Returns the amount of weeks elapsed between date1 and date2.
getWeeksElapsed <- function(date1, date2) {
  as.numeric(round(abs(date1 - date2) / 7, 0))
}

# Predict a future week for the userId with the sessions given.
predictWeekForUser <- function(userId, sessions) {
  sessions <- sessions %>% filter(user_id == userId)

  if (nrow(sessions) == 0) {
    warning("sessions df is empty.")
  }

  classifier <- naive_bayes(starting_hour ~ day + weekend + week_of_month, sessions, laplace = 1)

  weekDf <- data.frame(
    user_id = rep(userId, 7),
    day = factor(seq(1, 7)),
    weekend = as.logical(c(0, 0, 0, 0, 0, 1, 1))
  )

  sessionsPerDay <- sessions %>%
    group_by(user_id, day) %>%
    summarise(amountOfSessions = n())

  weeksElapsed <- sessions %>%
    group_by(user_id) %>%
    summarise(min = min(start_date), max = max(start_date), totalWeeksElapsed = getWeeksElapsed(min, max))

  sessionsPerDay <- base::merge(sessionsPerDay, weeksElapsed[, c("user_id", "totalWeeksElapsed")], by = "user_id") %>%
    mutate(session_ratio = amountOfSessions / totalWeeksElapsed) %>%
    filter(user_id == userId)

  occuringDays <- sessions %>% distinct(day)
  filteredWeekDf <- weekDf %>% filter(day %in% occuringDays$day) %>% mutate(day = factor(day, levels = occuringDays$day))
  filteredWeekDf$pred_starting_hour <- predict(classifier, filteredWeekDf, type = "class")

  filteredWeekDf$pred_acc <- 0
  probability <- predict(classifier, filteredWeekDf, type = "prob")

  nonOccuringDays <- weekDf %>% filter(!(day %in% occuringDays$day))
  if (nrow(nonOccuringDays) > 0) {
    nonOccuringDays$pred_starting_hour <- NA
    nonOccuringDays$pred_acc <- NA
    filteredWeekDf <- rbind(filteredWeekDf, nonOccuringDays)
  }
  resultWeekDf <- base::merge(filteredWeekDf, sessionsPerDay[, c("day", "session_ratio")], by = "day", all = T)
  resultWeekDf <- addProbability(resultWeekDf, probability)
}

# Returns the sessions belonging to a random week in the sessions df given.
getRandomWeekData <- function(sessions, userId = NULL) {
  randomRowIndex <- sample(nrow(sessions), 1)
  randomWeek <- sessions %>% filter(getWeekNumber(start_date) == getWeekNumber(sessions[randomRowIndex, ]$start_date))
  if (!is.null(userId)) {
    randomWeek <- randomWeek %>% filter(user_id == userId)
  }
  randomWeek <- randomWeek %>% addFeatures() %>% select(day, user_id, weekend, starting_hour, week_of_month)
  return(randomWeek)
}

evalPrediction <- function(actualWeek, predictedWeek, minPredAcc = 0, minSessionRatio = 0) {
  actualWeek <- actualWeek %>% select(day, starting_hour)
  origPredictedWeek <- predictedWeek
  predictedWeek <- predictedWeek %>%
    rename(starting_hour = pred_starting_hour) %>%
    filter(pred_acc > minPredAcc, session_ratio > minSessionRatio) %>%
    left_join(actualWeek %>% mutate(correct = TRUE)) %>%
    replace_na(list(correct = FALSE))

  correctTable <- table(predictedWeek$correct)
  accuracy <- correctTable[2]/(correctTable[1] + correctTable[2])
  writeLines("\n\n********** Evaluation of the prediction: **********\n")
  writeLines(paste0("Min prediction acc. from NB: ", minPredAcc))
  writeLines(paste0("Min session ratio: ", minSessionRatio))
  writeLines(paste0("Number of predicted sessions removed: ", nrow(origPredictedWeek) - nrow(predictedWeek)))
  writeLines(paste0("\nAccuracy: " , round(accuracy, 2)))
  writeLines("")
  return(predictedWeek)
}

# Public API call -------------------------------------------------------------------------------------------------

userId = 46 # The user being predicted

sessions <- getSessions(df, minimumSessions = 30)
sessionsPerUser <- sessions %>% group_by(user_id) %>% summarise(n = n())

sessionsForUser <- sessions %>% filter(user_id == userId)
summaryForUser <- sessionsForUser %>% group_by(day, starting_hour) %>% summarise(count = n())

randomWeek <- getRandomWeekData(sessions, userId)
predictedWeek <- predictWeekForUser(userId, sessions)
evalPrediction(randomWeek, predictedWeek, minPredAcc = 0, minSessionRatio = 0)

source('src/helpers/data_helper.R')
result <- predictFeature(sessionsForUser = sessionsForUser,
                         predictedWeek =  predictedWeek,
                         valueToPredict = "hours_elapsed")
