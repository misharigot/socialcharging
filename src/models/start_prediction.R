# Linear regression model for predicting session end time based on user classification
library(config)
library(readr)
library(xgboost)
library(lubridate)
library(naivebayes)
library(e1071)

config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/helpers/date_helper.R")

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
      starting_hour = factor(as.double(paste0(hour(start_date), ".", ifelse(minute(start_date) < 30, "0", "5")), levels = seq(0, 23.5, by = .5))),
      day = factor(getDay(start_date), levels = seq(1, 7)),
      weekend = as.logical(ifelse(day %in% c(6, 7), 1, 0))
    )
}

# Returns sessions where users have minimumSessions amount of sessions
getSessions <- function(minimumSessions = 30) {
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
  probabilityDf[, "max"] <- apply(probabilityDf[ ,1:48], 1, max)
  
  numberOfDays <- c(1:7)
  
  for (i in numberOfDays) {
    rowNumber <- which(dataframe$day == i)
    dataframe[rowNumber, "pred_acc"] <- probabilityDf[i, "max"]
  }
  
  return(dataframe)
}

sessions <- getSessions(minimumSessions = 30)
summary(sessions)
sessionsForUser <- sessions %>% filter(user_id == 46)
sessionsPerUser <- sessions %>% group_by(user_id) %>% summarise(n = n())

summForUser <- sessionsForUser %>% group_by(day, starting_hour) %>% summarise(count = n())

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
  
  classifier <- naive_bayes(starting_hour ~ day + weekend, sessions, laplace = 1)
  
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

# Public API call -------------------------------------------------------------------------------------------------

predicted <- predictWeekForUser(1048, sessions)
