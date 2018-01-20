# Linear regression model for predicting session end time based on user classification
library(config)
library(readr)
library(xgboost)
library(lubridate)
# library(naivebayes)
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

sessions <- getSessions(minimumSessions = 30)
summary(sessions)
sessionsForUser <- sessions %>% filter(user_id == 46)
# sessionsForUser <- sessions %>% filter(user_id == 46) %>% filter(start_date >=  "2017-01-09", end_date <= "2017-01-17")

summForUser <- sessionsForUser %>% group_by(day, starting_hour) %>% summarise(count = n())

# Create a df that represents a week with the hours per day (24/7)
# weekDf <- data.frame(
#   day = c(rep(1,24), rep(2,24), rep(3,24), rep(4,24), rep(5,24), rep(6,24), rep(7,24)),
#   hour = c(seq(0,23), seq(0,23), seq(0,23), seq(0,23), seq(0,23), seq(0,23), seq(0,23))
# )

getWeeksElapsed <- function(date1, date2) {
  as.numeric(round(abs(date1 - date2) / 7, 0))
}

# 1. Predict the chance a session is going to happen on day x for user y.
# 2. For days with > 0 sessions, predict their starting_hour

predictWeekForUser <- function(userId, sessions) {
  # userId = 1080
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
    mutate(sessionChance = totalWeeksElapsed / amountOfSessions * 100) %>%
    filter(user_id == userId)
  
  occuringDays <- sessions %>% distinct(day)
  filteredWeekDf <- weekDf %>% filter(day %in% occuringDays$day) %>% mutate(day = factor(day, levels = occuringDays$day))
  filteredWeekDf$pred_starting_hour <- predict(classifier, filteredWeekDf, type = "class")
  
  # To Do: get this prob in a column
  filteredWeekDf$pred_acc <- 0
  probability <- predict(classifier, filteredWeekDf, type = "prob")

  nonOccuringDays <- weekDf %>% filter(!(day %in% occuringDays$day))
  if (nrow(nonOccuringDays) > 0) {
    nonOccuringDays$pred_starting_hour <- NA
    nonOccuringDays$pred_acc <- NA
    filteredWeekDf <- rbind(filteredWeekDf, nonOccuringDays) 
  }
  resultWeekDf <- base::merge(filteredWeekDf, sessionsPerDay[, c("day", "sessionChance")], by = "day", all = T)
}

predicted <- predictWeekForUser(184, sessions)

# Accuracy
# truth <- (as.double(as.character(pred)) -  as.double(as.character(sessionsForUser$starting_hour)) < .5)
# table(truth)
# trues <- sum(table(truth)["TRUE"])
# falses <- sum(table(truth)["FALSE"])
# total <- trues + falses
# trues / total

# Returns the sessions belonging to a random week in the df given.
getRandomWeekData <- function(df) {
  randomRowIndex <- sample(nrow(df), 1)
  randomWeek <- df %>% filter(getWeekNumber(start_date) == getWeekNumber(df[randomRowIndex, ]$start_date))
  return(randomWeek)
}

