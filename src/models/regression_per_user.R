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
  day <- factor(seq(1,7))
  df %>% mutate(starting_hour = as.factor(paste0(hour(start_date), ".", ifelse(minute(start_date) < 30, 0, 5)) ),
                day = as.factor(getDay(start_date)),
                weekend = as.logical(ifelse(day %in% c(6,7), 1, 0))
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

sessionsForUser <- sessions %>% filter(user_id == 1080)
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

# sessionsOnMonday / totalMondays

# classifier <- naiveBayes(sessionsForUser, sessionsForUser$starting_hour, formula = starting_hour ~ day, laplace = 1)

predictWeekForUser <- function(userId, sessions) {
  userId = 1080
  sessions <- sessions %>% filter(user_id == userId)
  
  classifier <- naiveBayes(starting_hour ~ day + weekend, sessions, laplace = 1)
  
  weekDf <- data.frame(
    user_id = rep(userId, 7),
    day = c(seq(1,7)),
    weekend = c(0,0,0,0,0,1,1)
  )
  
  sessionsPerDay <- sessions %>%
    group_by(user_id, day) %>% 
    summarise(amountOfSessions = n())
  
  weeksElapsed <- sessions %>% 
    group_by(user_id) %>% 
    summarise(min = min(start_date), max = max(start_date), totalWeeksElapsed = getWeeksElapsed(min, max))
  
  sessionsPerDay <- base::merge(sessionsPerDay, weeksElapsed[, c("user_id", "totalWeeksElapsed")], by = "user_id") %>% 
    mutate(avgSessions = amountOfSessions/totalWeeksElapsed) %>%
    filter(user_id == userId)
  ?naiveBayes
  pred <- predict(classifier, weekDf, type = "class")
  weekDf$pred <- predict(classifier, weekDf, type = "class")
  # weekDf <- base::merge(weekDf, sessionsPerDay[, c("day", "avgSessions")], by = "day")
}

predicted <- predictWeekForUser(1080, sessions)

# Accuracy
truth <- (as.double(as.character(pred)) -  as.double(as.character(sessionsForUser$starting_hour)) < .5)
table(truth)
trues <- sum(table(truth)["TRUE"])
falses <- sum(table(truth)["FALSE"])
total <- trues + falses
trues / total

# Returns the sessions belonging to a random week in the df given.
getRandomWeekData <- function(df) {
  randomRowIndex <- sample(nrow(df), 1)
  randomWeek <- df %>% filter(getWeekNumber(start_date) == getWeekNumber(df[randomRowIndex, ]$start_date))
  return(randomWeek)
}

