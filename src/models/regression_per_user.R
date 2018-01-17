# Linear regression model for predicting session end time based on user classification
library(config)
library(readr)
library(xgboost)
library(lubridate)
library(naivebayes)
library(e1071)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)

# Filters out sessions invalid for regression. This function is also usable in dplyr's piped statements.
filterSessions <- function(df) {
  df %>% filter(!is.na(charged_kwh), hours_elapsed > 0, charged_kwh > 0)
}

# Adds features (new columns) based on existing columns in df.
addFeatures <- function(df) {
  df %>% mutate(starting_hour = as.factor(hour(start_date)),
                day = as.factor(strftime(as.Date(start_date), format = "%u")),
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
sessions2 <- getSessions(minimumSessions = 30) %>% group_by(user_id) %>% summarise(n = n())

sessionsForUser <- sessions %>% filter(user_id == 742)
# sessionsForUser <- sessions %>% filter(user_id == 46) %>% filter(start_date >=  "2017-01-09", end_date <= "2017-01-17")

summForUser <- sessionsForUser %>% group_by(day, starting_hour) %>% summarise(count = n())

# Create a df that represents a week with the hours per day (24/7)
# weekDf <- data.frame(
#   day = c(rep(1,24), rep(2,24), rep(3,24), rep(4,24), rep(5,24), rep(6,24), rep(7,24)),
#   hour = c(seq(0,23), seq(0,23), seq(0,23), seq(0,23), seq(0,23), seq(0,23), seq(0,23))
# )
weekDf <- data.frame(
  day = c(seq(1,7))
)

# classifier <- naiveBayes(sessionsForUser, sessionsForUser$starting_hour, formula = starting_hour ~ day, laplace = 1)
classifier <- naiveBayes(starting_hour ~ day + weekend, sessionsForUser, laplace = 1)
pred <- predict(classifier, sessionsForUser, type = "class")
sessionsForUser$pred <- predict(classifier, sessionsForUser, type = "class")

# Accuracy
truth <- (as.numeric(pred) -  as.numeric(sessionsForUser$starting_hour) < 1)
table(truth)
trues <- sum(table(truth)["TRUE"])
falses <- sum(table(truth)["FALSE"])
total <- trues + falses
trues / total
