library(purrr)
library(tidyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
config <- config::get(file = "config.yml")

# Classification --------------------------------------------------------------------------------------------------

# Filter and select only relevant rows/columns
cleanDf <- function(df) {
  # Users ids that have sessions >= minUserSessions
  usersWithEnoughSessions <- df %>%
    group_by(user_id) %>%
    summarise(count = n()) %>%
    filter(count >= minUserSessions) %>%
    select(user_id)
  
  df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed > 0.00,!is.na(start_date),!is.na(end_date),
      user_id %in% usersWithEnoughSessions$user_id
    ) %>%
    select(session_id,
           user_id,
           start_date,
           end_date,
           charged_kwh,
           hours_elapsed)
}

# Returns the bucket a POSIXct datetime belongs to
classifyTf <- function(datetime) {
  # datetime <- hour(datetime)
  if (datetime >= 12 & datetime < 18) {
    return("12-18")
  } else if (datetime >= 18 & datetime < 24) {
    return("18-00")
  } else if (datetime >= 0 & datetime < 6) {
    return("00-06")
  } else if (datetime >= 6 & datetime < 12) {
    return("06-12")
  }
}

# Boolean checks
isMorning <- function(x) {
  return(x == "06-12")
}

isAfternoon <- function(x) {
  return(x == "12-18")
}

isEvening <- function(x) {
  return(x == "18-00")
}

isNight <- function(x) {
  return(x == "00-06")
}

classifyElapsed <- function(elapsed) {
  if (elapsed <= 1) {
    return(1)
  } else if (elapsed > 1 & elapsed <= 2) {
    return(2)
  } else if (elapsed > 2 & elapsed <= 4) {
    return(3)
  } else if (elapsed > 4 & elapsed <= 6) {
    return(4)
  } else if (elapsed > 6 & elapsed <= 8) {
    return(5)
  } else if (elapsed > 8 & elapsed <= 10) {
    return(6)
  } else if (elapsed > 10 & elapsed <= 12) {
    return(7)
  } else if (elapsed > 12 & elapsed <= 14) {
    return(8)
  } else if (elapsed > 14 & elapsed <= 16) {
    return(9)
  } else if (elapsed > 16 & elapsed <= 18) {
    return(10)
  } else if (elapsed > 18 & elapsed <= 20) {
    return(11)
  } else if (elapsed > 20 & elapsed <= 22) {
    return(12)
  } else if (elapsed > 22 & elapsed <= 24) {
    return(13)
  } else {
    return(-1)
  }
}

classifyKwh <- function(kwh) {
  if (kwh > 0 & kwh <= 1) {
    return(1)
  } else if (kwh > 1 & kwh <= 2) {
    return(2)
  } else if (kwh > 2 & kwh <= 4) {
    return(3)
  } else if (kwh > 4 & kwh <= 6) {
    return(4)
  } else if (kwh > 8 & kwh <= 10) {
    return(5)
  } else if (kwh > 10 & kwh <= 15) {
    return(6)
  } else if (kwh > 15 & kwh <= 20) {
    return(7)
  } else if (kwh > 20 & kwh <= 25) {
    return(8)
  } else if (kwh > 25 & kwh <= 35) {
    return(9)
  } else if (kwh > 35 & kwh <= 45) {
    return(10)
  } else if (kwh > 45) {
    return(11)
  } else {
    return(-1)
  }
}

## Returns the classification a session belongs to
# stf: start_tf
# etf: end_tf
# hrs: hours_elapsed
getSessionClass <- function(stf, etf, hrs) {
  # stf: Morning
  if (isMorning(stf) & isMorning(etf) & hrs < 24) {
    return(1)
  } else if (isMorning(stf) & isAfternoon(etf) & hrs < 24) {
    return(2)
  } else if (isMorning(stf) & isEvening(etf) & hrs < 24) {
    return(3)
  } else if (isMorning(stf) & isNight(etf) & hrs < 24) {
    return(4)
  }
  
  # stf: Afternoon
  if (isAfternoon(stf) & isMorning(etf) & hrs < 24) {
    return(5)
  } else if (isAfternoon(stf) & isAfternoon(etf) & hrs < 24) {
    return(6)
  } else if (isAfternoon(stf) & isEvening(etf) & hrs < 24) {
    return(7)
  } else if (isAfternoon(stf) & isNight(etf) & hrs < 24) {
    return(8)
  }
  
  # stf: Evening
  if (isEvening(stf) & isMorning(etf) & hrs < 24) {
    return(9)
  } else if (isEvening(stf) & isAfternoon(etf) & hrs < 24) {
    return(10)
  } else if (isEvening(stf) & isEvening(etf) & hrs < 24) {
    return(11)
  } else if (isEvening(stf) & isNight(etf) & hrs < 24) {
    return(12)
  }
  
  # stf: Night
  if (isNight(stf) & isMorning(etf) & hrs < 24) {
    return(13)
  } else if (isNight(stf) & isAfternoon(etf) & hrs < 24) {
    return(14)
  } else if (isNight(stf) & isEvening(etf) & hrs < 24) {
    return(15)
  } else if (isNight(stf) & isNight(etf) & hrs < 24) {
    return(16)
  }
  return(-1) # Greater than 24 hours elapsed
}

# Classify sessions on timeframes
sessionClassificationDf <- function(cleanDf) {
  sessionClassifications <- cleanDf %>%
    mutate(start_date_hour = hour(start_date),
           end_date_hour = hour(end_date)) %>%
    mutate(
      start_tf = map(start_date_hour, classifyTf),
      end_tf = map(end_date_hour, classifyTf)
    ) %>%
    mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf))) %>%
    rowwise() %>%
    mutate(
      tfClass = getSessionClass(start_tf, end_tf, hours_elapsed),
      heClass = map(hours_elapsed , classifyElapsed),
      kwhClass = map(charged_kwh, classifyKwh)
    )
  
  sessionClassifications$tfClass <-
    as.factor(sessionClassifications$tfClass)
  sessionClassifications$heClass <-
    as.factor(unlist(sessionClassifications$heClass))
  sessionClassifications$kwhClass <-
    as.factor(unlist(sessionClassifications$kwhClass))
  return(sessionClassifications)
}


#Classify users by their most dominant timeframe
userClassificationDf <- function(sessionClassification) {
  tfClassifications <- sessionClassification %>%
    count(user_id, tfClass) %>%
    group_by(user_id) %>%
    slice(which.max(n))
  
  heClassifications <- sessionClassification %>%
    count(user_id, heClass) %>%
    group_by(user_id) %>%
    slice(which.max(n))
  
  kwhClassifications <- sessionClassification %>%
    count(user_id, kwhClass) %>%
    group_by(user_id) %>%
    slice(which.max(n))
  
  mergedDf <-
    base::merge(tfClassifications, heClassifications, by = "user_id")
  mergedDf <-
    base::merge(mergedDf, kwhClassifications, by = "user_id")
  mergedDf$n.x <- NULL
  mergedDf$n.y <- NULL
  mergedDf$n <- NULL
  
  return(mergedDf)
}


# Test function -----------------------------------------------------------

# Just CTRL + Enter inside this function to test it.
testExtendedPrediction <- function() {
  
  minUserSessions <- 5
  
  df <- read.csv(config$dataFolder)
  
  total_df <- sessionClassificationDf(cleanDf(df))
  
  total_df$tfClass <- as.numeric(total_df$tfClass)
  total_df$heClass <- as.numeric(total_df$heClass)
  total_df$kwhClass <- as.numeric(total_df$kwhClass)
  
  total_df$dayOfWeek <-
    weekdays(as.Date(total_df$start_date))
  
  lm_df_connectionTime <-
    lm(hours_elapsed ~ tfClass + heClass + kwhClass + dayOfWeek ,
       data = total_df)
  
  lm_df_kwh <-
    lm(charged_kwh ~ tfClass + heClass + kwhClass + dayOfWeek, data = total_df)
  
  lm_df_startTime <-
    lm(start_date_hour ~ tfClass + heClass + kwhClass + dayOfWeek, data = total_df)
  
  lm_df_endTime <-
    lm(end_date_hour ~ tfClass + heClass + kwhClass + dayOfWeek, data = total_df)
  
  summary(lm_df_connectionTime)
  summary(lm_df_kwh)
  summary(lm_df_startTime)
  summary(lm_df_endTime)
  
  total_df$EstConnectionTimeHours <-
    predict(lm_df_connectionTime, total_df)
  total_df$EstChargedKWh <- predict(lm_df_kwh, total_df)
  total_df$EstStartTime <- predict(lm_df_startTime, total_df)
  total_df$EstEndTime <- predict(lm_df_endTime, total_df)
  
  total_df <- total_df %>%
    filter(
      !is.na(EstConnectionTimeHours),
      !is.na(EstChargedKWh),
      !is.na(EstStartTime),
      !is.na(EstEndTime)
    )
  
  total_df$diffEstConnectionTime <-
    total_df$hours_elapsed - total_df$EstConnectionTimeHours
  total_df$diffChargedKWh <-
    total_df$charged_kwh - total_df$EstChargedKWh
  total_df$diffStartTime <-
    total_df$start_date_hour - total_df$EstStartTime
  total_df$diffEndTime <-
    total_df$end_date_hour - total_df$EstEndTime
  
  source("src/models/regression_test.R")
  statsConnectionTime <-
    testPrediction(total_df$diffEstConnectionTime, 1)
  statsChargedKWh <- testPrediction(total_df$diffChargedKWh, 1)
  statsStartTime <- testPrediction(total_df$diffStartTime, 1)
  statsEndTime <- testPrediction(total_df$diffEndTime, 1)
}
