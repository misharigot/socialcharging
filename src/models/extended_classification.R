# To retreive the data you need to run the following code with a dataframe as paramater:
# df <- testExtendedPrediction(df)

library(purrr)
library(tidyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
config <- config::get(file = "config.yml")

minUserSessions <- 5

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
    filter(!is.na(start_date),
           !is.na(charged_kwh),
           !is.na(hours_elapsed)) %>%
    select(session_id,
           user_id,
           start_date,
           end_date,
           charged_kwh,
           hours_elapsed)
}

# Returns the calssification or bucketed start time
classifyTf <- function(time) {
  
  if (time >= 0 & time < 2) {
    return(1)
  } else if (time >= 2 & time < 4) {
    return(2)
  } else if (time >= 4 & time < 6) {
    return(3)
  } else if (time >= 6 & time < 8) {
    return(4)
  } else if (time >= 8 & time < 10) {
    return(5)
  } else if (time >= 10 & time < 12) {
    return(6)
  } else if (time >= 12 & time < 14) {
    return(7)
  } else if (time >= 14 & time < 16) {
    return(8)
  } else if (time >= 16 & time < 18) {
    return(9)
  } else if (time >= 18 & time < 20) {
    return(10)
  } else if (time >= 20 & time < 22) {
    return(11)
  } else if (time >= 22 & time < 24) {
    return(12)
  } else {
    return(-1)
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
# getSessionClass <- function(stf, etf, hrs) {
#   # stf: Morning
#   if (isMorning(stf) & isMorning(etf) & hrs < 24) {
#     return(1)
#   } else if (isMorning(stf) & isAfternoon(etf) & hrs < 24) {
#     return(2)
#   } else if (isMorning(stf) & isEvening(etf) & hrs < 24) {
#     return(3)
#   } else if (isMorning(stf) & isNight(etf) & hrs < 24) {
#     return(4)
#   }
#   
#   # stf: Afternoon
#   if (isAfternoon(stf) & isMorning(etf) & hrs < 24) {
#     return(5)
#   } else if (isAfternoon(stf) & isAfternoon(etf) & hrs < 24) {
#     return(6)
#   } else if (isAfternoon(stf) & isEvening(etf) & hrs < 24) {
#     return(7)
#   } else if (isAfternoon(stf) & isNight(etf) & hrs < 24) {
#     return(8)
#   }
#   
#   # stf: Evening
#   if (isEvening(stf) & isMorning(etf) & hrs < 24) {
#     return(9)
#   } else if (isEvening(stf) & isAfternoon(etf) & hrs < 24) {
#     return(10)
#   } else if (isEvening(stf) & isEvening(etf) & hrs < 24) {
#     return(11)
#   } else if (isEvening(stf) & isNight(etf) & hrs < 24) {
#     return(12)
#   }
#   
#   # stf: Night
#   if (isNight(stf) & isMorning(etf) & hrs < 24) {
#     return(13)
#   } else if (isNight(stf) & isAfternoon(etf) & hrs < 24) {
#     return(14)
#   } else if (isNight(stf) & isEvening(etf) & hrs < 24) {
#     return(15)
#   } else if (isNight(stf) & isNight(etf) & hrs < 24) {
#     return(16)
#   }
#   return(-1) # Greater than 24 hours elapsed
# }

# Classify sessions on timeframes
sessionClassificationDf <- function(cleanDf) {
  
  sessionClassifications <- cleanDf %>%
    mutate(
      start_date_hour = hour(start_date))
  
  sessionClassifications <- sessionClassifications %>%
    mutate(
      start_time_class = map(start_date_hour, classifyTf),
      hours_elapsed_class = map(hours_elapsed , classifyElapsed),
      kwh_class = map(charged_kwh, classifyKwh))
  
  sessionClassifications$start_time_class <-
    as.factor(unlist(sessionClassifications$start_time_class))
  sessionClassifications$hours_elapsed_class <-
    as.factor(unlist(sessionClassifications$hours_elapsed_class))
  sessionClassifications$kwh_class <-
    as.factor(unlist(sessionClassifications$kwh_class))
  
  return(sessionClassifications)
}

#Classify users by their most dominant timeframe
userClassificationDf <- function(sessionClassification) {
  
  tfClassifications <- sessionClassification %>%
    count(user_id, start_time_class) %>%
    group_by(user_id) %>%
    slice(which.max(n))
  
  heClassifications <- sessionClassification %>%
    count(user_id, hours_elapsed_class) %>%
    group_by(user_id) %>%
    slice(which.max(n))
  
  kwhClassifications <- sessionClassification %>%
    count(user_id, kwh_class) %>%
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

# Test function regression --------------------------------------------------------------------------

testExtendedPrediction <- function(df) {
  
  total_df <- sessionClassificationDf(cleanDf(df))
  
  total_df$start_time_class <- as.numeric(total_df$start_time_class)
  total_df$hours_elapsed_class <- as.numeric(total_df$hours_elapsed_class)
  total_df$kwh_class <- as.numeric(total_df$kwh_class)
  
  total_df$day <-
    strftime(as.Date(total_df$start_date), format = "%u")
  
  lm_df_connectionTime <-
    lm(hours_elapsed ~ start_time_class + hours_elapsed_class + kwh_class + day ,
       data = total_df)
  
  lm_df_kwh <-
    lm(charged_kwh ~ start_time_class + hours_elapsed_class + kwh_class + day, data = total_df)
  
  lm_df_startTime <-
    lm(start_date_hour ~ start_time_class + hours_elapsed_class + kwh_class + day, data = total_df)
  
  summary(lm_df_connectionTime)
  summary(lm_df_kwh)
  summary(lm_df_startTime)
  
  total_df$pred_hours_elapsed <-
    predict(lm_df_connectionTime, total_df)
  total_df$pred_kwh <- predict(lm_df_kwh, total_df)
  total_df$pred_start_time <- predict(lm_df_startTime, total_df)
  
  total_df <- total_df %>%
    filter(
      !is.na(pred_hours_elapsed),
      !is.na(pred_kwh),
      !is.na(pred_start_time)
    ) %>%
    group_by(start_time_class, hours_elapsed_class, kwh_class, day) %>%
    summarise(pred_start_time = round(mean(pred_start_time)),
           pred_hours_elapsed = mean(pred_hours_elapsed),
           pred_kwh = mean(pred_kwh))
  return(total_df)
}
