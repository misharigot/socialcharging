# To retrieve the predicted data, you need to run the following function with a csv DF cleaned by base_clean as the argument:
# getPredictedValuesDf(df)
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
  
  df <- df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed > 0.00,!is.na(start_date),!is.na(end_date),
      user_id %in% usersWithEnoughSessions$user_id,
      !is.na(start_date),
      !is.na(charged_kwh),
      !is.na(hours_elapsed)) %>%
    mutate(start_date_hour = hour(start_date)) %>%
    select(session_id,
           user_id,
           start_date,
           start_date_hour,
           charged_kwh,
           hours_elapsed) 
  
  return(df)
}

# Public API ------------------------------------------------------------------------------------------------------

getPredictedValuesDf <- function(df) {
  total_df <- cleanDf(df)
  
  total_df$day <-
    strftime(as.Date(total_df$start_date), format = "%u")
  
  lm_df_connectionTime <-
    lm(hours_elapsed ~ start_date_hour + hours_elapsed + charged_kwh + day ,
       data = total_df)
  
  lm_df_kwh <-
    lm(charged_kwh ~ start_date_hour + hours_elapsed + charged_kwh + day, data = total_df)
  
  lm_df_startTime <-
    lm(start_date_hour ~ start_date_hour + hours_elapsed + charged_kwh + day, data = total_df)
  
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
    group_by(user_id, day) %>%
    summarise(pred_start_time = round(mean(pred_start_time)),
           pred_hours_elapsed = mean(pred_hours_elapsed),
           pred_kwh = mean(pred_kwh))
  return(total_df)
}