
# This script does some general cleaning on the social charging csv.
library(readr)
library(dplyr)
library(lubridate)

getScColnames <- function() {
  unlist(strsplit("session_id;user_id;smart_charging;start_date;end_date;ev_provider;car;corporate;evse_id;latitude;longitude;address;kw_charge_point_speed;outlets;charged_kwh",
                  ";"))
}

# Returns the social charging dataframe given, with general cleaning executed on the data set.
cleanDataframe <- function(df) {
  colnames(df) <- getScColnames()
  df %>%
  mutate(start_date = ymd_hms(start_date),
         end_date = ymd_hms(end_date),
         charged_kwh = as.numeric(charged_kwh)) %>%
  mutate(kw_charge_point_speed = gsub(00, "", kw_charge_point_speed)) %>%
  mutate(kw_charge_point_speed = as.numeric(kw_charge_point_speed)) %>%
  mutate(hours_elapsed = sapply(end_date - start_date, function(x) {
      round(x / 3600, 2)
    })) %>%
  mutate(effective_charging_hours = sapply(charged_kwh / kw_charge_point_speed, function(x) {
      round(x, digits = 4)
    }))
}

# Returns a cleaned second social charging df (27502 rows)
cleanSecondDf <- function(df) {
  colnames(df) <- getScColnames()
  df %>%
    mutate(start_date = ymd_hms(start_date),
           end_date = ymd_hms(end_date),
           charged_kwh = as.numeric(charged_kwh)) %>%
    mutate(hours_elapsed = sapply(end_date - start_date, function(x) {
      round(x / 3600, 2)
    }))
}

changeStructures <- function(df) {
  df$smart_charging <- as.character(df$smart_charging)
  df$start_date <- as.POSIXct(df$start_date)
  df$end_date <- as.POSIXct(df$end_date)
  df$ev_provider <- as.character(df$ev_provider)
  df$car <- as.character(df$car)
  df$corporate <- as.character(df$corporate)
  df$evse_id <- as.character(df$evse_id)
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  df$address <- as.character(df$address)
  df$outlets <- as.numeric(df$outlets)
  df$charged_kwh <- as.numeric(as.character(df$charged_kwh))
  df$hours_elapsed <- as.numeric(as.character(df$hours_elapsed))
  df$kw_charge_point_speed <- as.numeric(as.character(df$kw_charge_point_speed))
  df$user_class <- as.character(df$user_class)
  df$dayOfWeek <- as.character(df$dayOfWeek)
  as.numeric(as.character(df$hour))
  df$effective_charging_hours <- as.numeric(as.character(df$effective_charging_hours))
  df$user_pred <- as.numeric(as.character(df$user_pred))
  df$station_class <- as.character(df$station_class)
  df$station_pred <- as.numeric(as.character(df$station_pred))
  return(df)
}

