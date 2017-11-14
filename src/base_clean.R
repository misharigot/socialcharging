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
