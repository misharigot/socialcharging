# This script does some general cleaning on the social charging csv.
library(readr)
library(dplyr)
library(lubridate)

# Returns the social charging dataframe given, with general cleaning executed on the data set.
cleanDataframe <- function(df) {
  df %>%
  rename(charged_kwh = `charged_kwh,`, kw_charge_point_speed = `kw  charge point speed)`) %>%
  mutate(start_date = dmy_hm(start_date),
         end_date = dmy_hm(end_date),
         charged_kwh = as.numeric(charged_kwh)) %>%
  mutate(kw_charge_point_speed = gsub(00, "", kw_charge_point_speed)) %>%
  mutate(kw_charge_point_speed = as.numeric(kw_charge_point_speed)) %>%
  mutate(hours_elapsed = sapply(end_date - start_date, function(x) {
      round(x / 3600, 2)
    })) %>%
  mutate(effective_charging_hours = sapply(charged_kwh / kw_charge_point_speed, function(x) {
      round(x, digits = 2)
    }))
}

# Returns a cleaned second social charging df (27502 rows)
cleanSecondDf <- function(df) {
  df %>%
    rename(charged_kwh = `charged_kwh,`, kw_charge_point_speed = `kw  charge point speed)`) %>%
    mutate(start_date = ymd_hms(start_date),
           end_date = ymd_hms(end_date),
           charged_kwh = as.numeric(charged_kwh)) %>%
    mutate(hours_elapsed = sapply(end_date - start_date, function(x) {
      round(x / 3600, 2)
    }))
}
