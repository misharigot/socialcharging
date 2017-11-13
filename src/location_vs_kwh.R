# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
library(dplyr)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanDataframe(df)

# Data cleaning ---------------------------------------------------------
df <- df %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))

split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index - 1, nchar(target)))
}

interleave <- function(v1, v2) {
  ord1 <- 2 * (1:length(v1)) - 1
  ord2 <- 2 * (1:length(v2))
  c(v1, v2)[order(c(ord1, ord2))]
}

df$latitude <- sapply(df$latitude, function(x){
  insert <- "."[order(3)]
  index <- sort(3)
  paste(interleave(split_str_by_index(x, 3), "."), collapse = "")
})

df$longitude <- sapply(df$longitude, function(x){
  insert <- "."[order(2)]
  index <- sort(2)
  paste(interleave(split_str_by_index(x, 2), "."), collapse = "")
})

df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)

totalHours <- interval(min(df$start_date), max(df$end_date)) / 3600

# Advanced data cleaning ----------------------------------------------------------
cleanMapData <- function() {
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              outlets = first(outlets),
              total_sessions = n(),
              total_users = n_distinct(user_id),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed),
              total_effective_charging = sum(effective_charging_hours)) %>%
    mutate(efficiency_score = round((total_effective_charging / total_hours_elapsed) * 100 + 10, digits = 0),
           popularity_score = round(((total_hours_elapsed / as.numeric(totalHours))
                                     / outlets) * 100 + 10, digits = 0))

  df$total_sessions <- as.numeric(df$total_sessions)
  df$total_charged <- as.numeric(df$total_charged)

  return(df)
}

newDataFram <- cleanMapData()
