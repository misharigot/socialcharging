library(ggplot2)
library(config)
library(readr)
library(dplyr)
config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns a table with total kWh grouped by user
getKwhPerUser <- function() {
  df %>%
    group_by(user_id) %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    summarise(total_charged = sum(charged_kwh))
}

# Per user total amount charged kwh at the start of a hour timeframe
getKwhPerUserStarttime <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(total_charged = sum(charged_kwh))
}

# Per user total amount charged kwh at the end of a hour timeframe
getKWhPerUserEndtime <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(end_timeframe = paste(
      paste(hour(floor_date(end_date, "hour")), "00", sep = ":"), 
      paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":") 
    )) %>%
    group_by(user_id, end_timeframe) %>%
    summarise(total_charged = sum(charged_kwh))
}

# Plot functions ----------------------------------------------------------

#Returns a plot with the total amount charged started per hour and group by user
plotKwhPerUserStartTime <- function() {
  p <- ggplot(getKwhPerUserStarttime(), aes(y = total_charged, x = start_timeframe)) +
    geom_boxplot(alpha = 0.5) +
    geom_smooth() +
    labs(y = "Total kwh charged", x = "Start Timeframe") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("kwh charged per hour per user Start Timeframe")
  return(p)
}

#Returns a plot with the total amount charged ended per hour and group by user 
plotKwhPerUserEndTime <- function() {
  p <- ggplot(getKWhPerUserEndtime(), aes(y = total_charged, x = end_timeframe)) + 
    geom_boxplot(alpha = 0.5) + 
    geom_smooth() +
    labs(y = "Total kWh charged", x = "End Timeframe") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("kWh charged per hour per user End Timeframe")
  return (p)
}

# Calls -------------------------------------------------------------------

totalKwhPerUser <- getKwhPerUser()
plotKwhPerUserStartTime()
plotKwhPerUserEndTime()