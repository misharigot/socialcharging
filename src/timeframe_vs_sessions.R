# This script contains plots with different time frames vs start/end times of charging sessions.
library(config)
library(dplyr)
library(ggplot2)
config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns a table with amount of sessions started at timeframe of 1 hour
getStartTimeframe <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    group_by(start_timeframe) %>%
    summarise(count = n())
}

# Returns a table with amount of sessions ended at timeframe of 1 hour
getEndTimeframe <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(end_timeframe = paste(
      paste(hour(floor_date(end_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    group_by(end_timeframe) %>%
    summarise(count = n())
}

# Plot functions ----------------------------------------------------------

# Returns a plot with amount of sessions started at timeframe of 1 hour
plotStartTimeframe <- function() {
  p <- ggplot(getStartTimeframe(), aes(y = count, x = start_timeframe)) +
    geom_point() +
    geom_smooth() +
    labs(x = "1 hour timeframe", y = "number of started sessions") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Num of sessions starting within timeframe")
  return (p)
}

# Returns a plot with amount of sessions ended at timeframe of 1 hour
plotEndTimeframe <- function() {
  p <- ggplot(getEndTimeframe(), aes(y = count, x = end_timeframe)) +
    geom_point() +
    labs(x = "1 hour timeframe", y = "number of ended sessions") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Num of sessions ending within timeframe")
  return (p)
}

# Returns two plots side by side
multiplotTimeframes <- function() {
  return(multiplotHelper(plotStartTimeframe(), plotEndTimeframe(), cols = 2))
}

# Calls -------------------------------------------------------------------

plotStartTimeframe()
plotEndTimeframe()
