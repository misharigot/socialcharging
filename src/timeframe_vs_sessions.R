# This script contains plots with different time frames vs start/end times of charging sessions.
library(config)
library(dplyr)
library(ggplot2)
config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)
timeframeLevels <- c("6:00 7:00",
                     "7:00 8:00",
                     "8:00 9:00",
                     "9:00 10:00",
                     "10:00 11:00",
                     "11:00 12:00",
                     "12:00 13:00",
                     "13:00 14:00",
                     "14:00 15:00",
                     "15:00 16:00",
                     "16:00 17:00",
                     "17:00 18:00",
                     "18:00 19:00",
                     "19:00 20:00",
                     "20:00 21:00",
                     "21:00 22:00",
                     "22:00 23:00",
                     "23:00 24:00",
                     "0:00 1:00",
                     "1:00 2:00",
                     "2:00 3:00",
                     "3:00 4:00",
                     "4:00 5:00",
                     "5:00 6:00"
                     )
# Table functions ---------------------------------------------------------

# Returns a table with amount of sessions started at timeframe of 1 hour
getStartTimeframe <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    mutate(start_timeframe = factor(start_timeframe, levels = timeframeLevels)) %>%
    group_by(start_timeframe) %>%
    arrange(start_timeframe) %>%
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
    mutate(start_timeframe = factor(end_timeframe, levels = timeframeLevels)) %>%
    group_by(end_timeframe) %>%
    arrange(end_timeframe) %>%
    summarise(count = n())
}

# Plot functions ----------------------------------------------------------

# Returns a plot with amount of sessions started at timeframe of 1 hour
plotStartTimeframe <- function() {
  p <- ggplot(getStartTimeframe(), aes(y = count, x = start_timeframe)) +
    geom_bar(stat = "identity", fill = "#66bb6a") +
    coord_flip() +
    labs(title = "Charging sessions",
         subtitle = "Starting in timeframe",
         x = "1 hour timeframe",
         y = "started sessions") +
    scale_x_discrete(limits = c(timeframeLevels)) +
    theme_light()
  return(p)
}

# Returns a plot with amount of sessions ended at timeframe of 1 hour
plotEndTimeframe <- function() {
  p <- ggplot(getEndTimeframe(), aes(y = count, x = end_timeframe)) +
    geom_bar(stat = "identity", fill = "#66bb6a") +
    coord_flip() +
    labs(title = "Charging sessions",
         subtitle = "Ending in timeframe",
         x = "1 hour timeframe",
         y = "ended sessions") +
    scale_x_discrete(limits = c(timeframeLevels)) +
    theme_light()
  return(p)
}

# Returns two plots side by side
multiplotTimeframes <- function() {
  return(multiplotHelper(plotStartTimeframe(), plotEndTimeframe(), cols = 2))
}

# Calls -------------------------------------------------------------------
plotStartTimeframe()
plotEndTimeframe()
