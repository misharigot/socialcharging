library(ggplot2)
library(config)
library(readr)
library(dplyr)
config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns a table with total kWh grouped by user
getKwhPerUser <- function() {
  df %>%
    group_by(user_id) %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    summarise(avg_charged = mean(charged_kwh))
}

# Per user total amount charged kwh at the start of a hour timeframe
getKwhPerUserStartTime <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(avg_charged = mean(charged_kwh))
}

# Per user total amount charged kwh at the end of a hour timeframe
getHoursElapsedPerUserStartTime <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"), 
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":") 
    )) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(avg_hours_elapsed = mean(hours_elapsed))
}

# Plot functions ----------------------------------------------------------

#Returns a plot with the total amount charged started per hour and group by user
plotKwhPerUserStartTime <- function() {
  p <- ggplot(getKwhPerUserStartTime(), aes(y = avg_charged, x = start_timeframe)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_smooth() +
    labs(y = "Average kwh charged", x = "Start Timeframe") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Average kwh charged per hour per user Start Timeframe")
  return(p)
}

#Returns a plot with the total amount charged ended per hour and group by user 
plotHoursElapsedPerUserEndTime <- function() {
  p <- ggplot(getHoursElapsedPerUserStartTime(), aes(y = avg_hours_elapsed, x = start_timeframe)) + 
    geom_bar(stat = "identity", alpha = 0.5) + 
    geom_smooth() +
    labs(y = "Average hours elapsed", x = "Start Timeframe") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Average hours elapsed per user at a Start Timeframe")
  return (p)
}

Returns two plots side by side
multiplotUserTimeframes <- function() {
  return(multiplotHelper(plotKwhPerUserStartTime(), plotHoursElapsedPerUserEndTime(), cols = 2))
}

# Calls -------------------------------------------------------------------

totalKwhPerUser <- getKwhPerUser()
plotKwhPerUserStartTime()
plotHoursElapsedPerUserEndTime()
multiplotUserTimeframes()