library(ggplot2)
library(config)
library(readr)
library(dplyr)
config <- config::get(file = "config.yml")
source(config$multiplotHelper)

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

# Returns a table with total kWh grouped by user
getKwhPerUser <- function(scData) {
  scData %>%
    group_by(user_id) %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    summarise(avg_charged = mean(charged_kwh))
}

# 
getKwhPerUserStartTime <- function(scData) {
  scData %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    mutate(start_timeframe = factor(start_timeframe, levels = timeframeLevels)) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(total_charged = sum(charged_kwh), count = n(), mean_kwh = mean(charged_kwh)) %>%
    group_by(start_timeframe) %>%
    summarise(tcharged = sum(total_charged), count = n(), m_kwh = mean(total_charged))
}

# 
getHoursElapsedPerUserStartTime <- function(scData) {
  scData %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    mutate(start_timeframe = factor(start_timeframe, levels = timeframeLevels)) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(total_hours_elapsed = sum(hours_elapsed), count = n(), mean_hours = mean(hours_elapsed)) %>%
    group_by(start_timeframe) %>%
    summarise(thours = sum(total_hours_elapsed), count = n(), m_hours = mean(total_hours_elapsed))
}

# Plot functions ----------------------------------------------------------

#
plotKwhPerUserStartTime <- function(scData) {
  p <- ggplot(getKwhPerUserStartTime(scData), aes(y = m_kwh, x = start_timeframe)) +
    geom_bar(stat = "identity", fill = "#66bb6a") +
    geom_smooth() +
    scale_x_discrete(limits = c(timeframeLevels)) +
    labs(y = "Average kwh charged", x = "Start Timeframe") +
    ggtitle("Average of charged kwh at a Timeframe") +
    coord_flip() +
    theme_light()
  return(p)
}

#
plotHoursElapsedPerUserEndTime <- function(scData) {
  p <- ggplot(getHoursElapsedPerUserStartTime(scData), aes(y = m_hours, x = start_timeframe)) +
    geom_bar(stat = "identity", fill = "#66bb6a") +
    geom_smooth() +
    scale_x_discrete(limits = c(timeframeLevels)) +
    labs(y = "Average hours elapsed", x = "Start Timeframe") +
    ggtitle("Average hours elapsed at a Timeframe") +
    coord_flip() +
    theme_light()
  return(p)
}

# Returns two plots side by side
multiplotUserTimeframes <- function(scData) {
  return(multiplotHelper(plotKwhPerUserStartTime(scData), plotHoursElapsedPerUserEndTime(scData), cols = 2))
}
