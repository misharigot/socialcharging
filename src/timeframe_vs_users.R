library(ggplot2)
library(config)
library(readr)
library(dplyr)
library(forcats)
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

# 
getKwhPerUserStartTime <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"),
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":")
    )) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(total_charged = sum(charged_kwh), count = n(), mean_kwh = mean(charged_kwh)) %>%
    group_by(start_timeframe) %>%
    summarise(tcharged = sum(total_charged), count = n(), m_kwh = mean(total_charged))
}


# 
getHoursElapsedPerUserStartTime <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(start_timeframe = paste(
      paste(hour(floor_date(start_date, "hour")), "00", sep = ":"), 
      paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":") 
    )) %>%
    group_by(user_id, start_timeframe) %>%
    summarise(total_hours_elapsed = sum(hours_elapsed), count = n(), mean_hours = mean(hours_elapsed)) %>%
    group_by(start_timeframe) %>%
    summarise(thours = sum(total_hours_elapsed), count = n(), m_hours = mean(total_hours_elapsed))
}

# Plot functions ----------------------------------------------------------

#
plotKwhPerUserStartTime <- function() {
  p <- ggplot(getKwhPerUserStartTime(), aes(y = m_kwh, x = start_timeframe)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_smooth() +
    labs(y = "Average kwh charged", x = "Start Timeframe") +
    ggtitle("Average of charged kwh at a Timeframe") +
    coord_flip() +
    theme_light()
  return(p)
}

#
plotHoursElapsedPerUserEndTime <- function() {
  p <- ggplot(getHoursElapsedPerUserStartTime(), aes(y = m_hours, x = start_timeframe)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_smooth() +
    labs(y = "Average hours elapsed", x = "Start Timeframe") +
    ggtitle("Average hours elapsed at a Timeframe") +
    coord_flip() +
    theme_light()
  return (p)
}

#Returns two plots side by side
multiplotUserTimeframes <- function() {
  return(multiplotHelper(plotKwhPerUserStartTime(), plotHoursElapsedPerUserEndTime(), cols = 2))
}

# Calls -------------------------------------------------------------------

avgKwhPerUser <- getKwhPerUser()
tabel1 <- getKwhPerUserStartTime()
tabel2 <- getHoursElapsedPerUserStartTime()
plotKwhPerUserStartTime()
plotHoursElapsedPerUserEndTime()
multiplotUserTimeframes()