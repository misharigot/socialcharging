# This script contains tables/plots with different days of the week vs charged kWh, grouped by charging station.
library(config)
library(dplyr)
library(ggplot2)
config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns a table with sum kWh, grouped by station
getKwhPerStation <- function() {
  df %>%
    group_by(address) %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    summarise(sum_kwh = sum(charged_kwh))
}

# Returns a table with sum kWh, per day of the week, grouped by station
getKwhPerStationPerDay <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(endDate = floor_date(end_date, "day"), dayOfTheWeek = wday(end_date, label = TRUE)) %>%
    group_by(address, endDate, dayOfTheWeek) %>%
    summarise(sum_kwh = sum(charged_kwh))
}

# Plot functions ----------------------------------------------------------

# Returns a plot with total kWh, per day of the week, grouped by station
plotKwhPerStationPerDay <- function() {
  p <- ggplot(getKwhPerStationPerDay(), aes(y = sum_kwh, x = dayOfTheWeek)) +
    geom_boxplot(alpha = 0.5) +
    geom_smooth() +
    labs(x = "day of the week", y = "total kWh charged") +
    ggtitle("kWh charged per day per station")
  return (p)
}

# Calls -------------------------------------------------------------------

totalKwhPerStation <- getKwhPerStation()
plotKwhPerStationPerDay()
