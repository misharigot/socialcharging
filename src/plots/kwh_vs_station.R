# This script contains tables/plots with different days of the week vs charged kWh, grouped by charging station.
library(config)
library(dplyr)
library(ggplot2)
config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/helpers/multiplot_helper.R")

# Table functions ---------------------------------------------------------

# Returns a table with sum kWh, grouped by station
getKwhPerStation <- function() {
  df %>%
    group_by(address) %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    summarise(sum_kwh = sum(charged_kwh))
}

# Returns a table with sum kWh, per day of the week, grouped by station
getKwhPerStationPerDay <- function(df) {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(endDate = floor_date(end_date, "day"), dayOfTheWeek = wday(end_date)) %>%
    group_by(address, endDate, dayOfTheWeek) %>%
    summarise(sum_kwh = sum(charged_kwh) / n())
}

# Plot functions ----------------------------------------------------------

# Returns a plot with total kWh, per day of the week, grouped by station
plotKwhPerStationPerDay <- function(df) {
  p <- ggplot(getKwhPerStationPerDay(df), aes(y = sum_kwh, x = dayOfTheWeek)) +
    geom_bar(stat = "identity", fill = "#66bb6a") +
    labs(x = "day of the week", y = "total kWh charged") +
    ggtitle("kWh charged per day") +
    theme_light() +
    theme(axis.text=element_text(size = 12))
  return(p)
}

# Calls -------------------------------------------------------------------

# totalKwhPerStation <- getKwhPerStation()
# plotKwhPerStationPerDay()
