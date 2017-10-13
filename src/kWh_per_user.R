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

# Per user total amount charged kwh
getKWhPerUserPerHour <- function() {
  df %>%
    filter(!is.na(charged_kwh), !is.na(end_date)) %>%
    mutate(end_timeframe = paste(
      paste(hour(floor_date(end_date, "hour")), "00", sep = ":"), 
      paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":") 
    )) %>%
    group_by(end_timeframe) %>%
    summarise(total_charged = sum(charged_kwh))
}

# Plot functions ----------------------------------------------------------

#Plotje
kwhPerUser <- function() {
  p <- ggplot(getKWhPerUserPerHour(), aes(y = total_charged, x = end_timeframe)) + 
    geom_point(alpha = 0.5) + 
    geom_smooth() +
    labs(y = "Total kWh charged", x = "Timeframe") +
    ggtitle("kWh charged per hour per user")
  return (p)
}

# Calls -------------------------------------------------------------------

total <- getKwhPerUser()
kwhPerUser()