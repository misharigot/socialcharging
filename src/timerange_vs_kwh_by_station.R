# Timerange vs kWh grouped by charging station
library(config)
library(dplyr)
library(ggplot2)
config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

perStation <- df %>%
  group_by(address) %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  summarise(sum_kwh = sum(charged_kwh))

perStationPerDay <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(endDate = floor_date(end_date, "day"), dayOfTheWeek = wday(end_date, label = TRUE)) %>%
  group_by(address, endDate, dayOfTheWeek) %>%
  summarise(sum_kwh = sum(charged_kwh))

perStationPerTimeframeStart <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(start_timeframe = paste(
    paste(hour(floor_date(start_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":") 
    )) %>%
  group_by(address, start_timeframe) %>%
  summarise(sum_kwh = sum(charged_kwh))

perStationPerTimeframeEnd <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(end_timeframe = paste(
    paste(hour(floor_date(end_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":") 
  )) %>%
  group_by(address, end_timeframe) %>%
  summarise(sum_kwh = sum(charged_kwh))

countTimeframeStartPerStation <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(start_timeframe = paste(
    paste(hour(floor_date(start_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":") 
  )) %>%
  group_by(address, start_timeframe) %>%
  summarise(count = n())

countTimeframeEndPerStation <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(end_timeframe = paste(
    paste(hour(floor_date(end_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":") 
  )) %>%
  group_by(address, end_timeframe) %>%
  summarise(count = n())

#p <- ggplot(a, aes(y = address, x = sum_kwh)) + geom_point(alpha = 0.3) + geom_smooth()
#p + labs(x = "address", y = "kWh charged")
