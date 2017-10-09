# Timerange vs kWh grouped by charging station
library(config)
library(dplyr)
library(ggplot2)
config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# sum kWh group by station
perStation <- df %>%
  group_by(address) %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  summarise(sum_kwh = sum(charged_kwh))

# sum kWh per day group by station
perStationPerDay <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(endDate = floor_date(end_date, "day"), dayOfTheWeek = wday(end_date, label = TRUE)) %>%
  group_by(address, endDate, dayOfTheWeek) %>%
  summarise(sum_kwh = sum(charged_kwh))

# sum kWh per start_date timeframe per station
perStationPerTimeframeStart <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(start_timeframe = paste(
    paste(hour(floor_date(start_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":") 
    )) %>%
  group_by(address, start_timeframe) %>%
  summarise(sum_kwh = sum(charged_kwh), count = n())

# sum kWh per end_date timeframe per station
perStationPerTimeframeEnd <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(end_timeframe = paste(
    paste(hour(floor_date(end_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":") 
  )) %>%
  group_by(address, end_timeframe) %>%
  summarise(sum_kwh = sum(charged_kwh), count = n())

# sum kWh per start_date timeframe
countPerTimeframeStart <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(start_timeframe = paste(
    paste(hour(floor_date(start_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(start_date, "hour")) + 1, "00", sep = ":") 
  )) %>%
  group_by(start_timeframe) %>%
  summarise(sum_kwh = sum(charged_kwh), count = n())

# sum kWh per end_date timeframe
countPerTimeframeEnd <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(end_timeframe = paste(
    paste(hour(floor_date(end_date, "hour")), "00", sep = ":"), 
    paste(hour(floor_date(end_date, "hour")) + 1, "00", sep = ":") 
  )) %>%
  group_by(end_timeframe) %>%
  summarise(sum_kwh = sum(charged_kwh), count = n())

# perStationPerDay
p1 <- ggplot(perStationPerDay, aes(y = sum_kwh, x = dayOfTheWeek)) +
  geom_boxplot(alpha = 0.5) + 
  geom_smooth() + 
  labs(x = "day of the week", y = "total kWh charged") + 
  ggtitle("kWh charged per day per station")
p1

p2 <- ggplot(perStationPerTimeframeEnd, aes(y = count, x = end_timeframe)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "1 hour timeframe", y = "number of ended sessions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("kWh charged per session end timeframe per station")
p2

p3 <- ggplot(countPerTimeframeStart, aes(y = count, x = start_timeframe)) +
  geom_point() +
  geom_smooth() +
  labs(x = "1 hour timeframe", y = "number of started sessions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Num of sessions starting within timeframe")
p3

p4 <- ggplot(countPerTimeframeEnd, aes(y = count, x = end_timeframe)) +
  geom_point() +
  labs(x = "1 hour timeframe", y = "number of ended sessions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Num of sessions ending within timeframe")
p4
